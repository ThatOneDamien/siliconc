#include "semantics.h"
#include "utils/da.h"

static inline void analyze_main();
static void analyze_function_body(ObjFunc* function);
static bool analyze_attributes(Object* obj);


SemaContext* g_sema = NULL;

void analyze_module(ObjModule* module)
{
    for(uint32_t i = 0; i < module->submodules.size; ++i)
        analyze_module(module->submodules.data[i]);

    for(uint32_t i = 0; i < module->imports.size; ++i)
        resolve_import(module, module->imports.data[i]);
    SemaContext* prev = g_sema;
    SemaContext sema = {0};
    g_sema = &sema;
    g_sema->module = module;

    // FIXME: Add check that we are compiling an executable. Libraries must NOT
    //        have a main function.
    if(module == &g_compiler.top_module && g_compiler.emit_link)
        analyze_main();

    for(uint32_t i = 0; i < module->types.size; ++i)
        analyze_type_obj(module->types.data[i], NULL, RES_NORMAL, LOC_NULL, NULL);

    for(uint32_t i = 0; i < module->vars.size; ++i)
        analyze_global_var(module->vars.data[i]);

    for(uint32_t i = 0; i < module->funcs.size; ++i)
        analyze_function_body(module->funcs.data[i]);

    ASTStmt* assert_ = module->ct_asserts;
    while(assert_ != NULL)
    {
        analyze_ct_assert(assert_);
        assert_ = assert_->next;
    }

    g_sema = prev;
#ifdef SI_DEBUG
    if(g_compiler.debug_output & DEBUG_SEMA)
        print_module(module, false);
#endif
}

bool analyze_function(ObjFunc* func)
{
    if(func->header.status == STATUS_RESOLVED) return func->header.kind != OBJ_INVALID;
    if(func->header.status == STATUS_RESOLVING)
    {
        set_cyclic_def(&func->header);
        return false;
    }

    bool success = true;
    func->header.status = STATUS_RESOLVING;

    bool prev = g_sema->in_global_init;
    g_sema->in_global_init = true;

    if(!analyze_attributes(&func->header)) success = false;

    if(func->is_extern)
    {
        func->header.link_name = func->header.symbol;
        if(func->body != NULL)
            sic_error_at(func->header.loc, "Function marked 'extern' cannot have a body.");
    }
    else 
    {
        set_object_link_name(&func->header);
        if(func->body == NULL)
            sic_error_at(func->header.loc, "Function must either have a body, or be marked 'extern'.");
    }
    

    if(!resolve_type(&func->signature.ret_type.type, RES_ALLOW_VOID, 
                     func->signature.ret_type.loc, "Function cannot have return type"))
    {
        check_cyclic_def(&func->header, func->header.loc);
        success = false;
    }
    else
    {
        if(func->signature.ret_type.type->visibility < func->header.visibility)
        {
            sic_error_at(func->signature.ret_type.loc, "Return type is marked private, but parent function is marked public.");
            sic_diagnostic_at(DIAG_NOTE, func->header.loc, "From function definition here.");
            success = false;
        }
    }

    const ObjVarDA params = func->signature.params;
    for(uint32_t i = 0; i < params.size; ++i)
    {
        ObjVar* param = params.data[i];
        if(!resolve_type(&param->type_loc.type, RES_NORMAL, param->type_loc.loc, "Parameter cannot be of type"))
        {
            check_cyclic_def(&param->header, param->header.loc);
            success = false;
            continue;
        }

        if(param->type_loc.type->visibility < func->header.visibility)
        {
            sic_error_at(param->type_loc.loc, "Parameter's type has less visibility than parent function.");
            success = false;
            continue;
        }
    }

    g_sema->in_global_init = prev;
    func->header.status = STATUS_RESOLVED;
    func->func_type->status = STATUS_RESOLVED;

    if(!success)
    {
        func->header.kind = OBJ_INVALID;
        func->func_type->kind = TYPE_INVALID;
    }

    return success;
}

bool resolve_import(ObjModule* module, ObjImport* import)
{
    if(import->header.status == STATUS_RESOLVED) return import->header.kind != OBJ_INVALID;
    if(import->header.status == STATUS_RESOLVING)
    {
        set_cyclic_def(&import->header);
        return false;
    }

    import->header.status = STATUS_RESOLVING;
    analyze_attributes(&import->header);

    // TODO: Check external modules as well. They arent added yet, but when they are
    // add them here.
    const ModulePath path = import->unresolved;
    ObjModule* mod = &g_compiler.top_module;
        
    for(uint32_t i = 0; i < path.size - 1; ++i)
    {
        for(uint32_t j = 0; j < mod->imports.size; ++j)
        {
            ObjImport* other_import = mod->imports.data[j];
            if((other_import->header.symbol == NULL || other_import->header.symbol == path.data[i].sym) &&
               !resolve_import(mod, other_import))
            {
                check_cyclic_def(&import->header, import->header.loc);
                return false;
            }
        }

        if((mod = find_module(mod, path.data[i], false)) == NULL)
            return false;
    }

    if(import->header.symbol == NULL)
    {
        for(uint32_t i = 0; i < mod->imports.size; ++i)
        {
            ObjImport* other_import = mod->imports.data[i];
            if(!resolve_import(mod, other_import))
            {
                check_cyclic_def(&import->header, import->header.loc);
                return false;
            }
        }
        import->header.status = STATUS_RESOLVED;
        for(uint32_t i = 0; i < mod->symbol_ns.bucket_cnt; ++i)
        {
            HashEntry* entry = mod->symbol_ns.buckets + i;
            if(entry->key != NULL)
            {
                Object* old = hashmap_get(&module->symbol_ns, entry->key);
                if(old != NULL)
                {
                    sic_error_redef(&import->header, old);
                    import->header.kind = OBJ_INVALID;
                    return false;
                }

                ObjImport* new = CALLOC_STRUCT(ObjImport);
                new->header.symbol = entry->key;
                new->header.loc = import->header.loc;
                new->header.kind = OBJ_IMPORT;
                new->header.visibility = import->header.visibility;
                new->header.status = STATUS_RESOLVED;
                new->resolved = entry->value->kind == OBJ_IMPORT ? obj_as_import(entry->value)->resolved : entry->value;
                hashmap_put(&module->symbol_ns, entry->key, &new->header);
            }
        }

        for(uint32_t i = 0; i < mod->module_ns.bucket_cnt; ++i)
        {
            HashEntry* entry = mod->module_ns.buckets + i;
            if(entry->key != NULL && entry->value->visibility == VIS_PUBLIC)
            {
                Object* old = hashmap_get(&module->module_ns, entry->key);
                if(old != NULL)
                {
                    sic_diagnostic_at(DIAG_ERROR, import->header.loc, "Module with name \'%s\' already exists.", entry->key);
                    sic_diagnostic_at(DIAG_NOTE, old->loc, "Previous definition here.");
                    import->header.kind = OBJ_INVALID;
                    return false;
                }

                ObjImport* new = CALLOC_STRUCT(ObjImport);
                new->header.symbol = entry->key;
                new->header.loc = import->header.loc;
                new->header.kind = OBJ_IMPORT;
                new->header.visibility = import->header.visibility;
                new->header.status = STATUS_RESOLVED;
                new->resolved = entry->value->kind == OBJ_IMPORT ? obj_as_import(entry->value)->resolved : entry->value;
                hashmap_put(&module->module_ns, entry->key, &new->header);
            }
        }
        return true;
    }

    const Symbol actual = path.data[path.size - 1].sym;

    for(uint32_t i = 0; i < mod->imports.size; ++i)
    {
        ObjImport* other_import = mod->imports.data[i];
        if((other_import->header.symbol == NULL || other_import->header.symbol == actual) &&
           !resolve_import(mod, other_import))
        {
            check_cyclic_def(&import->header, import->header.loc);
            return false;
        }
    }

    bool used = false;
    import->header.status = STATUS_RESOLVED;

    Object* o = hashmap_get(&mod->module_ns, actual);
    if(o != NULL)
    {
        Object* old = hashmap_get(&module->module_ns, import->header.symbol);
        if(old != NULL)
        {
            sic_error_redef(&import->header, old);
            import->header.kind = OBJ_INVALID;
            return false;
        }
        import->resolved = o->kind == OBJ_IMPORT ? obj_as_import(o)->resolved : o;
        used = true;
        hashmap_put(&module->module_ns, import->header.symbol, &import->header);
    }

    o = hashmap_get(&mod->symbol_ns, actual);
    if(o != NULL)
    {
        Object* prev = hashmap_get(&module->symbol_ns, import->header.symbol);
        if(prev != NULL)
        {
            sic_error_redef(&import->header, prev);
            if(!used)
                import->header.kind = OBJ_INVALID;
            return false;
        }
        if(used)
        {
            ObjImport* prev_import = import;
            import = CALLOC_STRUCT(ObjImport);
            *import = *prev_import;
        }
        import->resolved = o->kind == OBJ_IMPORT ? obj_as_import(o)->resolved : o;
        hashmap_put(&module->symbol_ns, import->header.symbol, &import->header);
    }

    return true;
}

bool analyze_global_var(ObjVar* var)
{
    if(var->header.status == STATUS_RESOLVED) return var->header.kind != OBJ_INVALID;
    if(var->header.status == STATUS_RESOLVING)
    {
        set_cyclic_def(&var->header);
        return false;
    }

    bool prev = g_sema->in_global_init;
    g_sema->in_global_init = true;
    var->header.status = STATUS_RESOLVING;
    DBG_ASSERT(!var->uninitialized);

    analyze_attributes(&var->header);
    if(var->is_extern)
    {
        var->header.link_name = var->header.symbol;
        if(var->initial_val != NULL)
            sic_error_at(var->header.loc, "Global variable marked 'extern' cannot have an initial value.");
    }
    else 
    {
        set_object_link_name(&var->header);
    }

    if(!analyze_declaration(var))
    {
        g_sema->in_global_init = prev;
        check_cyclic_def(&var->header, var->header.loc);
        return false;
    }
    g_sema->in_global_init = prev;

    if(var->initial_val != NULL && !var->initial_val->const_eval)
    {
        sic_error_at(var->initial_val->loc, "Global variable must be initialized with a compile-time evaluable value.");
        var->header.kind = OBJ_INVALID;
        var->header.status = STATUS_RESOLVED;
        return false;
    }

    if(var->type_loc.type->visibility < var->header.visibility)
        // TODO: Make this error print the actual visibility of both.
        sic_error_at(var->header.loc, "Global variable's type has less visibility than the object itself.");

    var->header.status = STATUS_RESOLVED;
    return true;
}

Attr* get_builtin_attribute(Object* obj, AttrKind kind)
{
    DBG_ASSERT(obj != NULL);
    DBG_ASSERT(kind < ATTR_CUSTOM);
    for(uint32_t i = 0; i < obj->attrs.size; ++i)
        if(obj->attrs.data[i].kind == kind)
            return obj->attrs.data + i;
    return NULL;
}

static inline void analyze_main()
{
    Object* main = hashmap_get(&g_compiler.top_module.symbol_ns, g_sym_main);
    if(main == NULL)
        sic_fatal_error("Root module is missing main function. Declare it as 'fn main()'.");

    if(main->kind != OBJ_FUNC)
    {
        sic_error_at(main->loc, "Symbol 'main' in the root module is reserved for the entry function.");
        return;
    }

    ObjFunc* func = obj_as_func(main);
    const ObjVarDA params = func->signature.params;
    TypeKind rt_kind = func->signature.ret_type.type->kind;
    if(rt_kind != TYPE_INT && rt_kind != TYPE_VOID)
        goto BAD_SIG;

    if(params.size >= 1 && params.data[0]->type_loc.type->kind != TYPE_INT)
        goto BAD_SIG;

    Type* second;
    if(params.size >= 2)
    {
        second = params.data[1]->type_loc.type;
        if(second->kind != TYPE_POINTER)
            goto BAD_SIG;
        second = second->pointer_base;
        if(second->kind != TYPE_POINTER)
            goto BAD_SIG;
        second = second->pointer_base;
        if(second->kind != TYPE_CHAR)
            goto BAD_SIG;
    }

    main->link_name = main->symbol;
    return;

BAD_SIG:
    sic_error_at(main->loc, "The signature of the main function is invalid. "
                            "The return type should be 'int' or 'void', with "
                            "optional parameters 'int, char**'.");
    return;
}

static void analyze_function_body(ObjFunc* func)
{
    if(!analyze_function(func)) return;

    g_sema->in_global_init = false;
    const ObjVarDA params = func->signature.params;

    uint32_t scope = push_scope();
    for(uint32_t i = 0; i < params.size; ++i)
    {
        ObjVar* param = params.data[i];
        if(param->header.symbol != NULL)
        {
            push_obj(&params.data[i]->header);
            da_append(&g_sema->locals, param);
        }
    }


    ASTStmt* body = func->body;
    g_sema->cur_func = func;
    if(body != NULL && !analyze_stmt_block(body->stmt.block.body) && 
       func->signature.ret_type.type->kind != TYPE_VOID)
    {
        sic_error_at(func->header.loc, "Function does not return from all control paths.");
    }

    if(!func->is_extern)
    {
        for(uint32_t i = 0; i < g_sema->locals.size; ++i)
        {
            ObjVar* var = g_sema->locals.data[i];
            if(!var->read)
            {
                // TODO: Allow variables to be prefixed with _ in order to mean they are unused.
                if(var->written)
                    sic_error_at(var->header.loc, "Variable is written to, but its value is never read.");
                else
                    sic_error_at(var->header.loc, "Variable is unused.");
            }
            else if(!var->is_const_binding && !var->written)
            {
                sic_diagnostic_at(DIAG_WARNING, var->header.loc, "Variable is never written to, consider changing it to a const declaration.");
            }
        }
    }
    g_sema->cur_func = NULL;
    g_sema->locals.size = 0; // Clear locals
    pop_scope(scope);
}

static bool check_attribute_args(Attr* attr, uint32_t expected)
{
    if(attr->args.size < expected)
    {
        sic_error_at(attr->loc, "Too few arguments provided in attribute \'%s\'. Expected %u, got %u.",
                     attr->symbol, expected, attr->args.size);
        return false;
    }
    if(attr->args.size > expected)
    {
        sic_error_at(attr->loc, "Too many arguments provided in attribute \'%s\'. Expected %u, got %u.",
                     attr->symbol, expected, attr->args.size);
        return false;
    }
    return true;
}

static bool analyze_attributes(Object* obj)
{
    DBG_ASSERT(obj->status != STATUS_RESOLVED);
    AttrDA attrs = obj->attrs;
    ObjKind kind = obj->kind;
    bool valid = true;
    for(uint32_t i = 0; i < attrs.size; ++i)
    {
        Attr* attr = attrs.data + i;
        switch(attr->kind)
        {
        case ATTR_ABI:
            SIC_TODO();
        case ATTR_INLINE:
            if(!check_attribute_args(attr, 0)) break;
            if(kind != OBJ_FUNC)
            {
                sic_error_at(attr->loc, "Attribute %s can only be applied to struct definitions.",
                             attr->symbol);
                break;
            }
            continue;
        case ATTR_LINK_NAME: {
            if(!check_attribute_args(attr, 1)) break;
            ASTExpr* expr = attr->args.data[0];
            if(!analyze_expr(expr)) break;
            SIC_TODO();
            obj->link_name = NULL;
            continue;
        }
        case ATTR_NODISCARD:
            if(!check_attribute_args(attr, 0)) break;
            if(kind != OBJ_FUNC)
            {
                sic_error_at(attr->loc, "Attribute %s can only be applied to struct definitions.", attr->symbol);
                break;
            }
            continue;
        case ATTR_NOINLINE:
            if(!check_attribute_args(attr, 0)) break;
            if(kind != OBJ_FUNC)
            {
                sic_error_at(attr->loc, "Attribute %s can only be applied to struct definitions.", attr->symbol);
                break;
            }
            continue;
        case ATTR_NORETURN:
            if(!check_attribute_args(attr, 0)) break;
            if(kind != OBJ_FUNC)
            {
                sic_error_at(attr->loc, "Attribute %s can only be applied to struct definitions.", attr->symbol);
                break;
            }
            continue;
        case ATTR_PACKED:
            if(!check_attribute_args(attr, 0)) break;
            if(kind != OBJ_STRUCT)
            {
                sic_error_at(attr->loc, "Attribute %s can only be applied to struct definitions.", attr->symbol);
                break;
            }
            continue;
        case ATTR_CUSTOM:
            SIC_TODO();
        case ATTR_INVALID:
            SIC_UNREACHABLE();
        }

        attr->kind = ATTR_INVALID;
        valid = false;
        continue;
    }

    return valid;
}


bool analyze_type_obj(Object* type_obj, Type** o_type, 
                      ResolutionFlags flags, SourceLoc err_loc, const char* err_str)
{
    switch(type_obj->kind)
    {
    case OBJ_BITFIELD:
        SIC_TODO();
    case OBJ_ENUM:
        return analyze_enum(obj_as_enum(type_obj), o_type);
    case OBJ_STRUCT:
        return analyze_struct(obj_as_struct(type_obj), o_type);
    case OBJ_TYPEDEF:
        return analyze_typedef(obj_as_typedef(type_obj), o_type, flags, err_loc, err_str);
    case OBJ_UNION:
        return analyze_union(obj_as_struct(type_obj), o_type);
    case OBJ_INVALID:
        return false;
    case OBJ_ENUM_VALUE:
    case OBJ_FUNC:
    case OBJ_IMPORT:
    case OBJ_MODULE:
    case OBJ_VAR:
        break;
    }
    SIC_UNREACHABLE();
}

bool analyze_enum(ObjEnum* enum_, Type** o_type)
{
    Type* const type = enum_->type_ref;
    if(o_type != NULL)
        *o_type = type_apply_qualifiers(type, (*o_type)->qualifiers);

    if(enum_->header.status == STATUS_RESOLVED) return enum_->header.kind != OBJ_INVALID;

    analyze_attributes(&enum_->header);

    type->status = STATUS_RESOLVED;
    enum_->header.status = STATUS_RESOLVED;

    if(enum_->underlying.type == NULL)
        enum_->underlying.type = g_type_int;
    else if(!resolve_type(&enum_->underlying.type, RES_NORMAL, enum_->underlying.loc, "An enum's underlying type cannot be of type"))
        goto ERR;
    else 
    {
        enum_->underlying.type = enum_->underlying.type->canonical;
        if(!type_is_integer(enum_->underlying.type))
        {
            sic_error_at(enum_->underlying.loc, "Underlying type for enums must be an integer type.");
            goto ERR;
        }
    }

    type->canonical = type->kind == TYPE_ENUM ? enum_->underlying.type : type;

    uint32_t scope = push_scope();
    Int128 next_value = (Int128){ 0, 0 };
    for(uint32_t i = 0; i < enum_->values.size; ++i)
    {
        ObjEnumValue* value = enum_->values.data[i];
        value->enum_type = type;
        if(value->raw_value == NULL)
        {
            value->const_value = next_value;
            next_value = i128_add64(next_value, 1);
        }
        else if(!analyze_expr(value->raw_value))
            goto ERR;
        else if(value->raw_value->kind != EXPR_CONSTANT)
        {
            sic_error_at(value->header.loc, "Enum value must be assigned a constant integer expression.");
            goto ERR;
        }
        else if(!implicit_cast(&value->raw_value, enum_->underlying.type))
            goto ERR;
        else
        {
            value->const_value = value->raw_value->expr.constant.i;
            next_value = i128_add64(value->const_value, 1);
        }
        push_obj(&value->header);
    }
    pop_scope(scope);


    return true;
ERR:
    enum_->header.kind = OBJ_INVALID;
    type->kind = TYPE_INVALID;
    return false;
}

bool analyze_struct(ObjStruct* struct_, Type** o_type)
{
    if(o_type != NULL)
        *o_type = type_apply_qualifiers(struct_->type_ref, (*o_type)->qualifiers);

    if(struct_->header.status == STATUS_RESOLVED || g_sema->in_ptr || g_sema->in_typedef) 
        return struct_->header.kind != OBJ_INVALID;
    if(struct_->header.status == STATUS_RESOLVING)
    {
        set_cyclic_def(&struct_->header);
        return false;
    }

    struct_->header.status = STATUS_RESOLVING;
    analyze_attributes(&struct_->header);
    bool packed = get_builtin_attribute(&struct_->header, ATTR_PACKED) != NULL;
    if(packed)
        struct_->align = 1;
    for(uint32_t i = 0; i < struct_->members.size; ++i)
    {
        ObjVar* member = struct_->members.data[i];
        if(!resolve_type(&member->type_loc.type, RES_ALLOW_AUTO_ARRAY, member->type_loc.loc, "Struct member cannot be of type"))
        {
            check_cyclic_def(&struct_->header, struct_->header.loc);
            return false;
        }
        if(member->type_loc.type->visibility < struct_->header.visibility)
        {
            // TODO: Make this error print the actual visibility of both.
            sic_error_at(member->header.loc, "Member has type with less visibility than parent.");
            struct_->header.status = STATUS_RESOLVED;
            struct_->header.kind = OBJ_INVALID;
            return false;
        }
        if(packed)
        {
            struct_->size += type_size(member->type_loc.type);
        }
        else
        {
            uint32_t align = type_alignment(member->type_loc.type);
            DBG_ASSERT(is_pow_of_2(align));
            struct_->size = ALIGN_UP(struct_->size, align) + type_size(member->type_loc.type);
            struct_->align = MAX(struct_->align, align);
        }
    }

    struct_->size = ALIGN_UP(struct_->size, struct_->align);
    struct_->header.status = STATUS_RESOLVED;
    struct_->type_ref->status = STATUS_RESOLVED;

    return true;
}

bool analyze_typedef(ObjTypedef* typedef_, Type** o_type, ResolutionFlags flags,
                            SourceLoc err_loc, const char* err_str)
{
    DBG_ASSERT(typedef_->header.kind == OBJ_TYPEDEF);
    switch(typedef_->header.status)
    {
    case STATUS_RESOLVED:
        if(o_type != NULL)
        {
            *o_type = type_apply_qualifiers(typedef_->alias.type, (*o_type)->qualifiers);
            if(!resolve_type(o_type, flags, err_loc, err_str)) {
                check_cyclic_def(&typedef_->header, typedef_->header.loc);
                return false;
            }
            *o_type = type_apply_qualifiers(typedef_->type_ref, (*o_type)->qualifiers);
        }
        return true;
    case STATUS_RESOLVING:
        if(g_sema->in_typedef)
        {
            set_cyclic_def(&typedef_->header);
            return false;
        }
        FALLTHROUGH;
    case STATUS_UNRESOLVED: {
        typedef_->header.status = STATUS_RESOLVING;
        analyze_attributes(&typedef_->header);
        bool prev = g_sema->in_typedef;
        g_sema->in_typedef = true;
        bool success = resolve_type(&typedef_->alias.type, RES_ALLOW_VOID, typedef_->alias.loc, "Typedef cannot be assigned to type");
        g_sema->in_typedef = prev;
        if(!success)
        {
            check_cyclic_def(&typedef_->header, typedef_->header.loc);
            return false;
        }
        typedef_->header.status = STATUS_RESOLVED;
        typedef_->type_ref->status = STATUS_RESOLVED;
        typedef_->type_ref->canonical = typedef_->type_ref->kind == TYPE_ALIAS ?
                                            typedef_->alias.type->canonical :
                                            typedef_->type_ref;
        typedef_->alias.type = type_reduce(typedef_->alias.type);

        if(o_type != NULL)
        {
            *o_type = type_apply_qualifiers(typedef_->alias.type, (*o_type)->qualifiers);
            if(resolve_type(o_type, flags, err_loc, err_str)) 
            {
                *o_type = type_apply_qualifiers(typedef_->type_ref, (*o_type)->qualifiers);
                return true;
            }
            return false;
        }
        return true;
    }
    default:
        SIC_UNREACHABLE();
    }
}

bool analyze_union(ObjStruct* union_, Type** o_type)
{
    if(o_type != NULL)
        *o_type = type_apply_qualifiers(union_->type_ref, (*o_type)->qualifiers);
    if(union_->header.status == STATUS_RESOLVED || g_sema->in_ptr || g_sema->in_typedef) 
        return union_->header.kind != OBJ_INVALID;
    if(union_->header.status == STATUS_RESOLVING)
    {
        set_cyclic_def(&union_->header);
        return false;
    }
    union_->header.status = STATUS_RESOLVING;
    analyze_attributes(&union_->header);
    uint32_t largest_size = 0;

    if(o_type != NULL)
        *o_type = union_->type_ref;
    for(uint32_t i = 0; i < union_->members.size; ++i)
    {
        ObjVar* member = union_->members.data[i];
        if(!resolve_type(&member->type_loc.type, RES_NORMAL, member->type_loc.loc, "Union member cannot be of type"))
        {
            check_cyclic_def(&union_->header, union_->header.loc);
            return false;
        }
        if(member->type_loc.type->visibility < union_->header.visibility)
        {
            // TODO: Make this error print the actual visibility of both.
            sic_error_at(member->header.loc, "Member has type with less visibility than parent.");
            union_->header.status = STATUS_RESOLVED;
            union_->header.kind = OBJ_INVALID;
            return false;
        }
        uint32_t next_size = type_size(member->type_loc.type);
        if(largest_size < next_size)
        {
            largest_size = next_size;
            union_->largest_type = member->type_loc.type;
        }
    }
    union_->header.status = STATUS_RESOLVED;
    union_->type_ref->status = STATUS_RESOLVED;

    return true;
}

