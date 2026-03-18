#include "semantics.h"
#include "utils/da.h"

static void analyze_module(ObjModule* module);
static void analyze_function(ObjFunc* function);
static void analyze_method(ObjFunc* method);
static bool analyze_attributes(Object* obj);
static bool analyze_enum(ObjEnum* enum_);
static bool analyze_struct(ObjStruct* struct_);
static bool analyze_typedef(ObjTypedef* typedef_);
static bool analyze_union(ObjStruct* union_);
static inline void analyze_main();


SemaContext g_sema = {0};

void semantic_analysis()
{
    // TODO: Add the other stages like checking for conditional compilation of symbols, and other things
    analyze_module(&g_compiler.top_module);
}

static void analyze_module(ObjModule* module)
{
    for(uint32_t i = 0; i < module->submodules.size; ++i)
        analyze_module(module->submodules.data[i]);

    for(uint32_t i = 0; i < module->imports.size; ++i)
        resolve_import(module, module->imports.data[i]);
    const SemaContext sema = g_sema;
    g_sema = (SemaContext){0};
    g_sema.module = module;

    // FIXME: Add check that we are compiling an executable. Libraries must NOT
    //        have a main function.
    if(module == &g_compiler.top_module && g_compiler.emit_link)
        analyze_main();

    // We do methods first because they have not been added to their types yet.
    // If we ecounter a method call without analyzing them first the compiler will
    // throw an error saying it isn't a member.
    for(uint32_t i = 0; i < module->methods.size; ++i)
        analyze_method(module->methods.data[i]);

    for(uint32_t i = 0; i < module->types.size; ++i)
        analyze_type_obj(module->types.data[i]);

    for(uint32_t i = 0; i < module->vars.size; ++i)
        analyze_global_var(module->vars.data[i]);

    for(uint32_t i = 0; i < module->funcs.size; ++i)
        analyze_function(module->funcs.data[i]);

    ASTStmt* assert_ = module->ct_asserts;
    while(assert_ != NULL)
    {
        analyze_ct_assert(assert_);
        assert_ = assert_->next;
    }

    g_sema = sema;
#ifdef SI_DEBUG
    if(g_compiler.debug_output & DEBUG_SEMA)
        print_module(module, false);
#endif
}

bool analyze_function_signature(ObjFunc* func)
{
    if(func->header.status == STATUS_RESOLVED) return !obj_is_bad(&func->header);
    if(func->header.status == STATUS_RESOLVING)
    {
        set_circular_def(&func->header);
        return false;
    }

    bool success = true;
    func->header.status = STATUS_RESOLVING;

    bool prev = g_sema.in_global_init;
    g_sema.in_global_init = true;

    if(!analyze_attributes(&func->header)) success = false;

    if(func->is_extern)
    {
        if(func->link_name == NULL)
            func->link_name = func->header.sym;
        if(func->body != NULL)
            sic_error_at(func->header.loc, "Function marked 'extern' cannot have a body.");
    }
    else 
    {
        if(func->link_name == NULL)
        {
            scratch_clear();
            scratch_append_obj_link_name(&func->header);
            func->link_name = scratch_copy();
        }
        if(func->body == NULL)
            sic_error_at(func->header.loc, "Function must either have a body, or be marked 'extern'.");
    }
    

    if(!resolve_type(&func->signature.ret_type.type, TYPE_RES_ALLOW_VOID, 
                     func->signature.ret_type.loc, "Function cannot have return type"))
    {
        check_circular_def(&func->header, func->header.loc);
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
        if(!resolve_type(&param->type_loc.type, TYPE_RES_NORMAL, param->type_loc.loc, "Parameter cannot be of type"))
        {
            check_circular_def(&param->header, param->header.loc);
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

    g_sema.in_global_init = prev;
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
    if(import->header.status == STATUS_RESOLVED) return !obj_is_bad(&import->header);
    if(import->header.status == STATUS_RESOLVING)
    {
        set_circular_def(&import->header);
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
            if((other_import->header.sym == NULL || other_import->header.sym == path.data[i].sym) &&
               !resolve_import(mod, other_import))
            {
                check_circular_def(&import->header, import->header.loc);
                return false;
            }
        }

        if((mod = find_module(mod, path.data[i], false)) == NULL)
            return false;
    }

    if(import->header.sym == NULL)
    {
        for(uint32_t i = 0; i < mod->imports.size; ++i)
        {
            ObjImport* other_import = mod->imports.data[i];
            if(!resolve_import(mod, other_import))
            {
                check_circular_def(&import->header, import->header.loc);
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
                    sic_error_redef(&import->header, old, "global symbol");
                    import->header.kind = OBJ_INVALID;
                    return false;
                }

                ObjImport* new = CALLOC_STRUCT(ObjImport);
                new->header.sym = entry->key;
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
                    sic_error_redef(&import->header, old, "module with name");
                    return false;
                }

                ObjImport* new = CALLOC_STRUCT(ObjImport);
                new->header.sym = entry->key;
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
        if((other_import->header.sym == NULL || other_import->header.sym == actual) &&
           !resolve_import(mod, other_import))
        {
            check_circular_def(&import->header, import->header.loc);
            return false;
        }
    }

    bool used = false;
    import->header.status = STATUS_RESOLVED;

    Object* o = hashmap_get(&mod->module_ns, actual);
    if(o != NULL)
    {
        Object* old = hashmap_get(&module->module_ns, import->header.sym);
        if(old != NULL)
        {
            sic_error_redef(&import->header, old, "module with name");
            import->header.kind = OBJ_INVALID;
            return false;
        }
        import->resolved = o->kind == OBJ_IMPORT ? obj_as_import(o)->resolved : o;
        used = true;
        hashmap_put(&module->module_ns, import->header.sym, &import->header);
    }

    o = hashmap_get(&mod->symbol_ns, actual);
    if(o != NULL)
    {
        Object* prev = hashmap_get(&module->symbol_ns, import->header.sym);
        if(prev != NULL)
        {
            sic_error_redef(&import->header, prev, "global symbol");
            if(!used)
                import->header.kind = OBJ_INVALID;
            return false;
        }
        if(used)
        {
            ObjImport* prev_import = import;
            import = MALLOC_STRUCT(ObjImport);
            *import = *prev_import;
        }
        import->resolved = o->kind == OBJ_IMPORT ? obj_as_import(o)->resolved : o;
        hashmap_put(&module->symbol_ns, import->header.sym, &import->header);
    }

    return true;
}

bool analyze_global_var(ObjVar* var)
{
    if(var->header.status == STATUS_RESOLVED) return !obj_is_bad(&var->header);
    if(var->header.status == STATUS_RESOLVING)
    {
        set_circular_def(&var->header);
        return false;
    }

    bool prev = g_sema.in_global_init;
    g_sema.in_global_init = true;
    var->header.status = STATUS_RESOLVING;
    DBG_ASSERT(!var->uninitialized);

    analyze_attributes(&var->header);
    if(var->is_extern)
    {
        if(var->link_name == NULL)
            var->link_name = var->header.sym;
        if(var->initial_val != NULL)
            sic_error_at(var->header.loc, "Global variable marked 'extern' cannot have an initial value.");
    }
    else 
    {
        if(var->link_name == NULL)
        {
            scratch_clear();
            scratch_append_obj_link_name(&var->header);
            var->link_name = scratch_copy();
        }
    }

    if(!analyze_declaration(var))
    {
        g_sema.in_global_init = prev;
        check_circular_def(&var->header, var->header.loc);
        return false;
    }
    g_sema.in_global_init = prev;

    if(var->initial_val != NULL && !var->initial_val->is_const_eval)
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

bool analyze_type_obj(Object* type_obj)
{
    DBG_ASSERT(type_obj != NULL);
    switch(type_obj->status)
    {
    case STATUS_UNRESOLVED:
        break;
    case STATUS_RESOLVING:
        set_circular_def(type_obj);
        return false;
    case STATUS_RESOLVED:
        return !obj_is_bad(type_obj);
    }

    switch(type_obj->kind)
    {
    case OBJ_BITFIELD:
        SIC_TODO();
    case OBJ_ENUM:
        return analyze_enum(obj_as_enum(type_obj));
    case OBJ_STRUCT:
        return analyze_struct(obj_as_struct(type_obj));
    case OBJ_TYPEDEF:
        return analyze_typedef(obj_as_typedef(type_obj));
    case OBJ_UNION:
        return analyze_union(obj_as_struct(type_obj));
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

Attr* get_builtin_attr(Object* obj, AttrKind kind)
{
    DBG_ASSERT(obj != NULL);
    DBG_ASSERT(kind < ATTR_CUSTOM);
    for(uint32_t i = 0; i < obj->attrs.size; ++i)
        if(obj->attrs.data[i].kind == kind)
            return obj->attrs.data + i;
    return NULL;
}

static void check_var_usage(ObjVar* var)
{
    if(var->header.sym == NULL || var->header.sym[0] == '_') return;
    const char* kind;
    switch(var->binding_kind)
    {
    case VAR_BINDING_MUTABLE:
        if(!var->read)
        {
            if(var->written)
                sic_diagnostic_at(DIAG_WARNING, var->header.loc, "Variable is written to, but its value is never read.");
            else
                sic_diagnostic_at(DIAG_WARNING, var->header.loc, "Variable is unused.");
        }
        else if(!var->written)
            sic_diagnostic_at(DIAG_WARNING, var->header.loc, "Variable is never written to, consider changing it to a const declaration.");
        return;
    case VAR_BINDING_RT_CONST:
        kind = "const";
        break;
    case VAR_BINDING_CT_CONST:
        kind = "#const";
        break;
    default:
        SIC_UNREACHABLE();
    }
    if(!var->read)
    {
        sic_diagnostic_at(DIAG_WARNING, var->header.loc, "Variable marked %s is never read.", kind);
    }
}

static void analyze_function(ObjFunc* func)
{
    if(!analyze_function_signature(func)) return;

    g_sema.in_global_init = false;
    const ObjVarDA params = func->signature.params;

    uint32_t scope = push_scope();
    for(uint32_t i = 0; i < params.size; ++i)
    {
        ObjVar* param = params.data[i];
        if(param->header.sym != NULL)
        {
            push_obj(&params.data[i]->header);
            da_append(&g_sema.locals, param);
        }
    }


    if(!func->is_extern)
    {
        g_sema.cur_func = func;
        g_sema.code_is_unreachable = false;
        g_sema.has_errored_unreachable = false;
        if(!analyze_stmt_block(func->body->stmt.block) && func->signature.ret_type.type->kind != TYPE_VOID)
        {
            sic_error_at(func->header.loc, "Function does not return from all control paths.");
        }

        for(uint32_t i = 0; i < g_sema.locals.size; ++i)
            check_var_usage(g_sema.locals.data[i]);
        g_sema.cur_func = NULL;
    }

    g_sema.locals.size = 0; // Clear locals
    pop_scope(scope);
}

static void analyze_method(ObjFunc* method)
{
    if(method->header.status == STATUS_RESOLVED) return;
    DBG_ASSERT(method->is_method);
    const ModulePath path = {.size = 1, .capacity = 1, .data = &method->method_parent};
    Object* o = find_obj(&path);
    if(o == NULL) goto ERR;
    if(o->kind != OBJ_STRUCT)
    {
        // TODO: Allow enums and unions(maybe) to have methods.
        sic_error_at(method->method_parent.loc, "Method's parent type does not refer to a struct.");
        goto ERR;
    }
    // Make sure we are only declaring methods in the module that the type is declared.
    if(o->module != method->header.module)
    {
        sic_error_at(method->method_parent.loc, "Method's parent type is not declared in this module. "
                                                "You are only allowed to put member functions in the same "
                                                "module as the parent type.");
        goto ERR;
    }
    if(!analyze_type_obj(o)) goto ERR;
    if(!analyze_function_signature(method)) return;
    ObjStruct* struct_ = obj_as_struct(o);
    for(uint32_t i = 0; i < struct_->members.size; ++i)
    {
        Object* other = &struct_->members.data[i]->header;
        if(other->sym == method->header.sym)
        {
            sic_error_redef(&method->header, other, "member/method");
            goto ERR;
        }
    }
    for(uint32_t i = 0; i < struct_->methods.size; ++i)
    {
        Object* other = &struct_->methods.data[i]->header;
        if(other->sym == method->header.sym)
        {
            sic_error_redef(&method->header, other, "member/method");
            goto ERR;
        }
    }

    const ObjVarDA params = method->signature.params;
    if(params.size == 0)
        method->is_static = true;
    else
    {
        Type* first_param = params.data[0]->type_loc.type->canonical;
        if(first_param->kind == TYPE_POINTER_SINGLE)
            first_param = first_param->pointer.base->canonical;
        if(!type_equal(first_param, struct_->type_ref))
            method->is_static = true;
    }

    da_append(&struct_->methods, method);

    analyze_function(method);
    return;
ERR:
    invalidate_obj(&method->header);
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
        case ATTR_DYNAMIC:
            if(!check_attribute_args(attr, 0)) break;
            if(kind == OBJ_FUNC) continue;
            if(kind == OBJ_VAR && obj_as_var(obj)->kind == VAR_GLOBAL) continue;
            sic_error_at(attr->loc, "Attribute %s can only be applied to global functions/vars.", attr->symbol);
            break;
        case ATTR_INLINE:
            if(!check_attribute_args(attr, 0)) break;
            if(kind != OBJ_FUNC)
            {
                sic_error_at(attr->loc, "Attribute %s can only be applied to functions.", attr->symbol);
                break;
            }
            continue;
        case ATTR_LINK_NAME: {
            if(!check_attribute_args(attr, 1)) break;
            ASTExpr* expr = attr->args.data[0];
            if(!analyze_expr(expr)) break;
            if(expr->kind != EXPR_CONSTANT || expr->expr.constant.kind != CONSTANT_STRING)
            {
                sic_error_at(attr->loc, "Attribute %s first argument should be a string constant.", attr->symbol);
                break;
            }
            switch(kind)
            {
            case OBJ_FUNC:
                obj_as_func(obj)->link_name = expr->expr.constant.str.val;
                continue;
            case OBJ_VAR: {
                ObjVar* var = obj_as_var(obj);
                if(var->kind == VAR_GLOBAL)
                {
                    var->link_name = expr->expr.constant.str.val;
                    continue;
                }
                FALLTHROUGH;
            }
            default:
                sic_error_at(attr->loc, "Attribute %s can only be applied to global functions/vars.", attr->symbol);
                break;
            }
            break;
        }
        case ATTR_NODISCARD:
            if(!check_attribute_args(attr, 0)) break;
            if(kind != OBJ_FUNC)
            {
                sic_error_at(attr->loc, "Attribute %s can only be applied to functions.", attr->symbol);
                break;
            }
            continue;
        case ATTR_NOINLINE:
            if(!check_attribute_args(attr, 0)) break;
            if(kind != OBJ_FUNC)
            {
                sic_error_at(attr->loc, "Attribute %s can only be applied to functions.", attr->symbol);
                break;
            }
            continue;
        case ATTR_NORETURN:
            if(!check_attribute_args(attr, 0)) break;
            if(kind != OBJ_FUNC)
            {
                sic_error_at(attr->loc, "Attribute %s can only be applied to functions.", attr->symbol);
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
    }

    return valid;
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
        if(second->kind != TYPE_POINTER_MULTI)
            goto BAD_SIG;
        second = second->pointer.base;
        if(second->kind != TYPE_POINTER_MULTI)
            goto BAD_SIG;
        second = second->pointer.base;
        if(second->kind != TYPE_CHAR)
            goto BAD_SIG;
    }

    func->link_name = main->sym;
    return;

BAD_SIG:
    // TODO: Fix the look of main. Obviously I know it is disgusting to have *[*]*[*]const char
    // but I haven't added slices/strings quite yet, and I want to distinguish between single 
    // and multi pointers.
    sic_error_at(main->loc, "The signature of the main function is invalid. "
                            "The return type should be 'int' or 'void', with "
                            "optional parameters 'int, *[*]*[*]const char'.");
    return;
}

bool analyze_enum_underlying(ObjEnum* enum_)
{
    Type* const type = enum_->type_ref;
    if(type->status == STATUS_RESOLVED) return !type_is_bad(type);
    if(!resolve_type(&enum_->underlying.type, TYPE_RES_NORMAL, enum_->underlying.loc, "An enum's underlying type cannot be of type"))
        goto ERR;

    enum_->underlying.type = enum_->underlying.type->canonical;
    if(!type_is_integer(enum_->underlying.type))
    {
        sic_error_at(enum_->underlying.loc, "Underlying type for enums must be an integer type.");
        goto ERR;
    }

    type->status = STATUS_RESOLVED;
    type->canonical = type->kind == TYPE_ENUM ? enum_->underlying.type : type;
    return true;
ERR:
    enum_->type_ref = g_type_invalid;
    return false;
}

static bool analyze_enum(ObjEnum* enum_)
{
    enum_->header.status = STATUS_RESOLVING;
    analyze_attributes(&enum_->header);

    bool valid = analyze_enum_underlying(enum_);

    DBG_ASSERT(enum_->values.size > 0);
    TypeKind underlying_kind = enum_->underlying.type->kind;
    Int128 min_value;
    Int128 max_value;
    if(!analyze_enum_value(enum_, 0)) valid = false;
    min_value = enum_->values.data[0]->const_value;
    max_value = min_value;

    for(uint32_t i = 1; i < enum_->values.size; ++i)
    {
        ObjEnumValue* value = enum_->values.data[i];
        for(uint32_t j = 0; j < i; ++j)
        {
            ObjEnumValue* other = enum_->values.data[j];
            if(value->header.sym == other->header.sym)
            {
                sic_error_redef(&value->header, &other->header, "enum value");
                valid = false;
                goto NEXT;
            }
        }
        if(!analyze_enum_value(enum_, i))
        {
            valid = false;
            goto NEXT;
        }
        if(i128_cmp(value->const_value, min_value, underlying_kind) < 0)
        {
            min_value = value->const_value;
            enum_->min_idx = i;
        }
        else if(i128_cmp(value->const_value, max_value, underlying_kind) > 0)
        {
            max_value = value->const_value;
            enum_->max_idx = i;
        }

    NEXT:;
    }

    enum_->header.status = STATUS_RESOLVED;

    if(!valid)
    {
        invalidate_obj(&enum_->header);
        return false;
    }
    return true;
}

bool analyze_enum_value(ObjEnum* parent, uint32_t index)
{
    DBG_ASSERT(index < parent->values.size);
    ObjEnumValue* value = parent->values.data[index];
    switch(value->header.status)
    {
    case STATUS_UNRESOLVED:
        break;
    case STATUS_RESOLVING:
        set_circular_def(&value->header);
        return false;
    case STATUS_RESOLVED:
        return !obj_is_bad(&value->header);
    }

    bool enum_valid = analyze_enum_underlying(parent);
    Type* underlying = parent->underlying.type;
    ASTExpr* value_expr = value->raw_value;

    value->header.status = STATUS_RESOLVING;


    if(value_expr != NULL) 
    {
        ObjEnum* prev = g_sema.cur_enum;
        g_sema.cur_enum = parent;
        bool valid = analyze_rvalue(value_expr);
        g_sema.cur_enum = prev;
        if(!valid) goto ERR;
        if(value_expr->kind != EXPR_CONSTANT || value_expr->expr.constant.kind != CONSTANT_INTEGER)
        {
            sic_error_at(value->header.loc, "Enum value must be assigned a compile-time constant integer value.");
            goto ERR;
        }

        if(!enum_valid) goto ERR;
        TypeKind from_kind = type_reduce(value_expr->type)->kind;
        value->const_value = value_expr->expr.constant.i;
        if(!i128_fits(value->const_value, from_kind, underlying->kind))
        {
            sic_error_at(value->header.loc, "Enum value(%s) is not representable by underlying type.",
                         i128_to_string(value->const_value, type_kind_is_signed(from_kind)));
            goto ERR;
        }
    }
    else
    {
        if(index == 0)
        {
            value->const_value = UINT128_MIN;
        }
        else if(!analyze_enum_value(parent, index - 1)) goto ERR;
        else
        {
            Int128 last = parent->values.data[index - 1]->const_value;
            if(i128_cmp(last, int_max(underlying->kind), underlying->kind) >= 0)
            {
                sic_error_at(value->header.loc, "Enum value exceeds the maximum of underlying type.");
                goto ERR;
            }
            value->const_value = i128_add64(last, 1);
        }
    }

    value->header.status = STATUS_RESOLVED;
    return true;
ERR:
    invalidate_obj(&value->header);
    return false;
}

static bool analyze_struct(ObjStruct* struct_)
{
    struct_->header.status = STATUS_RESOLVING;
    analyze_attributes(&struct_->header);
    bool packed = has_builtin_attr(&struct_->header, ATTR_PACKED);
    if(packed)
        struct_->align = 1;
    for(uint32_t i = 0; i < struct_->members.size; ++i)
    {
        ObjVar* member = struct_->members.data[i];
        if(!resolve_type(&member->type_loc.type, TYPE_RES_ALLOW_AUTO_ARRAY, member->type_loc.loc, "Struct member cannot have type %s."))
        {
            check_circular_def(&struct_->header, struct_->header.loc);
            return false;
        }
        for(uint32_t j = 0; j < i; ++j)
        {
            Object* other = &struct_->members.data[j]->header;
            if(member->header.sym == other->sym)
                sic_error_redef(&member->header, other, "member");
        }
        Type* type = member->type_loc.type;

        if(type->visibility < struct_->header.visibility)
        {
            // TODO: Make this error print the actual visibility of both.
            sic_error_at(member->header.loc, "Member has type with less visibility than parent.");
            goto ERR;
        }

        ByteSize size;
        ByteSize align;
        if(type->kind == TYPE_INFERRED_ARRAY)
        {
            if(i != struct_->members.size - 1)
            {
                sic_error_at(member->header.loc, "Inferred array is only allowed as the last member of a struct.");
                goto ERR;
            }
            size = 0;
            align = type_alignment(type->array.elem_type);
            SIC_TODO_MSG("Inferred array members not working properly.");
        }
        else
        {
            size = type_size(type);
            align = type_alignment(type);
        }

        if(packed)
        {
            struct_->size += size;
        }
        else
        {
            DBG_ASSERT(is_pow_of_2(align));
            struct_->size = ALIGN_UP(struct_->size, align) + size;
            struct_->align = MAX(struct_->align, align);
        }
    }

    struct_->size = ALIGN_UP(struct_->size, struct_->align);
    struct_->header.status = STATUS_RESOLVED;
    struct_->type_ref->status = STATUS_RESOLVED;
    return true;

ERR:
    invalidate_obj(&struct_->header);
    return false;
}

static bool analyze_typedef(ObjTypedef* typedef_)
{
    typedef_->header.status = STATUS_RESOLVING;
    analyze_attributes(&typedef_->header);
    bool prev = g_sema.type_res_allow_unresolved;
    g_sema.type_res_allow_unresolved = true;
    bool success = resolve_type(&typedef_->alias.type, TYPE_RES_ALLOW_VOID, typedef_->alias.loc, "You cannot define a type equal to %s.");
    g_sema.type_res_allow_unresolved = prev;
    if(!success)
    {
        check_circular_def(&typedef_->header, typedef_->header.loc);
        return false;
    }
    typedef_->header.status = STATUS_RESOLVED;
    typedef_->type_ref->status = STATUS_RESOLVED;
    typedef_->type_ref->canonical = typedef_->type_ref->kind == TYPE_ALIAS ?
                                        typedef_->alias.type->canonical :
                                        typedef_->type_ref;
    typedef_->alias.type = type_reduce(typedef_->alias.type);
    return true;
}

static bool analyze_union(ObjStruct* union_)
{
    union_->header.status = STATUS_RESOLVING;
    analyze_attributes(&union_->header);
    uint32_t largest_size = 0;

    for(uint32_t i = 0; i < union_->members.size; ++i)
    {
        ObjVar* member = union_->members.data[i];
        if(!resolve_type(&member->type_loc.type, TYPE_RES_NORMAL, member->type_loc.loc, "Union member cannot have type %s."))
        {
            check_circular_def(&union_->header, union_->header.loc);
            return false;
        }
        for(uint32_t j = 0; j < i; ++j)
        {
            Object* other = &union_->members.data[j]->header;
            if(member->header.sym == other->sym)
                sic_error_redef(&member->header, other, "member");
        }
        if(member->type_loc.type->visibility < union_->header.visibility)
        {
            // TODO: Make this error print the actual visibility of both.
            sic_error_at(member->header.loc, "Member has type with less visibility than parent.");
            invalidate_obj(&union_->header);
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

