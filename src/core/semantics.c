#include "semantics.h"
#include "utils/da.h"

static void resolve_all_imports(ObjModule* module);
static void analyze_module(ObjModule* module);
static void analyze_function(ObjFunc* function);
static void analyze_method(ObjFunc* method);
static inline void analyze_main();


SemaContext g_sema = {0};

void semantic_analysis()
{
    // TODO: Add the other stages like checking for conditional compilation of symbols, and other things
    resolve_all_imports(&g_compiler.top_module);
    analyze_module(&g_compiler.top_module);
}

static void resolve_all_imports(ObjModule* module)
{
    g_sema.module = module;
    g_sema.in_global_init = true;
    for(uint32_t i = 0; i < module->imports.size; ++i)
        resolve_import(module, module->imports.data[i]);

    // We do methods first because they have not been added to their types yet.
    // If we ecounter a method call without analyzing them first the compiler will
    // throw an error saying it isn't a member.
    for(uint32_t i = 0; i < module->methods.size; ++i)
        analyze_method(module->methods.data[i]);

    for(uint32_t i = 0; i < module->submodules.size; ++i)
        resolve_all_imports(module->submodules.data[i]);
}

static void analyze_module(ObjModule* module)
{
    module->header.status = STATUS_RESOLVING;
    for(uint32_t i = 0; i < module->submodules.size; ++i)
        analyze_module(module->submodules.data[i]);

    const SemaContext sema = g_sema;
    g_sema = (SemaContext){0};
    g_sema.module = module;



    for(uint32_t i = 0; i < module->types.size; ++i)
        analyze_type_obj(module->types.data[i]);

    for(uint32_t i = 0; i < module->vars.size; ++i)
        analyze_global_var(module->vars.data[i]);

    for(uint32_t i = 0; i < module->funcs.size; ++i)
        analyze_function(module->funcs.data[i]);

    for(uint32_t i = 0; i < module->methods.size; ++i)
        analyze_function(module->methods.data[i]);

    // FIXME: Add check that we are compiling an executable. Libraries must NOT
    //        have a main function.
    if(module == &g_compiler.top_module && g_compiler.emit_link)
        analyze_main();

    Stmt* assert_ = module->ct_asserts;
    while(assert_ != NULL)
    {
        analyze_ct_assert(assert_);
        assert_ = assert_->next;
    }

    module->header.status = STATUS_RESOLVED;
    g_sema = sema;
#ifdef SIC_DEBUG
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

    ObjModule* prev_mod = g_sema.module;
    g_sema.module = func->header.module;

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
    g_sema.module = prev_mod;
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
            if(other_import == import) continue;
            if(other_import->header.visibility == VIS_PRIVATE) continue;
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
            if(other_import->header.visibility == VIS_PUBLIC && !resolve_import(mod, other_import))
            {
                check_circular_def(&import->header, import->header.loc);
                return false;
            }
        }
        import->header.status = STATUS_RESOLVED;
        for(uint32_t i = 0; i < mod->symbol_ns.bucket_cnt; ++i)
        {
            HashEntry* entry = mod->symbol_ns.buckets + i;
            if(entry->key != NULL && entry->value->visibility == VIS_PUBLIC)
            {
                Object* new = entry->value;
                Object* old = hashmap_get(&module->symbol_ns, entry->key);
                if(old != NULL)
                {
                    sic_error_redef(new, old, "global symbol");
                    import->header.kind = OBJ_INVALID;
                    return false;
                }

                ObjImport* new_import = CALLOC_STRUCT(ObjImport);
                new_import->header.sym = entry->key;
                new_import->header.loc = import->header.loc;
                new_import->header.kind = OBJ_IMPORT;
                new_import->header.visibility = import->header.visibility;
                new_import->header.status = STATUS_RESOLVED;
                new_import->resolved = new->kind == OBJ_IMPORT ? obj_as_import(new)->resolved : new;
                hashmap_put(&module->symbol_ns, entry->key, &new_import->header);
            }
        }

        for(uint32_t i = 0; i < mod->module_ns.bucket_cnt; ++i)
        {
            HashEntry* entry = mod->module_ns.buckets + i;
            if(entry->key != NULL && entry->value->visibility == VIS_PUBLIC)
            {
                Object* new = entry->value;
                Object* old = hashmap_get(&module->module_ns, entry->key);
                if(old != NULL)
                {
                    sic_error_redef(new, old, "module with name");
                    return false;
                }

                ObjImport* new_import = CALLOC_STRUCT(ObjImport);
                new_import->header.sym = entry->key;
                new_import->header.loc = import->header.loc;
                new_import->header.kind = OBJ_IMPORT;
                new_import->header.visibility = import->header.visibility;
                new_import->header.status = STATUS_RESOLVED;
                new_import->resolved = new->kind == OBJ_IMPORT ? obj_as_import(new)->resolved : new;
                hashmap_put(&module->module_ns, entry->key, &new_import->header);
            }
        }
        return true;
    }

    const Symbol actual = path.data[path.size - 1].sym;

    for(uint32_t j = 0; j < mod->imports.size; ++j)
    {
        ObjImport* other_import = mod->imports.data[j];
        if(other_import == import) continue;
        if(other_import->header.visibility == VIS_PRIVATE) continue;
        if((other_import->header.sym == NULL || other_import->header.sym == actual) &&
            !resolve_import(mod, other_import))
        {
            check_circular_def(&import->header, import->header.loc);
            return false;
        }
    }

    bool used = false;
    bool is_private = false;
    import->header.status = STATUS_RESOLVED;

    Object* o = hashmap_get(&mod->module_ns, actual);
    if(o != NULL)
    {
        if(o->visibility == VIS_PUBLIC)
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
        else
            is_private = true;
    }

    o = hashmap_get(&mod->symbol_ns, actual);
    if(o != NULL)
    {
        if(o->visibility == VIS_PUBLIC)
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
            used = true;
        }
        else
            is_private = true;
    }
    if(!used)
    {
        if(is_private)
            // TODO: Maybe show the definition location of the private object?
            sic_error_at(import->header.loc, "Object \'%s\' is declared as private.", import->header.sym);
        else
            sic_error_at(import->header.loc, "Failed to import object with name \'%s\'. Is the path correct?", import->header.sym);
        return false;
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

    ObjModule* prev_mod = g_sema.module;
    g_sema.module = var->header.module;

    bool success = true;

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
        check_circular_def(&var->header, var->header.loc);
        success = false;
    }
    else
    {
        if(var->initial_val != NULL && !var->initial_val->is_const_eval)
        {
            sic_error_at(var->initial_val->loc, "Global variable must be initialized with a compile-time evaluable value.");
            invalidate_obj(&var->header);
            success = false;
        }

        if(var->type_loc.type->visibility < var->header.visibility)
            // TODO: Make this error print the actual visibility of both.
            sic_error_at(var->header.loc, "Global variable's type has less visibility than the object itself.");
    }

    g_sema.in_global_init = prev;
    g_sema.module = prev_mod;
    var->header.status = STATUS_RESOLVED;
    return success;
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
    if(var->header.kind == OBJ_INVALID || var->header.sym == NULL || var->header.sym[0] == '_') return;
    if(g_compiler.disabled_warnings &= WARN_USAGE) return;
    if(var->binding_kind == VAR_BINDING_MUTABLE)
    {
        if(!var->read)
        {
            if(var->written)
                sic_diagnostic_at(DIAG_WARNING, var->header.loc, "Variable is written to, but its value is never read.");
            else
                sic_diagnostic_at(DIAG_WARNING, var->header.loc, "Variable is unused.");
        }
        else if(!var->written)
            sic_diagnostic_at(DIAG_WARNING, var->header.loc, "Variable is never written to, consider changing it to a const declaration.");
    }
    else if(!var->read)
    {
        sic_diagnostic_at(DIAG_WARNING, var->header.loc, "Variable's value is never read.");
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
        if(!analyze_stmt_list(func->body) && func->signature.ret_type.type->kind != TYPE_VOID)
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
    if(!obj_is_type(o))
    {
        sic_error_at(method->method_parent.loc, "Symbol does not refer to a type, only types can have methods.");
        goto ERR;
    }

    ObjType* ot = obj_as_type(o);
    if(ot->type_ref->kind == TYPE_ENUM || ot->type_ref->kind == TYPE_ALIAS)
    {
        sic_error_at(method->method_parent.loc, "Non-distinct aliases/enums cannot have methods. Please mark the type 'distinct' if you wish to add some.");
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
    if(!analyze_type_obj(ot)) goto ERR;
    if(!analyze_function_signature(method)) return;

    {
        Object* other = find_type_member(ot, method->header.sym);
        if(other != NULL)
            sic_error_redef(&method->header, other, "member/method");
    }

    const ObjVarDA params = method->signature.params;
    if(params.size == 0)
        method->is_static = true;
    else
    {
        Type* first_param = params.data[0]->type_loc.type->canonical;
        if(first_param->kind == TYPE_POINTER_SINGLE)
            first_param = first_param->pointer.base->canonical;
        if(!type_equal(first_param, ot->type_ref))
            method->is_static = true;
    }

    da_append(&ot->methods, method);

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

bool analyze_attributes(Object* obj)
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
            Expr* expr = attr->args.data[0];
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
        sic_fatal_error("Root module is missing main function. An example declaration is:\nfn main() void\n{\n}");

    if(main->kind != OBJ_FUNC)
    {
        sic_error_at(main->loc, "Symbol 'main' in the root module is reserved for the entry function.");
        return;
    }

    ObjFunc* func = obj_as_func(main);
    const ObjVarDA params = func->signature.params;
    TypeKind rt_kind = func->signature.ret_type.type->canonical->kind;
    if((rt_kind != TYPE_INT && rt_kind != TYPE_VOID) || params.size > 1)
        goto BAD_SIG;

    if(params.size == 1)
    {
        Type* param = params.data[0]->type_loc.type->canonical;
        if(param->kind != TYPE_SLICE)
            goto BAD_SIG;
        param = param->slice.base->canonical;
        if(param->kind != TYPE_POINTER_MULTI_UNKNOWN)
            goto BAD_SIG;
        param = param->pointer.base->canonical;
        if(param->kind != TYPE_CHAR)
            goto BAD_SIG;
    }

    g_compiler.main_function = func;
    return;

BAD_SIG:
    // TODO: Fix the look of main. Obviously I know it is disgusting to have []*[*]const char
    // but now even with slices, I don't want the startup to take O(n) in terms of length of the input
    // for no reason if you don't end up using the args. In the future it should be someting like:
    // [][]const char or []string.
    sic_error_at(main->loc, "The signature of the main function is invalid. "
                            "The return type should be 'int' or 'void', with "
                            "optional parameter 'args: []*[*]const char'.");
    return;
}

