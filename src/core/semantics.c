#include "semantics.h"
#include "utils/da.h"

static inline void analyze_main();
static void analyze_function_body(ObjFunc* function);
static bool analyze_attributes(AttrDA attrs, ObjKind kind);
static void analyze_stmt(ASTStmt* stmt, bool add_scope);
static bool analyze_stmt_block(ASTStmt* stmt);
static void analyze_break(ASTStmt* stmt);
static void analyze_continue(ASTStmt* stmt);
static void analyze_for(ASTStmt* stmt);
static void analyze_if(ASTStmt* stmt);
static void analyze_return(ASTStmt* stmt);
static void analyze_switch(ASTStmt* stmt);
static void analyze_while(ASTStmt* stmt);
static void analyze_ct_assert(ASTStmt* stmt);
static bool analyze_declaration(ObjVar* decl);
static void analyze_swap(ASTStmt* stmt);


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
        print_module(module);
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

    if(!analyze_declaration(var))
    {
        g_sema->in_global_init = true;
        check_cyclic_def(&var->header, var->header.loc);
        return false;
    }
    g_sema->in_global_init = prev;

    if(var->initial_val != NULL && !var->initial_val->const_eval)
    {
        sic_error_at(var->header.loc, "Global variable must be initialized with a compile-time evaluable value.");
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
        push_obj(&params.data[i]->header);


    ASTStmt* body = func->body;
    g_sema->cur_func = func;
    if(body != NULL && !analyze_stmt_block(body->stmt.block.body) && 
       func->signature.ret_type.type->kind != TYPE_VOID)
    {
        sic_error_at(func->header.loc, "Function does not return from all control paths.");
    }
    g_sema->cur_func = NULL;
    pop_scope(scope);
}

static UNUSED bool analyze_attributes(AttrDA attrs, ObjKind kind)
{
    bool valid = true;
    for(uint32_t i = 0; i < attrs.size; ++i)
    {
        Attr* attr = attrs.data + i;
        switch(attr->kind)
        {
        case ATTR_INLINE:
            if(attr->args.size != 0) goto TOO_MANY_ARGS;
            if(kind != OBJ_FUNC) goto ONLY_FUNCTION;
            continue;
        case ATTR_NODISCARD:
            if(attr->args.size != 0) goto TOO_MANY_ARGS;
            if(kind != OBJ_FUNC) goto ONLY_FUNCTION;
            continue;
        case ATTR_NOINLINE:
            if(attr->args.size != 0) goto TOO_MANY_ARGS;
            if(kind != OBJ_FUNC) goto ONLY_FUNCTION;
            continue;
        case ATTR_PACKED:
            if(attr->args.size != 0) goto TOO_MANY_ARGS;
            if(kind != OBJ_STRUCT)
            {
                sic_error_at(attr->loc, "Attribute @packed can only be applied to struct definitions.");
                attr->kind = ATTR_INVALID;
                valid = false;
            }
            continue;
        case ATTR_PURE:
            if(attr->args.size != 0) goto TOO_MANY_ARGS;
            if(kind != OBJ_FUNC) goto ONLY_FUNCTION;
            continue;
        case ATTR_CUSTOM:
            SIC_TODO();
        case ATTR_INVALID:
            SIC_UNREACHABLE();
        }

    TOO_MANY_ARGS:
        sic_error_at(attr->loc, "Attribute %s does not have any parameters.", attr->symbol);
        attr->kind = ATTR_INVALID;
        valid = false;
        continue;
    ONLY_FUNCTION:
        sic_error_at(attr->loc, "Attribute %s can only be applied to functions.", attr->symbol);
        attr->kind = ATTR_INVALID;
        valid = false;
        continue;
    }

    return valid;
}

static void analyze_stmt(ASTStmt* stmt, bool add_scope)
{
    switch(stmt->kind)
    {
    case STMT_INVALID:
    case STMT_NOP:
        // Just ignore these
        return;
    case STMT_BLOCK: {
        uint32_t scope = 0;
        if(add_scope)
            scope = push_scope();
        stmt->always_returns = analyze_stmt_block(stmt->stmt.block.body);
        if(add_scope)
            pop_scope(scope);
        return;
    }
    case STMT_BREAK:
        analyze_break(stmt);
        return;
    case STMT_CONTINUE:
        analyze_continue(stmt);
        return;
    case STMT_EXPR_STMT:
        analyze_expr(stmt->stmt.expr);
        return;
    case STMT_FOR:
        analyze_for(stmt);
        return;
    case STMT_IF:
        analyze_if(stmt);
        return;
    case STMT_MULTI_DECL: {
        const ObjVarDA decl_list = stmt->stmt.multi_decl;
        for(uint32_t i = 0; i < decl_list.size; ++i)
        {
            analyze_declaration(decl_list.data[i]);
            push_obj(&decl_list.data[i]->header);
        }
        return;
    }
    case STMT_RETURN:
        analyze_return(stmt);
        return;
    case STMT_SINGLE_DECL:
        analyze_declaration(stmt->stmt.single_decl);
        push_obj(&stmt->stmt.single_decl->header);
        return;
    case STMT_SWAP:
        analyze_swap(stmt);
        return;
    case STMT_SWITCH:
        analyze_switch(stmt);
        return;
    case STMT_WHILE:
        analyze_while(stmt);
        return;
    case STMT_CT_ASSERT:
        analyze_ct_assert(stmt);
        stmt->kind = STMT_NOP;
        return;
    }
    SIC_UNREACHABLE();
}

static bool analyze_stmt_block(ASTStmt* stmt)
{
    bool always_returns = false;
    while(stmt != NULL)
    {
        analyze_stmt(stmt, true);
        always_returns |= stmt->always_returns;
        stmt = stmt->next;
    }
    return always_returns;
}

static void analyze_break(ASTStmt* stmt)
{
    if(~g_sema->block_context & BLOCK_BREAKABLE)
        sic_error_at(stmt->loc, "Cannot break in the current context.");
}

static void analyze_continue(ASTStmt* stmt)
{
    if(~g_sema->block_context & BLOCK_CONTINUABLE)
        sic_error_at(stmt->loc, "Cannot continue in the current context.");
}

static void analyze_for(ASTStmt* stmt)
{
    ASTFor* for_stmt = &stmt->stmt.for_;
    uint32_t scope = push_scope();
    analyze_declaration(for_stmt->loop_var);
    push_obj(&for_stmt->loop_var->header);
    analyze_expr(for_stmt->collection);

    BlockContext context = g_sema->block_context;
    g_sema->block_context |= BLOCK_LOOP;
    analyze_stmt(for_stmt->body, false);
    g_sema->block_context = context;

    pop_scope(scope);
}

static void analyze_if(ASTStmt* stmt)
{
    ASTIf* if_stmt = &stmt->stmt.if_;
    implicit_cast(&if_stmt->cond, g_type_bool);
    analyze_stmt(if_stmt->then_stmt, true);
    if(if_stmt->else_stmt != NULL)
    {
        analyze_stmt(if_stmt->else_stmt, true);
        stmt->always_returns = if_stmt->then_stmt->always_returns & if_stmt->else_stmt->always_returns;
    }
    if(if_stmt->cond->kind == EXPR_CONSTANT)
    {
        if(if_stmt->cond->expr.constant.val.i)
        {
            sic_diagnostic_at(DIAG_WARNING, if_stmt->cond->loc,
                              "Condition always evaluates to true, consider "
                              "changing this to a #if statement or removing it.");
            *stmt = *if_stmt->then_stmt;
        }
        else
        {
            sic_diagnostic_at(DIAG_WARNING, if_stmt->cond->loc,
                              "Condition always evaluates to false, consider "
                              "changing this to a #if statement or removing it.");
            if(if_stmt->else_stmt != NULL)
                *stmt = *if_stmt->else_stmt;
            else
                stmt->kind = STMT_NOP;
        }
    }
}

static void analyze_return(ASTStmt* stmt)
{
    ASTReturn* ret = &stmt->stmt.return_;
    Type* ret_type = g_sema->cur_func->signature.ret_type.type;
    stmt->always_returns = true;
    if(ret_type->kind == TYPE_INVALID) return;
    if(ret->ret_expr != NULL)
    {
        if(ret_type->kind == TYPE_VOID)
        {
            sic_error_at(ret->ret_expr->loc, 
                            "Function returning void should not return a value.");
            return;
        }
        implicit_cast(&ret->ret_expr, ret_type);
    }
    else if(ret_type->kind != TYPE_VOID)
        sic_error_at(stmt->loc, "Function returning non-void should return a value.");

}

static void analyze_switch(ASTStmt* stmt)
{
    ASTSwitch* swi = &stmt->stmt.switch_;
    uint32_t scope;
    bool has_default = false;
    analyze_expr(swi->expr);
    if(!type_is_integer(swi->expr->type))
    {
        sic_error_at(swi->expr->loc, "Switch expression must be an integer type.");
        return;
    }
    if(type_size(swi->expr->type) < 4)
        implicit_cast(&swi->expr, g_type_int);

    BlockContext context = g_sema->block_context;
    g_sema->block_context |= BLOCK_SWITCH;
    bool always_returns = true;
    for(uint32_t i = 0; i < swi->cases.size; ++i)
    {
        ASTCase* cas = swi->cases.data + i;
        if(cas->expr != NULL)
        {
            if(!implicit_cast(&cas->expr, swi->expr->type)) goto CASE_BODY;
            if(cas->expr->kind != EXPR_CONSTANT)
            {
                sic_error_at(cas->expr->loc, "Case expression must be a compile-time evaluable constant.");
                goto CASE_BODY;
            }
            for(uint32_t j = 0; j < i; ++j)
            {
                ASTCase* other = swi->cases.data + j;
                if(other->expr->expr.constant.val.i == cas->expr->expr.constant.val.i)
                {
                    sic_error_at(cas->expr->loc, "Duplicate case for value %lu.", cas->expr->expr.constant.val.i);
                    sic_diagnostic_at(DIAG_NOTE, other->expr->loc, "Previous case statement here.");
                    goto CASE_BODY;
                }
            }
        }
        else if(has_default)
        {
            // TODO: Improve this error message, Im just too fucking lazy right now.
            sic_error_at(swi->expr->loc, "Switch statement contains duplicate default cases.");
        }
        else
            has_default = true;

    CASE_BODY:
        scope = push_scope();
        always_returns &= analyze_stmt_block(cas->body);
        pop_scope(scope);
    }
    stmt->always_returns = always_returns & has_default;
    g_sema->block_context = context;
}

static void analyze_while(ASTStmt* stmt)
{
    ASTWhile* while_stmt = &stmt->stmt.while_;
    implicit_cast(&while_stmt->cond, g_type_bool);
    BlockContext context = g_sema->block_context;
    g_sema->block_context |= BLOCK_LOOP; 
    analyze_stmt(while_stmt->body, true);
    g_sema->block_context = context;
    if(while_stmt->cond->kind == EXPR_CONSTANT &&
       !while_stmt->cond->expr.constant.val.i)
    {
        sic_diagnostic_at(DIAG_WARNING, while_stmt->cond->loc,
                          "Condition always evaluates to false, consider "
                          "removing this.");
        stmt->kind = STMT_NOP;
    }
}

static void analyze_ct_assert(ASTStmt* stmt)
{
    ASTCtAssert* assert_ = &stmt->stmt.ct_assert;
    bool valid = implicit_cast(&assert_->cond, g_type_bool);
    valid &= analyze_expr(assert_->err_msg);
    if(!valid) return;
    if(assert_->cond->kind != EXPR_CONSTANT)
    {
        sic_error_at(assert_->cond->loc, "Compile-time assert's first argument must be a "
                                         "compile-time evaluable boolean value.");
        return;
    }
    SIC_ASSERT(assert_->cond->expr.constant.kind == CONSTANT_INTEGER);
    if(assert_->err_msg->kind != EXPR_CONSTANT ||
       assert_->err_msg->expr.constant.kind != CONSTANT_STRING)
    {
        sic_error_at(assert_->err_msg->loc, "Compile-time assert's second argument must be a "
                                            "compile-time evaluable string.");
        return;
    }
    if(!assert_->cond->expr.constant.val.i)
    {
        sic_error_at(stmt->loc, "Compile-time assertion failed: %s", assert_->err_msg->expr.constant.val.str);
    }
}

static bool analyze_declaration(ObjVar* decl)
{
    if(!resolve_type(&decl->type_loc.type, RES_ALLOW_AUTO_ARRAY | RES_ALLOW_AUTO, 
                     decl->type_loc.loc, "Variable cannot be of type"))
        goto ERR;

    TypeKind kind = decl->type_loc.type->kind;
    if(decl->initial_val == NULL)
    {
        if(kind == TYPE_AUTO)
        {
            sic_error_at(decl->header.loc, "Declaring a variable with auto requires "
                                           "it to be initialized with an expression.");
            goto ERR;
        }

        if(kind == TYPE_PS_ARRAY)
        {
            sic_error_at(decl->header.loc, "Auto-sized arrays require an right hand side with an "
                                           "inferrible array size(i.e. an array literal) to be initialized.");
            goto ERR;
        }

    }
    else if(!analyze_expr(decl->initial_val))
        goto ERR;
    else
    {
        Type* rhs_type = decl->initial_val->type;
        Type* rhs_ctype = rhs_type->canonical;
        if(kind == TYPE_AUTO)
        {
            if(rhs_type->kind == TYPE_INIT_LIST)
            {
                sic_error_at(decl->header.loc, "Unable to deduce type of right hand expression. "
                             "For array literals, please declare a type.");
                goto ERR;
            }
            if(rhs_type->kind == TYPE_STRING_LIT)
            {
                SIC_ASSERT(decl->initial_val->expr.constant.kind == CONSTANT_STRING);
                // TODO: Replace this with actual string type. Most likely a char slice.
                rhs_type = type_pointer_to(g_type_char);
            }
            decl->type_loc.type = rhs_type;
        }
        else if(kind == TYPE_PS_ARRAY)
        {
            if(rhs_ctype->kind == TYPE_STATIC_ARRAY)
            {
                if(!type_equal(decl->type_loc.type->array.elem_type, rhs_ctype->array.elem_type))
                {
                    sic_error_at(decl->header.loc, 
                                 "Cannot assign auto-sized array type \'%s\' to "
                                 "incompatible array type \'%s\'.",
                                 type_to_string(decl->type_loc.type),
                                 type_to_string(rhs_type));
                    goto ERR;
                }
                decl->type_loc.type = rhs_type;
            }
            else if(rhs_type->kind == TYPE_INIT_LIST)
            {
                if(decl->initial_val->expr.array_init.size == 0)
                {
                    sic_error_at(decl->header.loc, "Cannot assign auto-sized array type to array literal with length 0.");
                    goto ERR;
                }

                decl->type_loc.type->kind = TYPE_STATIC_ARRAY;
                decl->type_loc.type->array.static_len = decl->initial_val->expr.array_init.max + 1;
                implicit_cast(&decl->initial_val, decl->type_loc.type);
            }
        }
        else
            implicit_cast(&decl->initial_val, decl->type_loc.type);
    }
    return true;
ERR:
    decl->header.kind = OBJ_INVALID;
    decl->header.status = STATUS_RESOLVED;
    return false;
}

static void analyze_swap(ASTStmt* stmt)
{
    analyze_expr(stmt->stmt.swap.left);
    analyze_expr(stmt->stmt.swap.right);
    ASTExpr* left = stmt->stmt.swap.left;
    ASTExpr* right = stmt->stmt.swap.right;
    if(expr_is_bad(left) || expr_is_bad(right) ||
       !expr_ensure_lvalue(left) || !expr_ensure_lvalue(right))
        return;

    if(!type_equal(left->type, right->type))
    {
        sic_error_at(stmt->loc, 
                     "Operands of swap statement have mismatched types \'%s\' and \'%s\'",
                     type_to_string(left->type), type_to_string(right->type));
    }

    if(!type_is_trivially_copyable(left->type))
    {
        ObjFunc* const func = g_sema->cur_func;
        func->swap_stmt_size = MAX(func->swap_stmt_size, type_size(left->type));
        func->swap_stmt_align = MAX(func->swap_stmt_align, type_alignment(left->type));
    }
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
        *o_type = type;

    if(enum_->header.status == STATUS_RESOLVED) return enum_->header.kind != OBJ_INVALID;

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
    uint64_t last_value = -1;
    for(uint32_t i = 0; i < enum_->values.size; ++i)
    {
        ObjEnumValue* value = enum_->values.data[i];
        value->enum_type = type;
        if(value->raw_value == NULL)
            value->const_value = ++last_value;
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
            last_value = value->const_value = value->raw_value->expr.constant.val.i;
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
        *o_type = struct_->type_ref;

    if(struct_->header.status == STATUS_RESOLVED || g_sema->in_ptr || g_sema->in_typedef) 
        return struct_->header.kind != OBJ_INVALID;
    if(struct_->header.status == STATUS_RESOLVING)
    {
        set_cyclic_def(&struct_->header);
        return false;
    }

    struct_->header.status = STATUS_RESOLVING;
    for(uint32_t i = 0; i < struct_->members.size; ++i)
    {
        ObjVar* member = struct_->members.data[i];
        if(!resolve_type(&member->type_loc.type, RES_ALLOW_AUTO_ARRAY, member->type_loc.loc, "Struct member cannot be of type"))
        {
            // FIXME: I believe this is wrong. I'm not sure the proper order in which to display the
            // cyclic definitions, but I definitely need to make sure this works properly.
            check_cyclic_def(&member->header, member->header.loc);
            struct_->header.status = STATUS_RESOLVED;
            struct_->header.kind = OBJ_INVALID;
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
        uint32_t align = type_alignment(member->type_loc.type);
        SIC_ASSERT(is_pow_of_2(align));
        struct_->size = ALIGN_UP(struct_->size, align) + type_size(member->type_loc.type);
        struct_->align = MAX(struct_->align, align);
    }

    struct_->size = ALIGN_UP(struct_->size, struct_->align);
    struct_->header.status = STATUS_RESOLVED;
    struct_->type_ref->status = STATUS_RESOLVED;

    return true;
}

bool analyze_typedef(ObjTypedef* typedef_, Type** o_type, ResolutionFlags flags,
                            SourceLoc err_loc, const char* err_str)
{
    SIC_ASSERT(typedef_->header.kind == OBJ_TYPEDEF);
    switch(typedef_->header.status)
    {
    case STATUS_RESOLVED:
        if(o_type != NULL)
        {
            *o_type = typedef_->alias.type;
            if(!resolve_type(o_type, flags, err_loc, err_str)) {
                check_cyclic_def(&typedef_->header, typedef_->header.loc);
                return false;
            }
            *o_type = typedef_->type_ref;
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
            *o_type = typedef_->alias.type;
            if(resolve_type(o_type, flags, err_loc, err_str)) {
                *o_type = typedef_->type_ref;
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
        *o_type = union_->type_ref;
    if(union_->header.status == STATUS_RESOLVED || g_sema->in_ptr || g_sema->in_typedef) 
        return union_->header.kind != OBJ_INVALID;
    if(union_->header.status == STATUS_RESOLVING)
    {
        set_cyclic_def(&union_->header);
        return false;
    }
    union_->header.status = STATUS_RESOLVING;
    uint32_t largest_size = 0;

    if(o_type != NULL)
        *o_type = union_->type_ref;
    for(uint32_t i = 0; i < union_->members.size; ++i)
    {
        ObjVar* member = union_->members.data[i];
        if(!resolve_type(&member->type_loc.type, RES_NORMAL, member->type_loc.loc, "Union member cannot be of type"))
        {
            // FIXME: Just like in analyze_struct
            check_cyclic_def(&member->header, member->header.loc);
            union_->header.status = STATUS_RESOLVED;
            union_->header.kind = OBJ_INVALID;
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

