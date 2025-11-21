#include "semantics.h"
#include "utils/da.h"

static void declare_global_obj(Object* global);
static void analyze_function_body(Object* function);
static void analyze_unit(CompUnit* unit);
static bool analyze_main(Object* main);
static void analyze_stmt(ASTStmt* stmt, bool add_scope);
static bool analyze_stmt_block(ASTStmt* stmt);
static void analyze_break(ASTStmt* stmt);
static void analyze_continue(ASTStmt* stmt);
static void analyze_for(ASTStmt* stmt);
static void analyze_if(ASTStmt* stmt);
static void analyze_return(ASTStmt* stmt);
static void analyze_switch(ASTStmt* stmt);
static void analyze_while(ASTStmt* stmt);
static void analyze_declaration(ASTDeclaration* decl);
static void analyze_swap(ASTStmt* stmt);

static bool analyze_enum_obj(Object* type_obj);
static bool analyze_struct_obj(Object* type_obj);
static bool analyze_type_alias(Object* type_obj, Type** o_type, ResolutionFlags flags,
                               SourceLoc err_loc, const char* err_str);
static bool analyze_union_obj(Object* type_obj);

SemaContext* g_sema = NULL;

void semantic_declaration(CompUnit* unit)
{
    SIC_ASSERT(unit != NULL);
    SemaContext* prev = g_sema;
    SemaContext sema = {0};
    g_sema = &sema;
    sema.unit = unit;
    for(uint32_t i = 0; i < unit->types.size; ++i)
        declare_global_obj(unit->types.data[i]);

    for(uint32_t i = 0; i < unit->vars.size; ++i)
        declare_global_obj(unit->vars.data[i]);

    for(uint32_t i = 0; i < unit->funcs.size; ++i)
        declare_global_obj(unit->funcs.data[i]);

    g_sema = prev;
}

void semantic_analysis(ModulePTRDA* modules)
{
    SIC_ASSERT(modules != NULL);
    for(uint32_t i = 0; i < modules->size; ++i)
    {
        Module* mod = modules->data[i];
        analyze_unit(mod->unit);
#ifdef SI_DEBUG
        if(g_args.debug_output & DEBUG_SEMA)
            print_unit(mod->unit);
#endif
    }
}

bool expr_is_lvalue(ASTExpr* expr)
{
    switch(expr->kind)
    {
    case EXPR_ARRAY_ACCESS:
    case EXPR_MEMBER_ACCESS:
        return true;
    case EXPR_IDENT:
        return expr->expr.ident->kind == OBJ_VAR;
    case EXPR_UNARY:
        return expr->expr.unary.kind == UNARY_DEREF;
    default:
        return false;
    }
}

static void declare_global_obj(Object* global)
{
    HashMap* priv = &g_sema->unit->priv_symbols;
    HashMap* pub  = &g_sema->unit->module->public_symbols;
    Object* other = hashmap_get(priv, global->symbol);
    if(other == NULL)
        other = hashmap_get(pub, global->symbol);

    if(other != NULL)
        sic_error_redef(global, other);

    if(global->visibility == VIS_PRIVATE)
        hashmap_put(priv, global->symbol, global);
    else
        hashmap_put(pub, global->symbol, global);

    if(global->symbol == g_sym_main && !analyze_main(global))
    {
        global->status = STATUS_RESOLVED;
        global->kind = OBJ_INVALID;
    }
}

static void analyze_unit(CompUnit* unit)
{
    SemaContext* prev = g_sema;
    SemaContext sema = {0};
    g_sema = &sema;
    g_sema->unit = unit;

    for(uint32_t i = 0; i < unit->types.size; ++i)
        analyze_type_obj(unit->types.data[i], NULL, RES_NORMAL, (SourceLoc){}, NULL);

    for(uint32_t i = 0; i < unit->vars.size; ++i)
        analyze_global_var(unit->vars.data[i]);

    for(uint32_t i = 0; i < unit->funcs.size; ++i)
        analyze_function_body(unit->funcs.data[i]);
    g_sema = prev;
}

bool analyze_function(Object* function)
{
    if(function->status == STATUS_RESOLVED) return function->kind != OBJ_INVALID;
    SIC_ASSERT(function->kind == OBJ_FUNC);
    if(function->status == STATUS_RESOLVING)
    {
        set_cyclic_def(function);
        return false;
    }
    FuncSignature* sig = function->func.signature;
    Type* func_type = function->type;
    ObjectDA* params = &sig->params;
    bool success = true;
    function->status = STATUS_RESOLVING;
    g_sema->in_global_init = true;

    if(!resolve_type(&sig->ret_type, RES_ALLOW_VOID, function->loc, "Function cannot have return type"))
    {
        check_cyclic_def(function, function->loc);
        success = false;
    }
    else
    {
        func_type->visibility = sig->ret_type->visibility;
        if(sig->ret_type->visibility < function->visibility)
        {
            sic_error_at(function->loc, "Function's return type has less visibility than parent function.");
            success = false;
        }
    }


    for(uint32_t i = 0; i < params->size; ++i)
    {
        Object* param = params->data[i];
        if(!resolve_type(&param->type, RES_NORMAL, param->loc, "Parameter cannot be of type"))
        {
            check_cyclic_def(param, param->loc);
            success = false;
            continue;
        }
        if(param->type->visibility < func_type->visibility)
            func_type->visibility = param->type->visibility;
        if(param->type->visibility < function->visibility)
        {
            sic_error_at(param->loc, "Parameter's type has less visibility than parent function.");
            success = false;
            continue;
        }
    }

    function->status = STATUS_RESOLVED;
    func_type->status = STATUS_RESOLVED;

    if(!success)
    {
        function->kind = OBJ_INVALID;
        func_type->kind = TYPE_INVALID;
    }

    return success;
}

static void analyze_function_body(Object* function)
{
    if(!analyze_function(function)) return;

    g_sema->in_global_init = false;
    FuncSignature* sig = function->func.signature;
    ObjectDA* params = &sig->params;

    uint32_t scope = push_scope();
    for(uint32_t i = 0; i < params->size; ++i)
    {
        Object* param = params->data[i];
        push_obj(param);
    }


    ASTStmt* body = function->func.body;
    g_sema->cur_func = function;
    if(body != NULL && !analyze_stmt_block(body->stmt.block.body) && 
       sig->ret_type->kind != TYPE_VOID)
    {
        sic_error_at(function->loc, "Function does not return from all control paths.");
    }
    g_sema->cur_func = NULL;
    pop_scope(scope);
}

bool analyze_global_var(Object* global_var)
{
    if(global_var->status == STATUS_RESOLVED) return global_var->kind != OBJ_INVALID;
    SIC_ASSERT(global_var->kind == OBJ_VAR);
    if(global_var->status == STATUS_RESOLVING)
    {
        set_cyclic_def(global_var);
        return false;
    }

    g_sema->in_global_init = true;
    global_var->status = STATUS_RESOLVING;

    if(!resolve_type(&global_var->type, RES_NORMAL, global_var->loc, "Variable cannot be of type"))
    {
        check_cyclic_def(global_var, global_var->loc);
        global_var->kind = OBJ_INVALID;
        global_var->status = STATUS_RESOLVED;
        return false;
    }

    if(global_var->var.initial_val != NULL)
    {
        if(!analyze_expr(&global_var->var.initial_val))
        {
            check_cyclic_def(global_var, global_var->loc);
            return false;
        }

    }

    // TODO: Check that this is constant. Make a function that checks if an expression is constant,
    // because something like &global_var, is technically a constant expression.
    if(global_var->type->visibility < global_var->visibility)
        // TODO: Make this error print the actual visibility of both.
        sic_error_at(global_var->loc, "Global variable's type has less visibility than the object itself.");

    global_var->status = STATUS_RESOLVED;
    return true;
}

static bool analyze_main(Object* main)
{
    if(main->kind != OBJ_FUNC)
    {
        sic_error_at(main->loc, "Symbol 'main' is reserved for entry function.");
        return false;
    }

    if(g_compiler.main_function != NULL)
    {
        sic_error_redef(main, g_compiler.main_function);
        return false;
    }

    FuncSignature* sig = main->func.signature;
    TypeKind rt_kind = sig->ret_type->kind;
    if(rt_kind != TYPE_INT && rt_kind != TYPE_VOID)
        goto BAD_SIG;

    if(sig->params.size >= 1 && sig->params.data[0]->type->kind != TYPE_INT)
        goto BAD_SIG;

    Type* second;
    if(sig->params.size >= 2)
    {
        second = sig->params.data[1]->type;
        if(second->kind != TYPE_POINTER)
            goto BAD_SIG;
        second = second->pointer_base;
        if(second->kind != TYPE_POINTER)
            goto BAD_SIG;
        second = second->pointer_base;
        if(second->kind != TYPE_CHAR)
            goto BAD_SIG;
    }

    g_compiler.main_function = main;
    return true;

BAD_SIG:
    sic_error_at(main->loc, "The signature of the main function is invalid. "
                            "The return type should be 'int' or 'void', with "
                            "optional parameters 'int, char**'.");
    return false;
}

static void analyze_stmt(ASTStmt* stmt, bool add_scope)
{
    switch(stmt->kind)
    {
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
        analyze_expr(&stmt->stmt.expr);
        return;
    case STMT_FOR:
        analyze_for(stmt);
        return;
    case STMT_GOTO:
        SIC_TODO();
    case STMT_IF:
        analyze_if(stmt);
        return;
    case STMT_LABEL:
        SIC_TODO();
    case STMT_MULTI_DECL: {
        ASTDeclDA* decl_list = &stmt->stmt.multi_decl;
        for(uint32_t i = 0; i < decl_list->size; ++i)
            analyze_declaration(decl_list->data + i);
        return;
    }
    case STMT_NOP:
        return;
    case STMT_RETURN:
        analyze_return(stmt);
        return;
    case STMT_SINGLE_DECL:
        analyze_declaration(&stmt->stmt.single_decl);
        return;
    case STMT_SWAP:
        analyze_swap(stmt);
        return;
    case STMT_SWITCH:
        analyze_switch(stmt);
        return;
    case STMT_TYPE_DECL:
        SIC_TODO();
    case STMT_WHILE:
        analyze_while(stmt);
        return;
    case STMT_INVALID:
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
    if(BIT_IS_UNSET(g_sema->block_context, BLOCK_BREAKABLE))
        sic_error_at(stmt->loc, "Cannot break in the current context.");
}

static void analyze_continue(ASTStmt* stmt)
{
    if(BIT_IS_UNSET(g_sema->block_context, BLOCK_CONTINUABLE))
        sic_error_at(stmt->loc, "Cannot continue in the current context.");
}

static void analyze_for(ASTStmt* stmt)
{
    ASTFor* for_stmt = &stmt->stmt.for_;
    uint32_t scope = push_scope();
    if(for_stmt->init_stmt != NULL)
        analyze_stmt(for_stmt->init_stmt, false);
    if(for_stmt->cond_expr != NULL)
        implicit_cast(&for_stmt->cond_expr, g_type_bool);
    if(for_stmt->loop_expr != NULL)
        analyze_expr(&for_stmt->loop_expr);

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
            sic_diagnostic_at(if_stmt->cond->loc, DIAG_WARNING, 
                              "Condition always evaluates to true, consider "
                              "changing this to a #if statement or removing it.");
            memcpy(stmt, if_stmt->then_stmt, sizeof(ASTStmt));
        }
        else
        {
            sic_diagnostic_at(if_stmt->cond->loc, DIAG_WARNING, 
                              "Condition always evaluates to false, consider "
                              "changing this to a #if statement or removing it.");
            if(if_stmt->else_stmt != NULL)
                memcpy(stmt, if_stmt->else_stmt, sizeof(ASTStmt));
            else
                stmt->kind = STMT_NOP;
        }
    }
}

static void analyze_return(ASTStmt* stmt)
{
    ASTReturn* ret = &stmt->stmt.return_;
    Type* ret_type = g_sema->cur_func->func.signature->ret_type;
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
        g_sema->ident_mask = IDENT_VAR_ONLY;
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
    analyze_expr(&swi->expr);
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
                    sic_diagnostic_at(other->expr->loc, DIAG_NOTE, "Previous case statement here.");
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
        sic_diagnostic_at(while_stmt->cond->loc, DIAG_WARNING, 
                          "Condition always evaluates to false, consider "
                          "removing this.");
        stmt->kind = STMT_NOP;
    }
}

static void analyze_declaration(ASTDeclaration* decl)
{
    Type** type_ref = &decl->obj->type;
    g_sema->ident_mask = IDENT_VAR_ONLY;
    if((*type_ref)->kind == TYPE_AUTO)
    {
        if(decl->init_expr == NULL)
        {
            sic_error_at(decl->obj->loc, "Declaring a variable with auto requires "
                                         "it to be initialized with an expression.");
            return;
        }
        if(!analyze_expr(&decl->init_expr))
            return;
        decl->obj->type = decl->init_expr->type;
    }
    else if(!resolve_type(type_ref, RES_NORMAL, decl->obj->loc, "Variable cannot be of type"))
        decl->obj->kind = OBJ_INVALID;
    else if(decl->init_expr != NULL)
        implicit_cast(&decl->init_expr, *type_ref);
    push_obj(decl->obj);
}

static void analyze_swap(ASTStmt* stmt)
{
    analyze_expr(&stmt->stmt.swap.left);
    analyze_expr(&stmt->stmt.swap.right);
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
        ObjFunc* func = &g_sema->cur_func->func;
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
        if(type_obj->status == STATUS_RESOLVED) break;
        if(!analyze_enum_obj(type_obj)) goto ERR;
        break;
    case OBJ_STRUCT:
        if(type_obj->status == STATUS_RESOLVED) break;
        if(g_sema->in_ptr || g_sema->in_typedef) break;
        if(!analyze_struct_obj(type_obj)) goto ERR;
        break;
    case OBJ_TYPE_ALIAS:
        if(!analyze_type_alias(type_obj, o_type, flags, err_loc, err_str)) goto ERR;
        return true;
    case OBJ_UNION:
        if(type_obj->status == STATUS_RESOLVED) break;
        if(g_sema->in_ptr || g_sema->in_typedef) break;
        if(!analyze_union_obj(type_obj)) goto ERR;
        break;
    case OBJ_INVALID:
        return false;
    case OBJ_ALIAS_EXPR:
    case OBJ_ENUM_VALUE:
    case OBJ_FUNC:
    case OBJ_VAR:
        SIC_UNREACHABLE();
    }
    if(o_type != NULL)
        *o_type = type_obj->type;
    return true;
ERR:
    type_obj->kind = OBJ_INVALID;
    type_obj->status = STATUS_RESOLVED;
    return false;
}

static bool analyze_enum_obj(Object* type_obj)
{
    ObjEnum* enum_ = &type_obj->enum_;
    type_obj->type->status = STATUS_RESOLVED;
    type_obj->status = STATUS_RESOLVED;
    if(enum_->underlying == NULL)
        enum_->underlying = g_type_int;
    else if(!resolve_type(&enum_->underlying, RES_NORMAL, type_obj->loc, "An enum's underlying type cannot be of type"))
        return false;
    else if(!type_is_integer(enum_->underlying))
    {
        sic_error_at(type_obj->loc, "Expected integral underlying type for enum.");
        return false;
    }

    uint32_t scope = push_scope();
    uint64_t last_value = -1;
    for(uint32_t i = 0; i < enum_->values.size; ++i)
    {
        Object* value = enum_->values.data[i];
        value->type = enum_->underlying;
        if(value->enum_val.value == NULL)
            value->enum_val.const_val = ++last_value;
        else if(!analyze_expr(&value->enum_val.value))
            return false;
        else if(value->enum_val.value->kind != EXPR_CONSTANT)
        {
            sic_error_at(value->loc, "Enum value must be assigned a constant integer expression.");
            return false;
        }
        else if(!implicit_cast(&value->enum_val.value, enum_->underlying))
            return false;
        else
            last_value = value->enum_val.const_val = value->enum_val.value->expr.constant.val.i;
        push_obj(value);
    }
    pop_scope(scope);
    return true;
}

static bool analyze_struct_obj(Object* type_obj)
{
    if(type_obj->status == STATUS_RESOLVING)
    {
        set_cyclic_def(type_obj);
        return false;
    }
    type_obj->status = STATUS_RESOLVING;
    ObjStruct* struct_ = &type_obj->struct_;
    for(uint32_t i = 0; i < struct_->members.size; ++i)
    {
        Object* member = struct_->members.data[i];
        if(!resolve_type(&member->type, RES_NORMAL, member->loc, "Struct member cannot be of type"))
        {
            check_cyclic_def(member, member->loc);
            return false;
        }
        if(member->type->visibility < type_obj->visibility)
        {
            // TODO: Make this error print the actual visibility of both.
            sic_error_at(member->loc, "Member has type with less visibility than parent.");
            return false;
        }
        uint32_t align = type_alignment(member->type);
        SIC_ASSERT(is_pow_of_2(align));
        struct_->size = ALIGN_UP(struct_->size, align) + type_size(member->type);
        struct_->align = MAX(struct_->align, align);

    }
    type_obj->type->status = STATUS_RESOLVED;
    type_obj->status = STATUS_RESOLVED;
    return true;
}

static bool analyze_type_alias(Object* type_obj, Type** o_type, ResolutionFlags flags,
                               SourceLoc err_loc, const char* err_str)
{
    switch(type_obj->status)
    {
    case STATUS_RESOLVED:
        if(o_type != NULL)
        {
            *o_type = type_obj->type;
            if(!resolve_type(o_type, flags, err_loc, err_str))
            {
                check_cyclic_def(type_obj, type_obj->loc);
                return false;
            }
        }
        return true;
    case STATUS_RESOLVING:
        if(g_sema->in_typedef)
        {
            set_cyclic_def(type_obj);
            return false;
        }
        FALLTHROUGH;
    case STATUS_UNRESOLVED: {
        type_obj->status = STATUS_RESOLVING;
        bool prev = g_sema->in_typedef;
        g_sema->in_typedef = true;
        bool success = resolve_type(&type_obj->type_alias, RES_ALLOW_VOID, type_obj->loc, "Typedef cannot be assigned to type");
        g_sema->in_typedef = prev;
        if(!success)
        {
            check_cyclic_def(type_obj, type_obj->loc);
            return false;
        }
        type_obj->status = STATUS_RESOLVED;
        type_obj->type->status = STATUS_RESOLVED;
        type_obj->type->canonical = type_obj->type->kind == TYPE_ALIAS ? type_obj->type_alias->canonical : type_obj->type;
        if(o_type != NULL)
        {
            *o_type = type_obj->type;
            return resolve_type(o_type, flags, err_loc, err_str);
        }
        return true;
    }
    default:
        SIC_UNREACHABLE();
    }
}

static bool analyze_union_obj(Object* type_obj)
{
    if(type_obj->status == STATUS_RESOLVING)
    {
        set_cyclic_def(type_obj);
        return false;
    }
    type_obj->status = STATUS_RESOLVING;
    ObjStruct* struct_ = &type_obj->struct_;
    uint32_t largest_size = 0;
    for(uint32_t i = 0; i < struct_->members.size; ++i)
    {
        Object* member = struct_->members.data[i];
        if(!resolve_type(&member->type, RES_NORMAL, member->loc, "Union member cannot be of type"))
        {
            check_cyclic_def(member->type->user_def, member->loc);
            return false;
        }
        if(member->type->visibility < type_obj->visibility)
        {
            // TODO: Make this error print the actual visibility of both.
            sic_error_at(member->loc, "Member has type with less visibility than parent.");
            return false;
        }
        uint32_t next_size = type_size(member->type);
        if(largest_size < next_size)
        {
            largest_size = next_size;
            struct_->largest_type = member->type;
        }
    }
    type_obj->type->status = STATUS_RESOLVED;
    type_obj->status = STATUS_RESOLVED;
    return true;
}

