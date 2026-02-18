#include "semantics.h"

static void analyze_break(ASTStmt* stmt);
static void analyze_continue(ASTStmt* stmt);
static void analyze_expr_stmt(ASTStmt* stmt);
static void analyze_for(ASTStmt* stmt);
static void analyze_if(ASTStmt* stmt);
static void analyze_return(ASTStmt* stmt);
static void analyze_swap(ASTStmt* stmt);
static void analyze_switch(ASTStmt* stmt);
static void analyze_while(ASTStmt* stmt);

void analyze_stmt(ASTStmt* stmt)
{
    switch(stmt->kind)
    {
    case STMT_INVALID:
    case STMT_NOP:
        // Just ignore these
        return;
    case STMT_BLOCK: {
        uint32_t scope = 0;
        scope = push_scope();
        stmt->always_returns = analyze_stmt_block(stmt->stmt.block.body);
        pop_scope(scope);
        return;
    }
    case STMT_BREAK:
        analyze_break(stmt);
        return;
    case STMT_CONTINUE:
        analyze_continue(stmt);
        return;
    case STMT_DECLARATION:
        analyze_declaration(stmt->stmt.declaration);
        push_obj(&stmt->stmt.declaration->header);
        da_append(&g_sema->locals, stmt->stmt.declaration);
        return;
    case STMT_EXPR_STMT:
        analyze_expr_stmt(stmt);
        return;
    case STMT_FOR:
        analyze_for(stmt);
        return;
    case STMT_IF:
        analyze_if(stmt);
        return;
    case STMT_RETURN:
        analyze_return(stmt);
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
    case STMT_CT_UNREACHABLE:
        stmt->always_returns = true;
        return;
    }
    SIC_UNREACHABLE();
}

bool analyze_stmt_block(ASTStmt* stmt)
{
    bool always_returns = false;
    while(stmt != NULL)
    {
        analyze_stmt(stmt);
        always_returns |= stmt->always_returns;
        stmt = stmt->next;
    }
    return always_returns;
}

static void analyze_break(ASTStmt* stmt)
{
    ASTStmt* target;
    if(stmt->stmt.break_cont.label.sym == NULL)
    {
        if(g_sema->break_target == NULL)
            sic_error_at(stmt->loc, "'break' statement outside of loop or switch.");
        target = g_sema->break_target;
    }
    else
        target = find_labeled_stmt(stmt->stmt.break_cont.label);
    stmt->stmt.break_cont.target = target;
}

static void analyze_continue(ASTStmt* stmt)
{
    ASTStmt* target;
    if(stmt->stmt.break_cont.label.sym == NULL)
    {
        if(g_sema->continue_target == NULL)
            sic_error_at(stmt->loc, "'continue' statement outside of loop.");
        target = g_sema->continue_target;
    }
    else
    {
        target = find_labeled_stmt(stmt->stmt.break_cont.label);
        if(target->kind == STMT_SWITCH) // Cannot continue in switch at the moment.
        {
            sic_error_at(stmt->stmt.break_cont.label.loc,
                         "Label \'%s\' refers to switch statement. 'continue' only works with loops.", 
                         stmt->stmt.break_cont.label.sym);
        }
    }
    stmt->stmt.break_cont.target = target;
}

static void analyze_expr_stmt(ASTStmt* stmt)
{
    ASTExpr* expr = stmt->stmt.expr;
    analyze_expr(expr);
    switch(expr->kind)
    {
    case EXPR_INVALID:
        return;
    case EXPR_BINARY:
        if(expr->expr.binary.kind == BINARY_ASSIGN) return;
        break;
    case EXPR_FUNC_CALL: {
        ASTExpr* func_expr = expr->expr.call.func_expr;
        if(func_expr->kind != EXPR_IDENT) return;
        Object* func = func_expr->expr.ident; 
        if(func->kind != OBJ_FUNC) return;
        if(get_builtin_attribute(func, ATTR_NODISCARD) == NULL) return;
        sic_error_at(expr->loc, "Result of function marked @nodiscard should not be discarded.");
        return;
    }
    default:
        break;
    }

    sic_diagnostic_at(DIAG_WARNING, expr->loc, "Expression result unused.");
}

static void analyze_for(ASTStmt* stmt)
{
    ASTFor* for_stmt = &stmt->stmt.for_;
    uint32_t scope = push_scope();
    analyze_declaration(for_stmt->loop_var);
    push_obj(&for_stmt->loop_var->header);
    analyze_expr(for_stmt->collection);

    ASTStmt* prev_break = g_sema->break_target;
    ASTStmt* prev_continue = g_sema->continue_target;
    g_sema->break_target = stmt;
    g_sema->continue_target = stmt;
    push_labeled_stmt(stmt, for_stmt->label);

    analyze_stmt(for_stmt->body);
    
    pop_labeled_stmt(stmt, for_stmt->label);
    g_sema->break_target = prev_break;
    g_sema->continue_target = prev_continue;
    pop_scope(scope);
}

static void analyze_if(ASTStmt* stmt)
{
    ASTIf* if_stmt = &stmt->stmt.if_;
    implicit_cast(&if_stmt->cond, g_type_bool);
    analyze_stmt(if_stmt->then_stmt);
    if(if_stmt->else_stmt != NULL)
    {
        analyze_stmt(if_stmt->else_stmt);
        stmt->always_returns = if_stmt->then_stmt->always_returns & if_stmt->else_stmt->always_returns;
    }
    if(if_stmt->cond->kind == EXPR_CONSTANT)
    {
        if(if_stmt->cond->expr.constant.b)
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

    ASTStmt* prev_break = g_sema->break_target;
    g_sema->break_target = stmt;
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
                if(i128_ucmp(other->expr->expr.constant.i, cas->expr->expr.constant.i) == 0)
                {
                    sic_error_at(cas->expr->loc, "Duplicate case for value %s.", 
                                 i128_to_string(cas->expr->expr.constant.i, type_is_signed(cas->expr->type->canonical)));
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
    g_sema->break_target = prev_break;
    stmt->always_returns = always_returns & has_default;
}

static void analyze_while(ASTStmt* stmt)
{
    ASTWhile* while_stmt = &stmt->stmt.while_;
    implicit_cast(&while_stmt->cond, g_type_bool);

    ASTStmt* prev_break = g_sema->break_target;
    ASTStmt* prev_continue = g_sema->continue_target;
    g_sema->break_target = stmt;
    g_sema->continue_target = stmt;
    push_labeled_stmt(stmt, while_stmt->label);
    analyze_stmt(while_stmt->body);
    pop_labeled_stmt(stmt, while_stmt->label);

    if(while_stmt->cond->kind == EXPR_CONSTANT &&
       !while_stmt->cond->expr.constant.b)
    {
        sic_diagnostic_at(DIAG_WARNING, while_stmt->cond->loc,
                          "Condition always evaluates to false, consider "
                          "removing this.");
        stmt->kind = STMT_NOP;
    }
    g_sema->break_target = prev_break;
    g_sema->continue_target = prev_continue;
}

void analyze_ct_assert(ASTStmt* stmt)
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
    DBG_ASSERT(assert_->cond->expr.constant.kind == CONSTANT_BOOL);
    if(assert_->err_msg->kind != EXPR_CONSTANT ||
       assert_->err_msg->expr.constant.kind != CONSTANT_STRING)
    {
        sic_error_at(assert_->err_msg->loc, "Compile-time assert's second argument must be a "
                                            "compile-time evaluable string.");
        return;
    }
    if(!assert_->cond->expr.constant.b)
    {
        sic_error_at(stmt->loc, "Compile-time assertion failed: %s", assert_->err_msg->expr.constant.str.val);
    }
}

bool analyze_declaration(ObjVar* decl)
{
    if(decl->type_loc.type == NULL)
    {
        if(decl->is_extern)
        {
            sic_error_at(decl->header.loc, "Extern variables require a type to be specified.");
            goto ERR;
        }
        if(decl->initial_val == NULL)
        {
            sic_error_at(decl->header.loc, "Variables require a type or an initial value to "
                                           "be specified. Please provide at least 1.");
            goto ERR;
        } 
        if(!analyze_expr(decl->initial_val))
            goto ERR;

        Type* rhs_type = decl->initial_val->type;
        if(rhs_type->kind == TYPE_INIT_LIST)
        {
            sic_error_at(decl->header.loc, "Unable to deduce type of right hand expression. "
                    "For array literals, please declare a type.");
            goto ERR;
        }
        if(rhs_type->kind == TYPE_STRING_LITERAL)
        {
            DBG_ASSERT(decl->initial_val->expr.constant.kind == CONSTANT_STRING);
            // TODO: Replace this with actual string type. Most likely a char slice.
            rhs_type = type_pointer_to(g_type_char);
        }
        decl->type_loc.type = decl->is_const_binding ? type_apply_qualifiers(rhs_type, TYPE_QUAL_CONST) : rhs_type;
        return true;
    }

    else if(!resolve_type(&decl->type_loc.type, RES_ALLOW_AUTO_ARRAY, 
                          decl->type_loc.loc, "Variable cannot be of type"))
        goto ERR;

    TypeKind kind = decl->type_loc.type->kind;
    if(decl->initial_val == NULL)
    {
        if(kind == TYPE_UNRESOLVED_ARRAY)
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
        if(kind == TYPE_UNRESOLVED_ARRAY) // Inferred Array
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
        else if(!implicit_cast(&decl->initial_val, decl->type_loc.type))
            return false;
    }
    if(decl->is_const_binding)
        decl->type_loc.type = type_apply_qualifiers(decl->type_loc.type, TYPE_QUAL_CONST);
    return true;
ERR:
    decl->header.kind = OBJ_INVALID;
    decl->header.status = STATUS_RESOLVED;
    return false;
}

static void analyze_swap(ASTStmt* stmt)
{
    ASTExpr* left = stmt->stmt.swap.left;
    ASTExpr* right = stmt->stmt.swap.right;
    analyze_lvalue(left, true);
    analyze_lvalue(right, true);
    if(expr_is_bad(left) || expr_is_bad(right))
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
static void analyze_swap(ASTStmt* stmt);
