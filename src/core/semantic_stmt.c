#include "semantics.h"

static void analyze_stmt(ASTStmt* stmt);

static void analyze_break(ASTStmt* stmt);
static void analyze_continue(ASTStmt* stmt);
static void analyze_expr_stmt(ASTStmt* stmt);
static void analyze_for(ASTStmt* stmt);
static void analyze_if(ASTStmt* stmt);
static void analyze_return(ASTStmt* stmt);
static void analyze_swap(ASTStmt* stmt);
static void analyze_switch(ASTStmt* stmt);
static void analyze_while(ASTStmt* stmt);
static void analyze_ct_if(ASTStmt* stmt);

static void analyze_stmt(ASTStmt* stmt)
{
    if(g_sema.code_is_unreachable && !g_sema.has_errored_unreachable &&
       stmt->kind != STMT_INVALID && stmt->kind != STMT_NOP)
    {
        sic_diagnostic_at(DIAG_WARNING, stmt->loc, "Code is unreachable");
        g_sema.has_errored_unreachable = true;
    }
    switch(stmt->kind)
    {
    case STMT_INVALID:
    case STMT_NOP:
        // Just ignore these
        return;
    case STMT_BLOCK: {
        uint32_t scope = 0;
        scope = push_scope();
        stmt->always_returns = analyze_stmt_block(stmt->stmt.block);
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
        da_append(&g_sema.locals, stmt->stmt.declaration);
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
    case STMT_CT_IF:
        analyze_ct_if(stmt);
        return;
    case STMT_CT_UNREACHABLE:
        stmt->always_returns = true;
        g_sema.code_is_unreachable = true;
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
        if(g_sema.break_target == NULL)
            sic_error_at(stmt->loc, "'break' statement outside of loop or switch.");
        target = g_sema.break_target;
    }
    else
        target = find_labeled_stmt(stmt->stmt.break_cont.label);
    stmt->stmt.break_cont.target = target;
    g_sema.code_is_unreachable = true;
}

static void analyze_continue(ASTStmt* stmt)
{
    ASTStmt* target;
    if(stmt->stmt.break_cont.label.sym == NULL)
    {
        if(g_sema.continue_target == NULL)
            sic_error_at(stmt->loc, "'continue' statement outside of loop.");
        target = g_sema.continue_target;
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
    g_sema.code_is_unreachable = true;
}

static void analyze_expr_stmt(ASTStmt* stmt)
{
    ASTExpr* expr = stmt->stmt.expr;
    analyze_expr(expr);
    switch(expr->kind)
    {
    case EXPR_INVALID:
    case EXPR_POSTFIX:
        return;
    case EXPR_BINARY:
        if(expr->expr.binary.kind == BINARY_ASSIGN) return;
        break;
    case EXPR_FUNC_CALL: {
        ASTExpr* func_expr = expr->expr.call.func_expr;
        Object* header = &func_expr->expr.function->header;
        if(func_expr->kind != EXPR_FUNCTION) return;
        if(has_builtin_attr(header, ATTR_NODISCARD))
        {
            sic_error_at(expr->loc, "Result of function marked @nodiscard should not be discarded.");
        }
        if(has_builtin_attr(header, ATTR_NORETURN))
        {
            stmt->always_returns = true; 
            g_sema.code_is_unreachable = true;
        }
        return;
    }
    case EXPR_UNARY: {
        UnaryOpKind kind = expr->expr.unary.kind;
        if(kind == UNARY_INC || kind == UNARY_DEC) return;
        break;
    }
    default:
        break;
    }

    sic_diagnostic_at(DIAG_WARNING, expr->loc, "Expression result unused.");
}

static void analyze_for(ASTStmt* stmt)
{
    ObjVar* loop_var = stmt->stmt.for_.loop_var;
    ASTExpr* collection = stmt->stmt.for_.collection;
    SymbolLoc label = stmt->stmt.for_.label;
    uint32_t scope = push_scope();
    push_obj(&loop_var->header);
    if(analyze_rvalue(collection))
    {
        // TODO: Change this to be generic when ranges are implemented properly.
        Type* ty = collection->type;
    RETRY:
        switch(ty->kind)
        {
        case TYPE_POINTER_MULTI:
            if(ty->pointer.static_len == 0)
            {
                sic_error_at(collection->loc, "Cannot loop over multi-pointer type '%s' because it has unknown length.", type_to_string(collection->type));
                break;
            }
            ty = ty->pointer.base;
            break;
        case TYPE_STATIC_ARRAY:
            ty = ty->array.elem_type;
            break;
        case TYPE_SLICE:
            ty = ty->slice.base;
            break;
        case TYPE_ALIAS:
            ty = ty->canonical;
            goto RETRY;
        case TYPE_STRUCT:
            if(ty == g_type_range)
            {
                ty = g_type_usize;
                break;
            }
            FALLTHROUGH;
        default:
            sic_error_at(collection->loc, "For loop cannot loop over non-collection type '%s'.", type_to_string(collection->type));
            break;
        }
        loop_var->type_loc.type = type_apply_qualifiers(ty, TYPE_QUAL_CONST);
    }
    else
        invalidate_obj(&loop_var->header);

    ASTStmt* prev_break = g_sema.break_target;
    ASTStmt* prev_continue = g_sema.continue_target;
    g_sema.break_target = stmt;
    g_sema.continue_target = stmt;
    push_labeled_stmt(stmt, label);

    analyze_stmt(stmt->stmt.for_.body);
    
    pop_labeled_stmt(stmt, label);
    g_sema.break_target = prev_break;
    g_sema.continue_target = prev_continue;
    pop_scope(scope);
}

static void analyze_if(ASTStmt* stmt)
{
    ASTExpr* cond = stmt->stmt.if_.cond;
    ASTStmt* then = stmt->stmt.if_.then_stmt;
    ASTStmt* elss = stmt->stmt.if_.else_stmt;
    bool prev_unreachable = g_sema.code_is_unreachable;
    bool prev_errored = g_sema.has_errored_unreachable;
    implicit_cast(cond, g_type_bool);
    analyze_stmt(then);
    if(elss != NULL)
    {
        g_sema.code_is_unreachable = prev_unreachable;
        g_sema.has_errored_unreachable = prev_errored;
        analyze_stmt(elss);
        stmt->always_returns = then->always_returns & elss->always_returns;
    }
    if(cond->kind == EXPR_CONSTANT)
    {
        if(cond->expr.constant.b)
        {
            sic_diagnostic_at(DIAG_WARNING, cond->loc,
                              "Condition always evaluates to true, consider "
                              "changing this to a #if statement or removing it.");
            *stmt = *then;
        }
        else
        {
            sic_diagnostic_at(DIAG_WARNING, cond->loc,
                              "Condition always evaluates to false, consider "
                              "changing this to a #if statement or removing it.");
            if(elss != NULL)
                *stmt = *elss;
            else
                stmt->kind = STMT_NOP;
        }
    }
    g_sema.code_is_unreachable = prev_unreachable;
    g_sema.has_errored_unreachable = prev_errored;
}

static void analyze_return(ASTStmt* stmt)
{
    ASTReturn* ret = &stmt->stmt.return_;
    Type* ret_type = g_sema.cur_func->signature.ret_type.type;
    stmt->always_returns = true;
    g_sema.code_is_unreachable = true;
    if(ret_type->kind == TYPE_INVALID) return;
    if(ret->ret_expr != NULL)
    {
        if(ret_type->kind == TYPE_VOID)
        {
            sic_error_at(ret->ret_expr->loc, 
                            "Function returning void should not return a value.");
            return;
        }
        implicit_cast(ret->ret_expr, ret_type);
    }
    else if(ret_type->kind != TYPE_VOID)
        sic_error_at(stmt->loc, "Function returning non-void should return a value.");

}

static void analyze_switch(ASTStmt* stmt)
{
    ASTSwitch* swi = &stmt->stmt.switch_;
    uint32_t scope;
    bool has_default = false;
    bool prev_unreachable = g_sema.code_is_unreachable;
    bool prev_errored = g_sema.has_errored_unreachable;

    analyze_rvalue(swi->expr);
    TypeKind kind = swi->expr->type->canonical->kind;
    if(!type_kind_is_integer(kind) && !type_kind_is_char(kind) && kind != TYPE_ENUM_DISTINCT)
    {
        sic_error_at(swi->expr->loc, "Switch expression can only be an integer, char, or enum type.");
        return;
    }

    if(type_size(swi->expr->type) < 4)
        implicit_cast(swi->expr, g_type_int);

    ASTStmt* prev_break = g_sema.break_target;
    g_sema.break_target = stmt;
    bool always_returns = true;
    for(uint32_t i = 0; i < swi->cases.size; ++i)
    {
        ASTCase* cas = swi->cases.data + i;
        if(cas->expr != NULL)
        {
            if(!implicit_cast(cas->expr, swi->expr->type)) goto CASE_BODY;
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
            sic_error_at(stmt->loc, "Switch statement contains duplicate default cases.");
        }
        else
            has_default = true;

    CASE_BODY:
        g_sema.code_is_unreachable = prev_unreachable;
        g_sema.has_errored_unreachable = prev_errored;
        scope = push_scope();
        always_returns &= analyze_stmt_block(cas->body);
        pop_scope(scope);
    }
    g_sema.break_target = prev_break;
    g_sema.code_is_unreachable = prev_unreachable;
    g_sema.has_errored_unreachable = prev_errored;
    stmt->always_returns = always_returns & has_default;
}

static void analyze_while(ASTStmt* stmt)
{
    ASTWhile* while_stmt = &stmt->stmt.while_;
    implicit_cast(while_stmt->cond, g_type_bool);

    ASTStmt* prev_break = g_sema.break_target;
    ASTStmt* prev_continue = g_sema.continue_target;
    bool prev_unreachable = g_sema.code_is_unreachable;
    bool prev_errored = g_sema.has_errored_unreachable;
    g_sema.break_target = stmt;
    g_sema.continue_target = stmt;
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
    g_sema.break_target = prev_break;
    g_sema.continue_target = prev_continue;
    g_sema.code_is_unreachable = prev_unreachable;
    g_sema.has_errored_unreachable = prev_errored;
}

void analyze_ct_assert(ASTStmt* stmt)
{
    ASTCtAssert* assert_ = &stmt->stmt.ct_assert;
    bool valid = implicit_cast(assert_->cond, g_type_bool);
    valid &= analyze_rvalue(assert_->err_msg);
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

void analyze_ct_if(ASTStmt* stmt)
{
    ASTIf* if_ = &stmt->stmt.if_;
    if(!implicit_cast(if_->cond, g_type_bool)) return;

    ASTExpr* cond = if_->cond;
    if(cond->kind != EXPR_CONSTANT)
    {
        sic_error_at(if_->cond->loc, "Compile-time if's condition must be compile-time evaluable.");
        return;
    }

    DBG_ASSERT(cond->expr.constant.kind == CONSTANT_BOOL);
    if(cond->expr.constant.b)
    {
        DBG_ASSERT(if_->then_stmt->kind == STMT_BLOCK);
        analyze_stmt_block(if_->then_stmt->stmt.block);
    }
    else if(if_->else_stmt)
    {
        if(if_->else_stmt->kind == STMT_CT_IF)
            analyze_ct_if(if_->else_stmt);
        else
        {
            DBG_ASSERT(if_->else_stmt->kind == STMT_BLOCK);
            analyze_stmt_block(if_->else_stmt->stmt.block);
        }
    }
}

bool analyze_declaration(ObjVar* decl)
{
    ASTExpr* initial_val = decl->initial_val;
    if(decl->type_loc.type == NULL)
    {
        if(decl->is_extern)
        {
            sic_error_at(decl->header.loc, "Extern variables require a type to be specified.");
            goto ERR;
        }
        if(initial_val == NULL)
        {
            sic_error_at(decl->header.loc, "Variables require a type or an initial value to "
                                           "be specified. Please provide at least 1.");
            goto ERR;
        } 
        if(!analyze_rvalue(initial_val))
            goto ERR;

        Type* rhs_type = initial_val->type;
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
            rhs_type = type_pointer_to_multi(g_type_char, NULL);
        }
        else if(type_is_int_literal(rhs_type))
        {
            sic_error_at(decl->header.loc, "Integer literals do not have an inherent type. Please "
                                           "explicitly define the type in the variable declaration.");
            goto ERR;
        }
        decl->type_loc.type = decl->binding_kind == VAR_BINDING_RT_CONST ? type_apply_qualifiers(rhs_type, TYPE_QUAL_CONST) : rhs_type;
        return true;
    }
    else if(!resolve_type(&decl->type_loc.type, TYPE_RES_ALLOW_AUTO_ARRAY, 
                          decl->type_loc.loc, "Variable cannot be of type %s."))
        goto ERR;

    TypeKind kind = decl->type_loc.type->kind;
    if(decl->initial_val == NULL)
    {
        if(kind == TYPE_INFERRED_ARRAY)
        {
            sic_error_at(decl->header.loc, "Auto-sized arrays require an right hand side with an "
                                           "inferrible array size(i.e. an array literal) to be initialized.");
            goto ERR;
        }
    }
    else if(!analyze_rvalue(initial_val))
        goto ERR;
    else
    {
        Type* rhs_type = initial_val->type;
        Type* rhs_ctype = rhs_type->canonical;
        if(kind == TYPE_INFERRED_ARRAY)
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
                if(initial_val->expr.array_init.size == 0)
                {
                    sic_error_at(decl->header.loc, "Cannot assign auto-sized array type to array literal with length 0.");
                    goto ERR;
                }

                decl->type_loc.type->kind = TYPE_STATIC_ARRAY;
                decl->type_loc.type->array.static_len = initial_val->expr.array_init.max + 1;
                if(!implicit_cast(initial_val, decl->type_loc.type)) return false;
            }
            else
            {
                sic_error_at(decl->header.loc, 
                             "Cannot resolve auto-sized array type \'%s\' to "
                             "incompatible type \'%s\'.",
                             type_to_string(decl->type_loc.type),
                             type_to_string(rhs_type));
                goto ERR;
            }
        }
        else if(!implicit_cast(initial_val, decl->type_loc.type))
            return false;
    }
    if(decl->binding_kind == VAR_BINDING_RT_CONST)
        decl->type_loc.type = type_apply_qualifiers(decl->type_loc.type, TYPE_QUAL_CONST);
    return true;
ERR:
    invalidate_obj(&decl->header);
    return false;
}

static void analyze_swap(ASTStmt* stmt)
{
    ASTExpr* left = stmt->stmt.swap.left;
    ASTExpr* right = stmt->stmt.swap.right;
    analyze_lvalue(left, true);
    analyze_lvalue(right, true);
    if(!analyze_rvalue_no_mutate(left) || !analyze_rvalue_no_mutate(right))
        return;

    if(!type_equal(left->type, right->type))
    {
        sic_error_at(stmt->loc, 
                     "Operands of swap statement have mismatched types \'%s\' and \'%s\'",
                     type_to_string(left->type), type_to_string(right->type));
    }

    if(!type_is_trivially_copyable(left->type))
    {
        ObjFunc* const func = g_sema.cur_func;
        func->swap_stmt_size = MAX(func->swap_stmt_size, type_size(left->type));
        func->swap_stmt_align = MAX(func->swap_stmt_align, type_alignment(left->type));
    }
}
static void analyze_swap(ASTStmt* stmt);
