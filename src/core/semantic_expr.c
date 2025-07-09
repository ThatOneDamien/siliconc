#include "semantics.h"

static bool analyze_binary(SemaContext* c, ASTExpr* expr);
static bool analyze_call(SemaContext* c, ASTExpr* expr);
static bool analyze_unary(SemaContext* c, ASTExpr* expr);
static bool analyze_add(SemaContext* c, ASTExpr* expr, ASTExpr* left, ASTExpr* right);
static bool analyze_sub(SemaContext* c, ASTExpr* expr, ASTExpr* left, ASTExpr* right);
static bool analyze_assign(SemaContext* c, ASTExpr* expr, ASTExpr* left, ASTExpr* right);
static bool analyze_op_assign(SemaContext* c, ASTExpr* expr, ASTExpr* left, ASTExpr* right);

void analyze_expr(SemaContext* c, ASTExpr* expr)
{
    switch(expr->kind)
    {
    case EXPR_BINARY: {
        ASTExprBinary* bin = &expr->expr.binary;
        analyze_expr(c, bin->lhs);
        analyze_expr(c, bin->rhs);
        if(!analyze_binary(c, expr))
            expr->kind = EXPR_INVALID;
        return;
    }
    case EXPR_CAST: {
        ASTExprCast* cast = &expr->expr.cast;
        analyze_expr(c, cast->expr_to_cast);
        if(!analyze_cast(c, expr))
            expr->kind = EXPR_INVALID;
        return;
    }
    case EXPR_FUNC_CALL: {
        ASTExprCall* call = &expr->expr.call;
        analyze_expr(c, call->func_expr);
        if(!analyze_call(c, expr))
            expr->kind = EXPR_INVALID;
        return;
    }
    case EXPR_PRE_SEMANTIC_IDENT: {
        ASTExprIdent ident = find_obj(c, &expr->loc);
        if(ident == NULL)
        {
            sema_error(c, &expr->loc, "Reference to undefined symbol \'%.*s\'.", expr->loc.len, expr->loc.start);
            expr->kind = EXPR_INVALID;
            return;
        }
        expr->expr.ident = ident;
        expr->type = ident->var.type;
        expr->kind = EXPR_IDENT;
        break;
    }
    case EXPR_UNARY: {
        ASTExprUnary* unary = &expr->expr.unary;
        analyze_expr(c, unary->child);
        if(!analyze_unary(c, expr))
            expr->kind = EXPR_INVALID;
        return;
    }
    case EXPR_CONSTANT:
    case EXPR_INVALID:
    case EXPR_NOP:
    case EXPR_TERNARY:
        return;
    default:
        SIC_UNREACHABLE();
    }
}

static bool analyze_binary(SemaContext* c, ASTExpr* expr)
{
    ASTExpr* left = expr->expr.binary.lhs;
    ASTExpr* right = expr->expr.binary.rhs;
    if(left->kind == EXPR_INVALID || right->kind == EXPR_INVALID)
        return false;
    switch(expr->expr.binary.kind)
    {
    case BINARY_ADD:
        return analyze_add(c, expr, left, right);
    case BINARY_SUB:
        return analyze_sub(c, expr, left, right);
    case BINARY_MUL:
    case BINARY_DIV:
    case BINARY_MOD:
    case BINARY_LOG_OR:
    case BINARY_LOG_AND:
    case BINARY_EQ:
    case BINARY_NE:
    case BINARY_LT:
    case BINARY_LE:
    case BINARY_GT:
    case BINARY_GE:
    case BINARY_SHL:
    case BINARY_SHR:
    case BINARY_BIT_OR:
    case BINARY_BIT_XOR:
    case BINARY_BIT_AND:
        SIC_TODO();
    case BINARY_ASSIGN:
        if(left->type->kind != right->type->kind)
            SIC_TODO_MSG("Implicit casting rules.");
        if(left->kind != EXPR_IDENT)
            SIC_TODO_MSG("Assignment of non-identifiers not handled yet.");
        expr->type = left->type;
        return true;
    case BINARY_ADD_ASSIGN:
    case BINARY_SUB_ASSIGN:
    case BINARY_MUL_ASSIGN:
    case BINARY_DIV_ASSIGN:
    case BINARY_MOD_ASSIGN:
    case BINARY_BIT_OR_ASSIGN:
    case BINARY_BIT_XOR_ASSIGN:
    case BINARY_BIT_AND_ASSIGN:
    case BINARY_SHL_ASSIGN:
    case BINARY_SHR_ASSIGN:
        return analyze_op_assign(c, expr, left, right);
    default:
        SIC_UNREACHABLE();
    }
}

static bool analyze_call(SemaContext* c, ASTExpr* expr)
{
    ASTExprCall* call = &expr->expr.call;
    if(call->func_expr->kind == EXPR_INVALID)
        return false;
    if(call->func_expr->kind != EXPR_IDENT || call->func_expr->expr.ident->kind != OBJ_FUNC)
    {
        SIC_TODO_MSG("Handle complex function calling");
        sema_error(c, &expr->loc, "Identifier in call expression is not a function.");
        return false;
    }

    Object* func = call->func_expr->expr.ident;
    if(call->args.size != func->func.params.size)
    {
        sema_error(c, &expr->loc, "Wrong number of arguments passed to function.");
        return false;
    }

    bool invalid = false;
    for(size_t i = 0; i < call->args.size; ++i)
    {
        analyze_expr(c, call->args.data[i]);
        invalid = invalid || call->args.data[i]->kind == EXPR_INVALID;
    }

    if(invalid)
        expr->kind = EXPR_INVALID;
    else
        expr->type = func->func.ret_type;

    return true;
}

static bool analyze_unary(SemaContext* c, ASTExpr* expr)
{
    ASTExpr* child = expr->expr.unary.child;
    if(child->kind == EXPR_INVALID)
        return false;
    switch(expr->expr.unary.kind)
    {
    case UNARY_ADDR_OF:
        if(child->kind != EXPR_IDENT)
        {
            sema_error(c, &expr->loc, "Cannot take address of rvalue.");
            return false;
        }
        expr->type = pointer_to(child->type);
        return true;
    case UNARY_DEREF:
        if(child->type->kind != TYPE_POINTER)
        {
            sema_error(c, &expr->loc, "Cannot dereference non-pointer type.");
            return false;
        }
        expr->type = child->type->pointer_base;
        return true;
    case UNARY_NEG:
        if(child->type->kind < TYPE_NUMERIC_START ||
           child->type->kind > TYPE_NUMERIC_END)
        {
            sema_error(c, &expr->loc, "Cannot negate non-numeric type.");
            return false;
        }
        expr->type = child->type;
        return true;
    default:
        SIC_UNREACHABLE();
    }
}

static bool analyze_add(SemaContext* c, ASTExpr* expr, ASTExpr* left, ASTExpr* right)
{
    bool left_is_pointer = left->type->kind == TYPE_POINTER;
    bool right_is_pointer = right->type->kind == TYPE_POINTER;
    if(right_is_pointer && !left_is_pointer)
    {
        left_is_pointer = true;
        right_is_pointer = false;
        ASTExpr* temp = left;
        left = right;
        right = temp;
    }

    if(left_is_pointer)
    {
        if(right->type->kind < TYPE_INTEGER_START || right->type->kind > TYPE_INTEGER_END)
        {
            sema_error(c, &expr->loc, "Invalid operand types for the add operation.");
            return false;
        }
        expr->type = left->type;
        return true;
    }

    if(left->type->kind != right->type->kind)
        SIC_TODO_MSG("Implicit casting rules.");

    expr->type = left->type;

    return true;
}

static bool analyze_sub(SemaContext* c, ASTExpr* expr, ASTExpr* left, ASTExpr* right)
{
    if(left->type->kind == TYPE_POINTER)
    {
        if(right->type->kind < TYPE_INTEGER_START || right->type->kind > TYPE_INTEGER_END)
        {
            sema_error(c, &expr->loc, "Invalid operand types for the sub operation.");
            return false;
        }
        expr->type = left->type;
        return true;
    }
    expr->type = left->type;
    return true;
}

static bool analyze_assign(UNUSED SemaContext* c, ASTExpr* expr, ASTExpr* left, ASTExpr* right)
{
    if(left->type->kind != right->type->kind)
        SIC_TODO_MSG("Implicit casting rules.");
    if(left->kind != EXPR_IDENT)
        SIC_TODO_MSG("Assignment of non-identifiers not handled yet.");
    expr->type = left->type;
    return true;
}

static bool analyze_op_assign(SemaContext* c, ASTExpr* expr, ASTExpr* left, ASTExpr* right)
{
    // TODO: Check if lhs is an lvalue
    BinaryOpKind conversion[BINARY_OP_ASSIGN_END - BINARY_OP_ASSIGN_START + 1] = {
        [BINARY_ADD_ASSIGN     - BINARY_OP_ASSIGN_START] = BINARY_ADD,
        [BINARY_SUB_ASSIGN     - BINARY_OP_ASSIGN_START] = BINARY_SUB,
        [BINARY_MUL_ASSIGN     - BINARY_OP_ASSIGN_START] = BINARY_MUL,
        [BINARY_DIV_ASSIGN     - BINARY_OP_ASSIGN_START] = BINARY_DIV,
        [BINARY_MOD_ASSIGN     - BINARY_OP_ASSIGN_START] = BINARY_MOD,
        [BINARY_BIT_OR_ASSIGN  - BINARY_OP_ASSIGN_START] = BINARY_BIT_OR,
        [BINARY_BIT_XOR_ASSIGN - BINARY_OP_ASSIGN_START] = BINARY_BIT_XOR,
        [BINARY_BIT_AND_ASSIGN - BINARY_OP_ASSIGN_START] = BINARY_BIT_AND,
        [BINARY_SHL_ASSIGN     - BINARY_OP_ASSIGN_START] = BINARY_SHL,
        [BINARY_SHR_ASSIGN     - BINARY_OP_ASSIGN_START] = BINARY_SHR,
    };

    ASTExpr* new_expr = MALLOC_STRUCT(ASTExpr);
    new_expr->kind = EXPR_BINARY;
    new_expr->loc = expr->loc;
    new_expr->expr.binary.kind = conversion[expr->expr.binary.kind - BINARY_OP_ASSIGN_START];
    new_expr->expr.binary.lhs = left;
    new_expr->expr.binary.rhs = right;
    expr->expr.binary.kind = BINARY_ASSIGN;
    expr->expr.binary.rhs = new_expr;
    analyze_binary(c, new_expr);
    return new_expr->kind != EXPR_INVALID && analyze_assign(c, expr, left, new_expr);
}
