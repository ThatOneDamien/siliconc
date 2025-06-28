#include "semantics.h"

static void analyze_binary(SemaContext* c, ASTExpr* expr);
static void analyze_unary(SemaContext* c, ASTExpr* expr);
static bool analyze_add(SemaContext* c, ASTExpr* expr, ASTExpr* left, ASTExpr* right);

void analyze_expr(SemaContext* c, ASTExpr* expr)
{
    switch(expr->kind)
    {
    case EXPR_BINARY: {
        ASTExprBinary* bin = &expr->expr.binary;
        analyze_expr(c, bin->lhs);
        analyze_expr(c, bin->rhs);
        if(bin->lhs->kind == EXPR_INVALID ||
           bin->rhs->kind == EXPR_INVALID)
        {
            expr->kind = EXPR_INVALID;
            return;
        }
        analyze_binary(c, expr);
        break;
    }
    case EXPR_CAST: {
        ASTExprCast* cast = &expr->expr.cast;
        analyze_expr(c, cast->expr_to_cast);
        if(cast->expr_to_cast->kind == EXPR_INVALID)
        {
            expr->kind = EXPR_INVALID;
            return;
        }
        analyze_cast(c, expr);
        break;
    }
    case EXPR_CONSTANT:
        return;
    case EXPR_FUNC_CALL: {
        ASTExprCall* call = &expr->expr.call;
        analyze_expr(c, call->func_expr);
        if(call->func_expr->kind == EXPR_INVALID)
        {
            expr->kind = EXPR_INVALID;
            return;
        }
        if(call->func_expr->kind != EXPR_IDENT || call->func_expr->expr.ident.obj->kind != OBJ_FUNC)
        {
            sema_error(c, &expr->loc, "Attempted to call non-function and non-function-pointer variable.");
            expr->kind = EXPR_INVALID;
            return;
        }
        break;
    }
    case EXPR_IDENT:
        sic_error_fatal("Encountered unexpected error.");
        break;
    case EXPR_INVALID:
    case EXPR_NOP:
        return;
    case EXPR_PRE_SEMANTIC_IDENT: {
        ASTExprIdent* ident = &expr->expr.ident;
        ident->obj = find_obj(c, &expr->loc);
        if(ident->obj == NULL)
        {
            sema_error(c, &expr->loc, "Reference to undefined symbol \'%.*s\'.", expr->loc.len, expr->loc.start);
            expr->kind = EXPR_INVALID;
            return;
        }
        expr->type = ident->obj->var.type;
        expr->kind = EXPR_IDENT;
        break;
    }
    case EXPR_TERNARY:
        break;
    case EXPR_UNARY: {
        ASTExprUnary* unary = &expr->expr.unary;
        analyze_expr(c, unary->child);
        if(unary->child->kind == EXPR_INVALID)
        {
            expr->kind = EXPR_INVALID;
            return;
        }
        analyze_unary(c, expr);
        break;
    }
    }
}

static void analyze_binary(SemaContext* c, ASTExpr* expr)
{
    ASTExpr* left = expr->expr.binary.lhs;
    ASTExpr* right = expr->expr.binary.rhs;
    switch(expr->expr.binary.kind)
    {
    case BINARY_INVALID:
        sic_error_fatal("Encountered unexpected error.");
        break;
    case BINARY_ADD:
        if(!analyze_add(c, expr, left, right))
            expr->kind = EXPR_INVALID;
        break;
    case BINARY_SUB:
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
    case BINARY_ASSIGN:
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
        SIC_TODO();
        break;
    }
}

static void analyze_unary(SemaContext* c, ASTExpr* expr)
{
    ASTExpr* child = expr->expr.unary.child;
    switch(expr->expr.unary.kind)
    {
    case UNARY_ADDR_OF:
        if(child->kind != EXPR_IDENT)
        {
            sema_error(c, &expr->loc, "Cannot take address of rvalue.");
            expr->kind = EXPR_INVALID;
            return;
        }
        expr->type = pointer_to(child->type);
        break;
    case UNARY_DEREF:
        if(child->type->kind != TYPE_POINTER)
        {
            sema_error(c, &expr->loc, "Cannot dereference non-pointer type.");
            expr->kind = EXPR_INVALID;
            return;
        }
        expr->type = child->type->pointer_base;
        break;
    case UNARY_INVALID:
        break;
    case UNARY_NEG:
        if(child->type->kind < TYPE_NUMERIC_START ||
           child->type->kind > TYPE_NUMERIC_END)
        {
            sema_error(c, &expr->loc, "Cannot negate non-numeric type.");
            expr->kind = EXPR_INVALID;
            return;
        }
        expr->type = child->type;
        break;
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
        SIC_TODO();

    expr->type = left->type;

    return true;
}
