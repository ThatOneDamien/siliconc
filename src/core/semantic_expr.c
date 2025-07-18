#include "semantics.h"

// Expr kind functions
static bool analyze_array_access(SemaContext* c, ASTExpr* expr);
static bool analyze_binary(SemaContext* c, ASTExpr* expr);
static bool analyze_call(SemaContext* c, ASTExpr* expr);
static bool analyze_unary(SemaContext* c, ASTExpr* expr);
static bool analyze_unresolved_access(SemaContext* c, ASTExpr* expr);

// Binary functions
static bool analyze_add(SemaContext* c, ASTExpr* expr, ASTExpr* left, ASTExpr* right);
static bool analyze_sub(SemaContext* c, ASTExpr* expr, ASTExpr* left, ASTExpr* right);
static bool analyze_mul(SemaContext* c, ASTExpr* expr, ASTExpr* left, ASTExpr* right);
static bool analyze_div(SemaContext* c, ASTExpr* expr, ASTExpr* left, ASTExpr* right);
static bool analyze_comparison(SemaContext* c, ASTExpr* expr, ASTExpr* left, ASTExpr* right);
static bool analyze_shift(SemaContext* c, ASTExpr* expr, ASTExpr* left, ASTExpr* right);
static bool analyze_bit_op(SemaContext* c, ASTExpr* expr, ASTExpr* left, ASTExpr* right);
static bool analyze_assign(SemaContext* c, ASTExpr* expr, ASTExpr* left, ASTExpr* right);
static bool analyze_op_assign(SemaContext* c, ASTExpr* expr, ASTExpr* left, ASTExpr* right);

// Unary functions
static bool analyze_addr_of(SemaContext* c, ASTExpr* expr, ASTExpr* child);
static bool analyze_deref(SemaContext* c, ASTExpr* expr, ASTExpr* child);
static bool analyze_negate(SemaContext* c, ASTExpr* expr, ASTExpr* child);

static bool arith_type_conv(SemaContext* c, ASTExpr* e1, ASTExpr* e2);
static void promote_int_type(SemaContext* c, ASTExpr* expr);

void analyze_expr(SemaContext* c, ASTExpr* expr)
{
    switch(expr->kind)
    {
    case EXPR_ARRAY_ACCESS: {
        ASTExprAAccess* aa = &expr->expr.array_access;
        analyze_expr(c, aa->array_expr);
        analyze_expr(c, aa->index_expr);
        if(!analyze_array_access(c, expr))
            expr->kind = EXPR_INVALID;
        return;
    }
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
        return;
    }
    case EXPR_UNARY: {
        ASTExprUnary* unary = &expr->expr.unary;
        analyze_expr(c, unary->child);
        if(!analyze_unary(c, expr))
            expr->kind = EXPR_INVALID;
        return;
    }
    case EXPR_UNRESOLVED_ACCESS: {
        ASTExprUAccess* uaccess = &expr->expr.unresolved_access;
        analyze_expr(c, uaccess);
    }
    case EXPR_CONSTANT:
    case EXPR_INVALID:
    case EXPR_NOP:
        return;
    case EXPR_TERNARY:
        SIC_TODO();
    case EXPR_IDENT:
    case EXPR_MEMBER_ACCESS:
        break;
    }
    SIC_UNREACHABLE();
}

static bool analyze_array_access(SemaContext* c, ASTExpr* expr)
{
    (void)c;
    expr->type = type_pointer_base(expr->expr.array_access.array_expr->type);
    return true;
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
        return analyze_mul(c, expr, left, right);
    case BINARY_DIV:
        return analyze_div(c, expr, left, right);
    case BINARY_MOD:
        return analyze_div(c, expr, left, right);
    case BINARY_LOG_OR:
    case BINARY_LOG_AND:
        SIC_TODO();
    case BINARY_EQ:
    case BINARY_NE:
    case BINARY_LT:
    case BINARY_LE:
    case BINARY_GT:
    case BINARY_GE:
        return analyze_comparison(c, expr, left, right);
    case BINARY_SHL:
    case BINARY_LSHR:
    case BINARY_ASHR:
        return analyze_shift(c, expr, left, right);
    case BINARY_BIT_OR:
    case BINARY_BIT_XOR:
    case BINARY_BIT_AND:
        return analyze_bit_op(c, expr, left, right);
    case BINARY_ASSIGN:
        return analyze_assign(c, expr, left, right);
    case BINARY_ADD_ASSIGN:
    case BINARY_SUB_ASSIGN:
    case BINARY_MUL_ASSIGN:
    case BINARY_DIV_ASSIGN:
    case BINARY_MOD_ASSIGN:
    case BINARY_BIT_OR_ASSIGN:
    case BINARY_BIT_XOR_ASSIGN:
    case BINARY_BIT_AND_ASSIGN:
    case BINARY_SHL_ASSIGN:
    case BINARY_LSHR_ASSIGN:
    case BINARY_ASHR_ASSIGN:
        return analyze_op_assign(c, expr, left, right);
    case BINARY_INVALID:
        break;
    }
    SIC_UNREACHABLE();
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
    FuncSignature* sig = func->func.signature;
    if(call->args.size < sig->params.size)
    {
        sema_error(c, &expr->loc, 
                   "Too few arguments passed to function. Expected %s%lu, have %lu.",
                   sig->is_var_arg ? "at least " : "", sig->params.size, call->args.size);
        return false;
    }
    if(!sig->is_var_arg && call->args.size > sig->params.size)
    {
        sema_error(c, &expr->loc, 
                   "Too many arguments passed to function. Expected %lu, have %lu.",
                   sig->params.size, call->args.size);
        return false;
    }

    bool valid = true;
    for(size_t i = 0; i < sig->params.size; ++i)
    {
        ASTExpr* arg = call->args.data[i];
        analyze_expr(c, arg);
        valid = valid && arg->kind != EXPR_INVALID &&
                implicit_cast(c, arg, sig->params.data[i]->var.type);
    }

    for(size_t i = sig->params.size; i < call->args.size; ++i)
    {
        ASTExpr* arg = call->args.data[i];
        analyze_expr(c, arg);
        if(arg->kind == EXPR_INVALID)
        {
            valid = false;
            continue;
        }
        implicit_cast_varargs(c, arg);
    }


    expr->type = sig->ret_type;

    return valid;
}

static bool analyze_unary(SemaContext* c, ASTExpr* expr)
{
    ASTExpr* child = expr->expr.unary.child;
    if(child->kind == EXPR_INVALID)
        return false;
    switch(expr->expr.unary.kind)
    {
    case UNARY_ADDR_OF:
        return analyze_addr_of(c, expr, child);
    case UNARY_DEREF:
        return analyze_deref(c, expr, child);
    case UNARY_NEG:
        return analyze_negate(c, expr, child);
    case UNARY_INVALID:
        break;
    }
    SIC_UNREACHABLE();
}

static bool analyze_unresolved_access(SemaContext* c, ASTExpr* expr)
{
    
}

static bool analyze_add(SemaContext* c, ASTExpr* expr, ASTExpr* left, ASTExpr* right)
{
    bool left_is_pointer = type_is_pointer(left->type) || left->type->kind == TYPE_SS_ARRAY;
    bool right_is_pointer = type_is_pointer(right->type) || right->type->kind == TYPE_SS_ARRAY;
    if(right_is_pointer)
    {
        left_is_pointer = true;
        ASTExpr* temp = left;
        left = right;
        right = temp;
    }

    if(left_is_pointer)
    {
        if(!type_is_integer(right->type))
        {
            sema_error(c, &expr->loc, "Invalid operand types %s and %s.",
                       type_to_string(left->type), type_to_string(right->type));
            return false;
        }

        // This checks if the constant indexing will overflow the size of the static
        // array. This check is not done for ds-arrays or when the index expression 
        // is calculated at runtime. 
        if(left->type->kind == TYPE_SS_ARRAY && right->kind == EXPR_CONSTANT && 
           right->expr.constant.val.i >= left->type->array.ss_size)
        {
            // TODO: Make this a warning?
            sema_error(c, &expr->loc, "Array index will overflow the size of the array.");
            return false;
        }
        expr->type = left->type;
        return true;
    }

    if(!arith_type_conv(c, left, right))
        return false;

    // if(left->kind == EXPR_CONSTANT && right->kind == EXPR_CONSTANT)
    // {
    //
    // }

    expr->type = left->type;
    return true;
}

static bool analyze_sub(SemaContext* c, ASTExpr* expr, ASTExpr* left, ASTExpr* right)
{
    if(left->type->kind == TYPE_POINTER)
    {
        if(!type_is_integer(right->type))
        {
            sema_error(c, &expr->loc, "Invalid operand types %s and %s.",
                       type_to_string(left->type), type_to_string(right->type));
            return false;
        }
        expr->type = left->type;
        return true;
    }

    if(!arith_type_conv(c, left, right))
        return false;

    expr->type = left->type;
    return true;
}

static bool analyze_mul(SemaContext* c, ASTExpr* expr, ASTExpr* left, ASTExpr* right)
{
    if(!type_is_numeric(left->type) || !type_is_numeric(right->type))
    {
        sema_error(c, &expr->loc, "Invalid operand types %s and %s.",
                   type_to_string(left->type), type_to_string(right->type));
        return false;
    }

    if(!arith_type_conv(c, left, right))
        return false;

    expr->type = left->type;
    return true;
}

static bool analyze_div(SemaContext* c, ASTExpr* expr, ASTExpr* left, ASTExpr* right)
{
    if(!type_is_numeric(left->type) || !type_is_numeric(right->type))
    {
        sema_error(c, &expr->loc, "Invalid operand types %s and %s.",
                   type_to_string(left->type), type_to_string(right->type));
        return false;
    }

    if(!arith_type_conv(c, left, right))
        return false;

    expr->type = left->type;
    return true;
}

static bool analyze_comparison(SemaContext* c, ASTExpr* expr, ASTExpr* left, ASTExpr* right)
{
    expr->type = g_type_bool;
    if(left->type->kind == right->type->kind && left->type->kind == TYPE_POINTER)
        return true;

    if(!type_is_numeric(left->type) || !type_is_numeric(right->type))
    {
        sema_error(c, &expr->loc, "Invalid operand types %s and %s.",
                   type_to_string(left->type), type_to_string(right->type));
        return false;
    }

    return arith_type_conv(c, left, right);
}

static bool analyze_shift(SemaContext* c, ASTExpr* expr, ASTExpr* left, ASTExpr* right)
{
    if(!type_is_integer(left->type) || !type_is_integer(right->type))
    {
        sema_error(c, &expr->loc, "Invalid operand types %s and %s.",
                   type_to_string(left->type), type_to_string(right->type));
        return false;
    }

    if(type_size(left->type) != type_size(right->type) && !implicit_cast(c, right, left->type))
        return false;

    expr->type = left->type;
    return true;
}

static bool analyze_bit_op(SemaContext* c, ASTExpr* expr, ASTExpr* left, ASTExpr* right)
{
    if(!type_is_numeric(left->type) || !type_is_numeric(right->type))
    {
        sema_error(c, &expr->loc, "Invalid operand types %s and %s.",
                   type_to_string(left->type), type_to_string(right->type));
        return false;
    }

    if(!arith_type_conv(c, left, right))
        return false;

    expr->type = left->type;
    return true;

}

static bool analyze_assign(SemaContext* c, ASTExpr* expr, ASTExpr* left, ASTExpr* right)
{
    switch(left->kind)
    {
    case EXPR_ARRAY_ACCESS:
        break;
    case EXPR_IDENT:
        if(left->expr.ident->kind != OBJ_VAR)
        {
            sema_error(c, &expr->loc, "Unable to assign object \'%.*s\'.",
                       left->expr.ident->symbol.len, left->expr.ident->symbol.start);
            return false;
        }
        break;
    case EXPR_UNARY:
        if(left->expr.unary.kind != UNARY_DEREF)
            goto ERR;
        break;
    default:
        goto ERR;
    }
    expr->type = left->type;
    return implicit_cast(c, right, left->type);

ERR:
    sema_error(c, &expr->loc, "Expression is not assignable.");
    return false;
}

static bool analyze_op_assign(SemaContext* c, ASTExpr* expr, ASTExpr* left, ASTExpr* right)
{
    // TODO: Check if lhs is an lvalue
    static BinaryOpKind conversion[BINARY_OP_ASSIGN_END - BINARY_OP_ASSIGN_START + 1] = {
        [BINARY_ADD_ASSIGN     - BINARY_OP_ASSIGN_START] = BINARY_ADD,
        [BINARY_SUB_ASSIGN     - BINARY_OP_ASSIGN_START] = BINARY_SUB,
        [BINARY_MUL_ASSIGN     - BINARY_OP_ASSIGN_START] = BINARY_MUL,
        [BINARY_DIV_ASSIGN     - BINARY_OP_ASSIGN_START] = BINARY_DIV,
        [BINARY_MOD_ASSIGN     - BINARY_OP_ASSIGN_START] = BINARY_MOD,
        [BINARY_BIT_OR_ASSIGN  - BINARY_OP_ASSIGN_START] = BINARY_BIT_OR,
        [BINARY_BIT_XOR_ASSIGN - BINARY_OP_ASSIGN_START] = BINARY_BIT_XOR,
        [BINARY_BIT_AND_ASSIGN - BINARY_OP_ASSIGN_START] = BINARY_BIT_AND,
        [BINARY_SHL_ASSIGN     - BINARY_OP_ASSIGN_START] = BINARY_SHL,
        [BINARY_LSHR_ASSIGN    - BINARY_OP_ASSIGN_START] = BINARY_LSHR,
        [BINARY_ASHR_ASSIGN    - BINARY_OP_ASSIGN_START] = BINARY_ASHR,
    };

    ASTExpr* new_expr = MALLOC_STRUCT(ASTExpr);
    new_expr->kind = EXPR_BINARY;
    new_expr->loc = expr->loc;
    new_expr->expr.binary.kind = conversion[expr->expr.binary.kind - BINARY_OP_ASSIGN_START];
    new_expr->expr.binary.lhs = MALLOC_STRUCT(ASTExpr);
    memcpy(new_expr->expr.binary.lhs, left, sizeof(ASTExpr));
    new_expr->expr.binary.rhs = right;
    expr->expr.binary.kind = BINARY_ASSIGN;
    expr->expr.binary.rhs = new_expr;
    analyze_binary(c, new_expr);
    return new_expr->kind != EXPR_INVALID && analyze_assign(c, expr, left, new_expr);
}

static bool analyze_addr_of(SemaContext* c, ASTExpr* expr, ASTExpr* child)
{
    if(child->kind != EXPR_IDENT)
    {
        sema_error(c, &expr->loc, "Cannot take address of rvalue.");
        return false;
    }
    expr->type = type_pointer_to(child->type);
    return true;
}

static bool analyze_deref(SemaContext* c, ASTExpr* expr, ASTExpr* child)
{
    switch(child->type->kind)
    {
    case TYPE_POINTER:
        expr->type = child->type->pointer_base;
        return true;
    case TYPE_SS_ARRAY:
    case TYPE_DS_ARRAY:
        expr->type = child->type->array.elem_type;
        return true;
    default:
        sema_error(c, &expr->loc, "Cannot dereference non-pointer type %s.",
                   type_to_string(child->type));
        return false;
    }
}

static bool analyze_negate(SemaContext* c, ASTExpr* expr, ASTExpr* child)
{
    if(!type_is_numeric(child->type))
    {
        sema_error(c, &expr->loc, "Cannot negate non-numeric type %s.",
                   type_to_string(child->type));
        return false;
    }

    if(child->kind == EXPR_CONSTANT)
    {
        expr->kind = EXPR_CONSTANT;
        if(child->expr.constant.kind == CONSTANT_FLOAT)
        {
            expr->expr.constant.val.f = -child->expr.constant.val.f;
            expr->expr.constant.kind = CONSTANT_FLOAT;
            return true;
        }

        expr->expr.constant.val.i = -child->expr.constant.val.i;
        expr->expr.constant.kind = CONSTANT_INTEGER;
        switch(child->type->kind)
        {
        case TYPE_UBYTE:
            expr->type = child->expr.constant.val.i > 0x7F ? g_type_short : g_type_byte;
            return true;
        case TYPE_USHORT:
            expr->type = child->expr.constant.val.i > 0x7FFF ? g_type_int : g_type_short;
            return true;
        case TYPE_UINT:
            expr->type = child->expr.constant.val.i > 0x7FFFFFFF ? g_type_long : g_type_int;
            return true;
        case TYPE_ULONG:
        case TYPE_BYTE:
        case TYPE_SHORT:
        case TYPE_INT:
        case TYPE_LONG:
            return true;
        default:
            SIC_UNREACHABLE();
        }
    }

    expr->type = child->type;
    return true;
}

static bool arith_type_conv(SemaContext* c, ASTExpr* e1, ASTExpr* e2)
{
    Type* t1 = e1->type;
    Type* t2 = e2->type;
    TypeKind kind1 = t1->kind;
    TypeKind kind2 = t2->kind;
    // TODO: Optimize this section, it should be really easy by changing the order of
    //       the TypeKind enum, but for now I just want it working.
    if(kind1 == TYPE_DOUBLE && kind2 != TYPE_DOUBLE)
        return implicit_cast(c, e2, t1);
    if(kind2 == TYPE_DOUBLE && kind1 != TYPE_DOUBLE)
        return implicit_cast(c, e1, t2);

    if(kind1 == TYPE_FLOAT && kind2 != TYPE_FLOAT)
        return implicit_cast(c, e2, t1);
    if(kind2 == TYPE_FLOAT && kind1 != TYPE_FLOAT)
        return implicit_cast(c, e1, t2);

    if(!type_is_integer(t1))
    {
        sema_error(c, &e1->loc, "Invalid operand type %s.", type_to_string(t1));
        return false;
    }
    if(!type_is_integer(t2))
    {
        sema_error(c, &e2->loc, "Invalid operand type %s.", type_to_string(t2));
        return false;
    }

    promote_int_type(c, e1);
    promote_int_type(c, e2);

    if(type_equal(e1->type, e2->type))
        return true;

    bool e1_signed = type_is_signed(e1->type);
    bool e2_signed = type_is_signed(e2->type);
    ASTExpr* high_rank;
    ASTExpr* low_rank;

    if(e1_signed == e2_signed)
    {
        if(type_size(e1->type) >= type_size(e2->type))
        {
            high_rank = e1;
            low_rank = e2;
        }
        else
        {
            high_rank = e2;
            low_rank = e1;
        }
        if(!implicit_cast(c, low_rank, high_rank->type))
            SIC_UNREACHABLE();
        return true;
    }

    ASTExpr* sint;
    ASTExpr* uint;

    if(e1_signed)
    {
        sint = e1;
        uint = e2;
    }
    else
    {
        sint = e2;
        uint = e1;
    }

    if(type_size(uint->type) >= type_size(sint->type))
    {
        if(!implicit_cast(c, sint, uint->type))
            SIC_UNREACHABLE();
        return true;
    }

    if(!implicit_cast(c, uint, sint->type))
        SIC_UNREACHABLE();
    return true;

}

static void promote_int_type(SemaContext* c, ASTExpr* expr)
{
    SIC_ASSERT(type_is_integer(expr->type));
    if(type_size(expr->type) < 4 && !implicit_cast(c, expr, g_type_int))
        SIC_UNREACHABLE();
}
