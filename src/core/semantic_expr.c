#include "semantics.h"

// Expr kind functions
static bool analyze_array_access(SemaContext* c, ASTExpr* expr);
static bool analyze_binary(SemaContext* c, ASTExpr* expr);
static bool analyze_call(SemaContext* c, ASTExpr* expr);
static bool analyze_ternary(SemaContext* c, ASTExpr* expr);
static bool analyze_unary(SemaContext* c, ASTExpr* expr);
static bool analyze_unresolved_arrow(SemaContext* c, ASTExpr* expr);
static bool analyze_unresolved_dot(SemaContext* c, ASTExpr* expr);

// Binary functions
static bool analyze_add(SemaContext* c, ASTExpr* expr, ASTExpr* left, ASTExpr* right);
static bool analyze_sub(SemaContext* c, ASTExpr* expr, ASTExpr* left, ASTExpr* right);
static bool analyze_mul(SemaContext* c, ASTExpr* expr, ASTExpr* left, ASTExpr* right);
static bool analyze_div(SemaContext* c, ASTExpr* expr, ASTExpr* left, ASTExpr* right);
static bool analyze_logical(SemaContext* c, ASTExpr* expr, ASTExpr* left, ASTExpr* right);
static bool analyze_comparison(SemaContext* c, ASTExpr* expr, ASTExpr* left, ASTExpr* right);
static bool analyze_shift(SemaContext* c, ASTExpr* expr, ASTExpr* left, ASTExpr* right);
static bool analyze_bit_op(SemaContext* c, ASTExpr* expr, ASTExpr* left, ASTExpr* right);
static bool analyze_assign(SemaContext* c, ASTExpr* expr, ASTExpr* left, ASTExpr* right);
static bool analyze_op_assign(SemaContext* c, ASTExpr* expr, ASTExpr* left, ASTExpr* right);

// Unary functions
static bool analyze_addr_of(SemaContext* c, ASTExpr* expr, ASTExpr* inner);
static bool analyze_bit_not(SemaContext* c, ASTExpr* expr, ASTExpr* inner);
static bool analyze_deref(SemaContext* c, ASTExpr* expr, ASTExpr* inner);
static bool analyze_incdec(SemaContext* c, ASTExpr* expr, ASTExpr* inner);
static bool analyze_log_not(SemaContext* c, ASTExpr* expr, ASTExpr* inner);
static bool analyze_negate(SemaContext* c, ASTExpr* expr, ASTExpr* inner);

static bool arith_type_conv(SemaContext* c, ASTExpr* parent, ASTExpr* e1, ASTExpr* e2);
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
        analyze_expr(c, cast->inner);
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
    case EXPR_POSTFIX: {
        ASTExpr* inner = expr->expr.unary.inner;
        analyze_expr(c, inner);
        if(inner->kind == EXPR_INVALID)
            expr->kind = EXPR_INVALID;
        else
            analyze_incdec(c, expr, inner);
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
        analyze_expr(c, unary->inner);
        if(!analyze_unary(c, expr))
            expr->kind = EXPR_INVALID;
        return;
    }
    case EXPR_UNRESOLVED_ARR: {
        ASTExprUAccess* uaccess = &expr->expr.unresolved_access;
        analyze_expr(c, uaccess->parent_expr);
        if(!analyze_unresolved_arrow(c, expr))
            expr->kind = EXPR_INVALID;
        return;
    }
    case EXPR_UNRESOLVED_DOT: {
        ASTExprUAccess* uaccess = &expr->expr.unresolved_access;
        analyze_expr(c, uaccess->parent_expr);
        if(!analyze_unresolved_dot(c, expr))
            expr->kind = EXPR_INVALID;
        return;
    }
    case EXPR_CONSTANT:
    case EXPR_INVALID:
    case EXPR_NOP:
        return;
    case EXPR_TERNARY: {
        ASTExprTernary* ternary = &expr->expr.ternary;
        analyze_expr(c, ternary->cond_expr);
        if(ternary->then_expr != NULL)
            analyze_expr(c, ternary->then_expr);
        analyze_expr(c, ternary->else_expr);
        if(!analyze_ternary(c, expr))
            expr->kind = EXPR_INVALID;
        return;
    }
    case EXPR_IDENT:
    case EXPR_MEMBER_ACCESS:
        break;
    }
    SIC_UNREACHABLE();
}

static bool analyze_array_access(SemaContext* c, ASTExpr* expr)
{
    ASTExpr* arr = expr->expr.array_access.array_expr;
    ASTExpr* index = expr->expr.array_access.index_expr;
    if(arr->kind == EXPR_INVALID || index->kind == EXPR_INVALID)
        return false;

    if(!type_is_array(arr->type) && !type_is_pointer(arr->type))
    {
        sema_error(c, &expr->loc, "Attempted to access element of non-array and non-pointer type \'%s\'",
                   type_to_string(arr->type));
        return false;
    }

    if(arr->type->kind == TYPE_SS_ARRAY && index->kind == EXPR_CONSTANT && 
       index->expr.constant.val.i >= arr->type->array.ss_size)
    {
        sic_diagnostic_at(c->unit->file.full_path, &expr->loc, DIAG_WARNING,
                          "Array index will overflow the size of the array.");
        return false;
    }

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
        return analyze_logical(c, expr, left, right);
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

static bool analyze_ternary(SemaContext* c, ASTExpr* expr)
{
    ASTExpr* cond = expr->expr.ternary.cond_expr;
    ASTExpr* then = expr->expr.ternary.then_expr;
    ASTExpr* elss = expr->expr.ternary.else_expr;
    if(cond->kind == EXPR_INVALID || elss->kind == EXPR_INVALID)
        return false;

    if(then == NULL)
        then = cond;
    else if(then->kind == EXPR_INVALID)
        return false;

    if(type_equal(then->type, elss->type))
        goto EXIT;

    if(!arith_type_conv(c, expr, then, elss))
        return false;
EXIT:
    expr->type = elss->type;
    return implicit_cast(c, cond, g_type_bool);
}

static bool analyze_unary(SemaContext* c, ASTExpr* expr)
{
    ASTExpr* inner = expr->expr.unary.inner;
    if(inner->kind == EXPR_INVALID)
        return false;
    switch(expr->expr.unary.kind)
    {
    case UNARY_ADDR_OF:
        return analyze_addr_of(c, expr, inner);
    case UNARY_BIT_NOT:
        return analyze_bit_not(c, expr, inner);
    case UNARY_DEC:
    case UNARY_INC:
        return analyze_incdec(c, expr, inner);
    case UNARY_DEREF:
        return analyze_deref(c, expr, inner);
    case UNARY_LOG_NOT:
        return analyze_log_not(c, expr, inner);
    case UNARY_NEG:
        return analyze_negate(c, expr, inner);
    case UNARY_INVALID:
        break;
    }
    SIC_UNREACHABLE();
}

static Object* resolve_member(SemaContext* c, Type* type, SourceLoc* symbol)
{
    ObjectDA* members = &type->user_def->struct_.members;
    for(size_t i = 0; i < members->size; ++i)
        if(members->data[i]->symbol.len == symbol->len &&
           memcmp(members->data[i]->symbol.start, symbol->start, symbol->len) == 0)
            return members->data[i];
    sema_error(c, symbol, "Struct %.*s has no member %.*s.",
               type->user_def->symbol.len, type->user_def->symbol.start,
               symbol->len, symbol->start);
    return NULL;
}

static bool analyze_unresolved_arrow(SemaContext* c, ASTExpr* expr)
{
    ASTExprUAccess* uaccess = &expr->expr.unresolved_access;
    ASTExpr* parent = uaccess->parent_expr;
    if(parent->kind == EXPR_INVALID)
        return false;

    if(parent->type->kind != TYPE_POINTER || !type_is_user_def(parent->type->pointer_base))
    {
        sema_error(c, &expr->loc, "Arrow operator can only be used on pointers to structure-like types.");
        return false;
    }

    ASTExpr* deref = CALLOC_STRUCT(ASTExpr);
    deref->type = parent->type->pointer_base;
    deref->loc = expr->loc;
    deref->kind = EXPR_UNARY;
    deref->expr.unary.kind = UNARY_DEREF;
    deref->expr.unary.inner = parent;
    Object* member = resolve_member(c, deref->type, &uaccess->member_expr->loc);
    if(member == NULL)
        return false;

    expr->kind = EXPR_MEMBER_ACCESS;
    expr->expr.member_access.parent_expr = deref;
    expr->expr.member_access.member = member;
    expr->type = member->var.type;
    return true;
}

static bool analyze_unresolved_dot(SemaContext* c, ASTExpr* expr)
{
    ASTExprUAccess* uaccess = &expr->expr.unresolved_access;
    ASTExpr* parent = uaccess->parent_expr;
    if(parent->kind == EXPR_INVALID)
        return false;

    if(parent->type->kind == TYPE_POINTER && type_is_user_def(parent->type->pointer_base))
    {
        ASTExpr* deref = CALLOC_STRUCT(ASTExpr);
        deref->type = parent->type->pointer_base;
        deref->loc = expr->loc;
        deref->kind = EXPR_UNARY;
        deref->expr.unary.kind = UNARY_DEREF;
        deref->expr.unary.inner = parent;
        parent = deref;
    }
    else if(!type_is_user_def(parent->type))
    {
        sema_error(c, &uaccess->member_expr->loc, "Attempted to access member of incompatable type \'%s\'.",
                   type_to_string(parent->type));
        return false;
    }

    Object* member = resolve_member(c, parent->type, &uaccess->member_expr->loc);
    
    if(member == NULL)
        return false;

    expr->kind = EXPR_MEMBER_ACCESS;
    expr->expr.member_access.parent_expr = parent;
    expr->expr.member_access.member = member;
    expr->type = member->var.type;
    return true;
}

static bool analyze_add(SemaContext* c, ASTExpr* expr, ASTExpr* left, ASTExpr* right)
{
    bool left_is_pointer = type_is_pointer(left->type) || left->type->kind == TYPE_SS_ARRAY;
    bool right_is_pointer = type_is_pointer(right->type) || right->type->kind == TYPE_SS_ARRAY;
    if(right_is_pointer)
    {
        left_is_pointer = true;
        expr->expr.binary.lhs = right;
        expr->expr.binary.rhs = left;
        left = right;
        right = expr->expr.binary.lhs;
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

    if(!arith_type_conv(c, expr, left, right))
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

    if(!arith_type_conv(c, expr, left, right))
        return false;

    expr->type = left->type;
    return true;
}

static bool analyze_mul(SemaContext* c, ASTExpr* expr, ASTExpr* left, ASTExpr* right)
{
    if(!arith_type_conv(c, expr, left, right))
        return false;

    expr->type = left->type;
    return true;
}

static bool analyze_div(SemaContext* c, ASTExpr* expr, ASTExpr* left, ASTExpr* right)
{
    if(!arith_type_conv(c, expr, left, right))
        return false;

    expr->type = left->type;
    return true;
}

static bool analyze_logical(SemaContext* c, ASTExpr* expr, ASTExpr* left, ASTExpr* right)
{
    if(!implicit_cast(c, left, g_type_bool) || !implicit_cast(c, right, g_type_bool))
        return false;
    expr->type = g_type_bool;
    return true;
}

static bool analyze_comparison(SemaContext* c, ASTExpr* expr, ASTExpr* left, ASTExpr* right)
{
    expr->type = g_type_bool;
    bool left_is_pointer = type_is_pointer(left->type);
    if(type_is_pointer(right->type) && !left_is_pointer)
    {
        left_is_pointer = true;
        ASTExpr* temp = left;
        left = right;
        right = temp;
    }
    if(left_is_pointer)
        return implicit_cast(c, right, left->type);

    if(!type_is_numeric(left->type) || !type_is_numeric(right->type))
    {
        sema_error(c, &expr->loc, "Invalid operand types %s and %s.",
                   type_to_string(left->type), type_to_string(right->type));
        return false;
    }

    return arith_type_conv(c, expr, left, right);
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
    if(!type_is_integer(left->type) || !type_is_integer(right->type))
    {
        sema_error(c, &expr->loc, "Invalid operand types %s and %s.",
                   type_to_string(left->type), type_to_string(right->type));
        return false;
    }

    promote_int_type(c, left);
    promote_int_type(c, right);
    expr->type = left->type;
    return true;

}

static bool analyze_assign(SemaContext* c, ASTExpr* expr, ASTExpr* left, ASTExpr* right)
{
    if(!expr_is_lvalue(c, left))
        return false;
    expr->type = left->type;
    return implicit_cast(c, right, left->type);
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

static bool analyze_addr_of(SemaContext* c, ASTExpr* expr, ASTExpr* inner)
{
    if(inner->kind != EXPR_IDENT)
    {
        sema_error(c, &expr->loc, "Cannot take address of rvalue.");
        return false;
    }
    expr->type = type_pointer_to(inner->type);
    return true;
}

static bool analyze_bit_not(SemaContext* c, ASTExpr* expr, ASTExpr* inner)
{
    if(!type_is_integer(inner->type))
    {
        sema_error(c, &expr->loc, "Invalid operand type %s.", 
                   type_to_string(inner->type));
        return false;
    }

    promote_int_type(c, inner);
    expr->type = inner->type;
    return true;
}

static bool analyze_deref(SemaContext* c, ASTExpr* expr, ASTExpr* inner)
{
    switch(inner->type->kind)
    {
    case TYPE_POINTER:
        expr->type = inner->type->pointer_base;
        return true;
    case TYPE_SS_ARRAY:
    case TYPE_DS_ARRAY:
        expr->type = inner->type->array.elem_type;
        return true;
    default:
        sema_error(c, &expr->loc, "Cannot dereference non-pointer type %s.",
                   type_to_string(inner->type));
        return false;
    }
}

static bool analyze_incdec(SemaContext* c, ASTExpr* expr, ASTExpr* inner)
{
    if(!expr_is_lvalue(c, inner))
        return false;

    expr->type = inner->type;
    if(!type_is_integer(expr->type))
        SIC_TODO_MSG("Remove temporary");
    return true;
}

static bool analyze_log_not(SemaContext* c, ASTExpr* expr, ASTExpr* inner)
{
    expr->type = g_type_bool;
    return implicit_cast(c, inner, g_type_bool);
}

static bool analyze_negate(SemaContext* c, ASTExpr* expr, ASTExpr* inner)
{
    if(!type_is_numeric(inner->type))
    {
        sema_error(c, &expr->loc, "Cannot negate non-numeric type %s.",
                   type_to_string(inner->type));
        return false;
    }

    expr->type = inner->type;

    if(inner->kind == EXPR_CONSTANT)
    {
        expr->kind = EXPR_CONSTANT;
        if(inner->expr.constant.kind == CONSTANT_FLOAT)
        {
            expr->expr.constant.val.f = -inner->expr.constant.val.f;
            expr->expr.constant.kind = CONSTANT_FLOAT;
            return true;
        }

        expr->expr.constant.val.i = -inner->expr.constant.val.i;
        expr->expr.constant.kind = CONSTANT_INTEGER;
        switch(inner->type->kind)
        {
        case TYPE_UBYTE:
            expr->type = inner->expr.constant.val.i > 0x7F ? g_type_short : g_type_byte;
            return true;
        case TYPE_USHORT:
            expr->type = inner->expr.constant.val.i > 0x7FFF ? g_type_int : g_type_short;
            return true;
        case TYPE_UINT:
            expr->type = inner->expr.constant.val.i > 0x7FFFFFFF ? g_type_long : g_type_int;
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

    return true;
}


static bool arith_type_conv(SemaContext* c, ASTExpr* parent, ASTExpr* e1, ASTExpr* e2)
{
    if(!type_is_numeric(e1->type) || !type_is_numeric(e2->type))
    {
        sema_error(c, &parent->loc, "Invalid operand types %s and %s.",
                   type_to_string(e1->type), type_to_string(e2->type));
        return false;
    }

    if(e1->type->kind < e2->type->kind)
    {
        ASTExpr* temp = e1;
        e1 = e2;
        e2 = temp;
    }

    if(type_size(e1->type) >= 4)
    {
        if(!implicit_cast(c, e2, e1->type))
            SIC_UNREACHABLE();
        return true;
    }

    promote_int_type(c, e1);
    promote_int_type(c, e2);
    SIC_ASSERT(e1->type->kind == e2->type->kind);
    return true;
}

static void promote_int_type(SemaContext* c, ASTExpr* expr)
{
    SIC_ASSERT(type_is_integer(expr->type));
    if(type_size(expr->type) < 4 && !implicit_cast(c, expr, g_type_int))
        SIC_UNREACHABLE();
}
