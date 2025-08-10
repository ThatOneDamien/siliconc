#include "semantics.h"

#define ANALYZE_EXPR_SET_VALID(expr) do { valid &= analyze_expr(c, expr); } while(0)
#define ANALYZE_EXPR_OR_RET(expr) do { if(!analyze_expr(c, expr)) return false; } while(0)
#define IF_INVALID_RET() do { if(!valid) return false; } while(0)

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

bool analyze_expr_no_set(SemaContext* c, ASTExpr* expr)
{
    switch(expr->kind)
    {
    case EXPR_ARRAY_ACCESS:
        return analyze_array_access(c, expr);
    case EXPR_BINARY:
        return analyze_binary(c, expr);
    case EXPR_CAST:
        return analyze_cast(c, expr);
    case EXPR_DEFAULT:
        SIC_ERROR_DBG("here");
        sic_error_at(expr->loc, "Keyword \'default\' not allowed in this context.");
        return false;
    case EXPR_FUNC_CALL:
        return analyze_call(c, expr);
    case EXPR_POSTFIX: {
        ASTExpr* inner = expr->expr.unary.inner;
        return analyze_expr(c, inner) && analyze_incdec(c, expr, inner);
    }
    case EXPR_PRE_SEMANTIC_IDENT: {
        Symbol sym = expr->expr.pre_sema_ident;
        ASTExprIdent ident = find_obj(c, sym);
        if(ident == NULL)
        {
            sic_error_at(expr->loc, "Reference to undefined symbol \'%s\'.", sym);
            return false;
        }
        expr->expr.ident = ident;
        expr->type = ident->var.type;
        expr->kind = EXPR_IDENT;
        return true;
    }
    case EXPR_UNARY:
        return analyze_unary(c, expr);
    case EXPR_UNRESOLVED_ARR:
        return analyze_unresolved_arrow(c, expr);
    case EXPR_UNRESOLVED_DOT:
        return analyze_unresolved_dot(c, expr);
    case EXPR_CONSTANT:
        return true;
    case EXPR_INVALID:
        return false;
    case EXPR_TERNARY:
        return analyze_ternary(c, expr);
    case EXPR_IDENT:
    case EXPR_MEMBER_ACCESS:
    case EXPR_NOP:
        break;
    }
    SIC_UNREACHABLE();
}

bool resolve_default(ASTExpr* expr, Type* type)
{
    expr->kind = EXPR_CONSTANT;
    expr->type = type;
    switch(type->kind)
    {
    case TYPE_BOOL:
    case TYPE_BYTE:
    case TYPE_UBYTE:
    case TYPE_SHORT:
    case TYPE_USHORT:
    case TYPE_INT:
    case TYPE_UINT:
    case TYPE_LONG:
    case TYPE_ULONG:
    case TYPE_POINTER:
        expr->expr.constant.kind = CONSTANT_INTEGER;
        expr->expr.constant.val.i = 0;
        return true;
    case TYPE_FLOAT:
    case TYPE_DOUBLE:
        expr->expr.constant.kind = CONSTANT_FLOAT;
        expr->expr.constant.val.i = 0;
        return true;
    case TYPE_SS_ARRAY:
    case TYPE_DS_ARRAY:
    case TYPE_ENUM:
    case TYPE_STRUCT:
    case TYPE_TYPEDEF:
    case TYPE_UNION:
        SIC_TODO();
    case TYPE_INVALID:
    case TYPE_VOID:
    case TYPE_NULLPTR:
    case TYPE_PRE_SEMA_ARRAY:
    case __TYPE_COUNT:
        SIC_UNREACHABLE();
    }
    return false;
}

static bool analyze_array_access(SemaContext* c, ASTExpr* expr)
{
    bool valid = true;
    ASTExpr* arr = expr->expr.array_access.array_expr;
    ASTExpr* index = expr->expr.array_access.index_expr;
    ANALYZE_EXPR_SET_VALID(arr);
    ANALYZE_EXPR_SET_VALID(index);
    IF_INVALID_RET();

    if(!type_is_array(arr->type) && !type_is_pointer(arr->type))
    {
        sic_error_at(expr->loc, "Attempted to access element of non-array and non-pointer type \'%s\'",
                   type_to_string(arr->type));
        return false;
    }

    if(arr->type->kind == TYPE_SS_ARRAY && index->kind == EXPR_CONSTANT && 
       index->expr.constant.val.i >= arr->type->array.ss_size)
    {
        sic_diagnostic_at(expr->loc, DIAG_WARNING,
                          "Array index will overflow the size of the array.");
        return false;
    }

    expr->type = type_pointer_base(expr->expr.array_access.array_expr->type);
    return true;
}

static bool analyze_binary(SemaContext* c, ASTExpr* expr)
{
    bool valid = true;
    ASTExpr* left = expr->expr.binary.lhs;
    ASTExpr* right = expr->expr.binary.rhs;
    BinaryOpKind kind = expr->expr.binary.kind;
    ANALYZE_EXPR_SET_VALID(left);
    if(kind != BINARY_ASSIGN || right->kind != EXPR_DEFAULT)
        ANALYZE_EXPR_SET_VALID(right);
    IF_INVALID_RET();

    switch(kind)
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
    ANALYZE_EXPR_OR_RET(call->func_expr);

    if(call->func_expr->kind != EXPR_IDENT || call->func_expr->expr.ident->kind != OBJ_FUNC)
    {
        SIC_TODO_MSG("Handle complex function calling");
        sic_error_at(expr->loc, "Identifier in call expression is not a function.");
        return false;
    }

    Object* func = call->func_expr->expr.ident;
    FuncSignature* sig = func->func.signature;
    if(call->args.size < sig->params.size)
    {
        sic_error_at(expr->loc, 
                   "Too few arguments passed to function. Expected %s%u, have %u.",
                   sig->is_var_arg ? "at least " : "", sig->params.size, call->args.size);
        return false;
    }
    if(!sig->is_var_arg && call->args.size > sig->params.size)
    {
        sic_error_at(expr->loc, 
                   "Too many arguments passed to function. Expected %u, have %u.",
                   sig->params.size, call->args.size);
        return false;
    }

    bool valid = true;
    for(size_t i = 0; i < sig->params.size; ++i)
    {
        ASTExpr* arg = call->args.data[i];
        analyze_expr(c, arg);
        valid = valid && !expr_is_bad(arg) &&
                implicit_cast(c, arg, sig->params.data[i]->var.type);
    }

    for(size_t i = sig->params.size; i < call->args.size; ++i)
    {
        ASTExpr* arg = call->args.data[i];
        analyze_expr(c, arg);
        if(expr_is_bad(arg))
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
    bool valid = true;
    ASTExpr* cond = expr->expr.ternary.cond_expr;
    ASTExpr* then = expr->expr.ternary.then_expr;
    ASTExpr* elss = expr->expr.ternary.else_expr;
    ANALYZE_EXPR_SET_VALID(cond);
    if(then != NULL)
        ANALYZE_EXPR_SET_VALID(then);
    else
        then = cond;
    ANALYZE_EXPR_SET_VALID(elss);
    IF_INVALID_RET();

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
    ANALYZE_EXPR_OR_RET(inner);
    
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

static Object* resolve_member(Type* type, ASTExprUAccess* access)
{
    ObjectDA* members = &type->user_def->struct_.members;
    for(size_t i = 0; i < members->size; ++i)
        if(members->data[i]->symbol == access->member_sym)
            return members->data[i];
    sic_error_at(access->member_loc, "Type \'%s\' has no member \'%s\'.",
               type_to_string(type), access->member_sym);
    return NULL;
}

static bool analyze_unresolved_arrow(SemaContext* c, ASTExpr* expr)
{
    ASTExprUAccess* uaccess = &expr->expr.unresolved_access;
    ASTExpr* parent = uaccess->parent_expr;
    ANALYZE_EXPR_OR_RET(parent);

    if(parent->type->kind != TYPE_POINTER || !type_is_user_def(parent->type->pointer_base))
    {
        sic_error_at(expr->loc, "Arrow operator can only be used on pointers to structure-like types.");
        return false;
    }

    ASTExpr* deref = CALLOC_STRUCT(ASTExpr);
    deref->type = parent->type->pointer_base;
    deref->loc = expr->loc;
    deref->kind = EXPR_UNARY;
    deref->expr.unary.kind = UNARY_DEREF;
    deref->expr.unary.inner = parent;
    Object* member = resolve_member(deref->type, uaccess);
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
    ANALYZE_EXPR_OR_RET(parent);

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
        sic_error_at(uaccess->member_loc, "Attempted to access member of non-structure type \'%s\'.",
                   type_to_string(parent->type));
        return false;
    }

    Object* member = resolve_member(parent->type, uaccess);
    
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
            sic_error_at(expr->loc, "Invalid operand types %s and %s.",
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
            sic_error_at(expr->loc, "Array index will overflow the size of the array.");
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
            sic_error_at(expr->loc, "Invalid operand types %s and %s.",
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
        sic_error_at(expr->loc, "Invalid operand types %s and %s.",
                   type_to_string(left->type), type_to_string(right->type));
        return false;
    }

    return arith_type_conv(c, expr, left, right);
}

static bool analyze_shift(SemaContext* c, ASTExpr* expr, ASTExpr* left, ASTExpr* right)
{
    if(!type_is_integer(left->type) || !type_is_integer(right->type))
    {
        sic_error_at(expr->loc, "Invalid operand types %s and %s.",
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
        sic_error_at(expr->loc, "Invalid operand types %s and %s.",
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
    if(!expr_is_lvalue(left))
        return false;
    expr->type = left->type;
    if(right->kind == EXPR_DEFAULT && !resolve_default(right, left->type))
       return false;

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
    // TODO: Make this deep copy most likely, I feel like there is a better way to do this.
    new_expr->expr.binary.lhs = MALLOC_STRUCT(ASTExpr);
    memcpy(new_expr->expr.binary.lhs, left, sizeof(ASTExpr));
    new_expr->expr.binary.rhs = right;
    expr->expr.binary.kind = BINARY_ASSIGN;
    expr->expr.binary.rhs = new_expr;
    analyze_binary(c, new_expr);
    return !expr_is_bad(new_expr) && analyze_assign(c, expr, left, new_expr);
}

static bool analyze_addr_of(SemaContext* c, ASTExpr* expr, ASTExpr* inner)
{
    (void)c;
    if(inner->kind != EXPR_IDENT)
    {
        sic_error_at(expr->loc, "Cannot take address of rvalue.");
        return false;
    }
    expr->type = type_pointer_to(inner->type);
    return true;
}

static bool analyze_bit_not(SemaContext* c, ASTExpr* expr, ASTExpr* inner)
{
    if(!type_is_integer(inner->type))
    {
        sic_error_at(expr->loc, "Invalid operand type %s.", 
                   type_to_string(inner->type));
        return false;
    }

    promote_int_type(c, inner);
    expr->type = inner->type;
    return true;
}

static bool analyze_deref(SemaContext* c, ASTExpr* expr, ASTExpr* inner)
{
    (void)c;
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
        sic_error_at(expr->loc, "Cannot dereference non-pointer type %s.",
                   type_to_string(inner->type));
        return false;
    }
}

static bool analyze_incdec(SemaContext* c, ASTExpr* expr, ASTExpr* inner)
{
    (void)c;
    if(!expr_is_lvalue(inner))
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
    (void)c;
    if(!type_is_numeric(inner->type))
    {
        sic_error_at(expr->loc, "Cannot negate non-numeric type %s.",
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
        sic_error_at(parent->loc, "Invalid operand types %s and %s.",
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
