#include "semantics.h"

#define ANALYZE_EXPR_SET_VALID(expr) do { valid &= analyze_expr(expr); } while(0)
#define ANALYZE_EXPR_OR_RET(expr) do { if(!analyze_expr(expr)) return false; } while(0)
#define IF_INVALID_RET() do { if(!valid) return false; } while(0)
#define ERROR_AND_RET(ret_val, loc, ...) do { sic_error_at(loc, __VA_ARGS__); return ret_val; } while(0)

// Expr kind functions
static bool analyze_array_access(ASTExpr* expr);
static bool analyze_binary(ASTExpr* expr);
static bool analyze_call(ASTExpr* expr);
static bool analyze_ident(ASTExpr** expr_ref);
static bool analyze_ternary(ASTExpr* expr);
static bool analyze_unary(ASTExpr** expr_ref);
static bool analyze_unresolved_arrow(ASTExpr* expr);
static bool analyze_unresolved_dot(ASTExpr* expr);
static bool analyze_ct_sizeof(ASTExpr* expr);

// Binary functions
static bool analyze_add(ASTExpr* expr, ASTExpr** lhs, ASTExpr** rhs);
static bool analyze_sub(ASTExpr* expr, ASTExpr** lhs, ASTExpr** rhs);
static bool analyze_mul(ASTExpr* expr, ASTExpr** lhs, ASTExpr** rhs);
static bool analyze_div(ASTExpr* expr, ASTExpr** lhs, ASTExpr** rhs);
static bool analyze_logical(ASTExpr* expr, ASTExpr** lhs, ASTExpr** rhs);
static bool analyze_comparison(ASTExpr* expr, ASTExpr** lhs, ASTExpr** rhs);
static bool analyze_shift(ASTExpr* expr, ASTExpr** lhs, ASTExpr** rhs);
static bool analyze_bit_op(ASTExpr* expr, ASTExpr** lhs, ASTExpr** rhs);
static bool analyze_assign(ASTExpr* expr, ASTExpr** lhs, ASTExpr** rhs);
static bool analyze_op_assign(ASTExpr* expr, ASTExpr** lhs, ASTExpr** rhs);

// Unary functions
static bool analyze_addr_of(ASTExpr** expr_ref, ASTExpr* inner);
static bool analyze_bit_not(ASTExpr* expr, ASTExpr** inner);
static bool analyze_deref(ASTExpr** expr_ref, ASTExpr* inner);
static bool analyze_incdec(ASTExpr* expr, ASTExpr* inner);
static bool analyze_log_not(ASTExpr* expr, ASTExpr* inner);
static bool analyze_negate(ASTExpr* expr, ASTExpr* inner);

static bool arith_type_conv(ASTExpr* parent, ASTExpr** e1, ASTExpr** e2);
static void promote_int_type(ASTExpr** expr);

bool analyze_expr_no_set(ASTExpr** expr_ref)
{
    ASTExpr* expr = *expr_ref;
    if(expr->evaluated)
        return expr->kind != EXPR_INVALID;
    expr->evaluated = true;
    switch(expr->kind)
    {
    case EXPR_ARRAY_ACCESS:
        return analyze_array_access(expr);
    case EXPR_BINARY:
        return analyze_binary(expr);
    case EXPR_CAST:
        return analyze_cast(expr);
    case EXPR_DEFAULT:
        ERROR_AND_RET(false, expr->loc, "Keyword \'default\' not allowed in this context.");
    case EXPR_FUNC_CALL:
        return analyze_call(expr);
    case EXPR_INITIALIZER_LIST:
        ERROR_AND_RET(false, expr->loc, "Initializer list not allowed in this context.");
    case EXPR_POSTFIX:
        return analyze_expr(&expr->expr.unary.inner) && 
               analyze_incdec(expr, expr->expr.unary.inner);
    case EXPR_PRE_SEMANTIC_IDENT:
        return analyze_ident(expr_ref);
    case EXPR_UNARY:
        return analyze_unary(expr_ref);
    case EXPR_UNRESOLVED_ARR:
        return analyze_unresolved_arrow(expr);
    case EXPR_UNRESOLVED_DOT:
        return analyze_unresolved_dot(expr);
    case EXPR_CONSTANT:
        return resolve_type(&expr->type, RES_NORMAL, expr->loc, "Constant cannot be of type");
    case EXPR_INVALID:
        return false;
    case EXPR_TERNARY:
        return analyze_ternary(expr);
    case EXPR_CT_SIZEOF:
        return analyze_ct_sizeof(expr);
    case EXPR_IDENT:
    case EXPR_MEMBER_ACCESS:
    case EXPR_TYPE_IDENT:
        break;
    }
    SIC_UNREACHABLE();
}

static bool analyze_array_access(ASTExpr* expr)
{
    bool valid = true;
    g_sema.ident_mask = IDENT_VAR_ONLY;
    ANALYZE_EXPR_SET_VALID(&expr->expr.array_access.array_expr);
    g_sema.ident_mask = IDENT_VAR_ONLY;
    ANALYZE_EXPR_SET_VALID(&expr->expr.array_access.index_expr);
    IF_INVALID_RET();
    ASTExpr* arr = expr->expr.array_access.array_expr;
    ASTExpr* index = expr->expr.array_access.index_expr;

    if(!type_is_array(arr->type) && !type_is_pointer(arr->type))
    {
        ERROR_AND_RET(false, expr->loc, 
                      "Attempted to access element of non-array and non-pointer type \'%s\'",
                      type_to_string(arr->type));
    }

    if(arr->type->kind == TYPE_STATIC_ARRAY && index->kind == EXPR_CONSTANT && 
       index->expr.constant.val.i >= arr->type->array.ss_size)
    {
        sic_diagnostic_at(expr->loc, DIAG_WARNING,
                          "Array index will overflow the size of the array.");
        return false;
    }

    expr->type = type_pointer_base(arr->type);
    return true;
}

static inline bool analyze_binary(ASTExpr* expr)
{
    bool valid = true;
    ASTExpr** lhs = &expr->expr.binary.lhs;
    ASTExpr** rhs = &expr->expr.binary.rhs;
    BinaryOpKind kind = expr->expr.binary.kind;
    g_sema.ident_mask = IDENT_VAR_ONLY;
    ANALYZE_EXPR_SET_VALID(lhs);
    if(kind != BINARY_ASSIGN)
    {
        g_sema.ident_mask = IDENT_VAR_ONLY;
        ANALYZE_EXPR_SET_VALID(rhs);
    }
    IF_INVALID_RET();
    switch(expr->expr.binary.kind)
    {
    case BINARY_ADD:
        return analyze_add(expr, lhs, rhs);
    case BINARY_SUB:
        return analyze_sub(expr, lhs, rhs);
    case BINARY_MUL:
        return analyze_mul(expr, lhs, rhs);
    case BINARY_DIV:
        return analyze_div(expr, lhs, rhs);
    case BINARY_MOD:
        return analyze_div(expr, lhs, rhs);
    case BINARY_LOG_OR:
    case BINARY_LOG_AND:
        return analyze_logical(expr, lhs, rhs);
    case BINARY_EQ:
    case BINARY_NE:
    case BINARY_LT:
    case BINARY_LE:
    case BINARY_GT:
    case BINARY_GE:
        return analyze_comparison(expr, lhs, rhs);
    case BINARY_SHL:
    case BINARY_LSHR:
    case BINARY_ASHR:
        return analyze_shift(expr, lhs, rhs);
    case BINARY_BIT_OR:
    case BINARY_BIT_XOR:
    case BINARY_BIT_AND:
        return analyze_bit_op(expr, lhs, rhs);
    case BINARY_ASSIGN:
        return analyze_assign(expr, lhs, rhs);
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
        return analyze_op_assign(expr, lhs, rhs);
    case BINARY_INVALID:
        break;
    }
    SIC_UNREACHABLE();
}

static bool analyze_call(ASTExpr* expr)
{
    ASTExprCall* call = &expr->expr.call;
    g_sema.ident_mask = IDENT_VAR_ONLY | IDENT_FUNC;
    ANALYZE_EXPR_OR_RET(&call->func_expr);
    
    Type* func_type = call->func_expr->type;
    if(func_type->kind != TYPE_FUNC_PTR)
        ERROR_AND_RET(false, expr->loc, "Attempted to call non-function.");

    FuncSignature* sig = func_type->func_ptr;
    if(call->args.size < sig->params.size)
    {
        ERROR_AND_RET(false, expr->loc, 
                      "Too few arguments passed to function. Expected %s%u, have %u.",
                      sig->is_var_arg ? "at least " : "", sig->params.size, call->args.size);
    }
    if(!sig->is_var_arg && call->args.size > sig->params.size)
    {
        ERROR_AND_RET(false, expr->loc, 
                      "Too many arguments passed to function. Expected %u, have %u.",
                      sig->params.size, call->args.size);
    }

    bool valid = true;
    for(uint32_t i = 0; i < sig->params.size; ++i)
    {
        Object* param = sig->params.data[i];
        if(!implicit_cast(call->args.data + i, param->type))
        {
            valid = false;
            continue;
        }
    }

    for(uint32_t i = sig->params.size; i < call->args.size; ++i)
    {
        ASTExpr** arg = call->args.data + i;
        if(!analyze_expr(arg))
        {
            valid = false;
            continue;
        }
        implicit_cast_varargs(arg);
    }


    expr->type = sig->ret_type;

    return valid;
}

static bool analyze_ident(ASTExpr** expr_ref)
{
    ASTExpr* expr = *expr_ref;
    Symbol sym = expr->expr.pre_sema_ident;
    Object* ident = find_obj(sym);
    if(ident == NULL)
        ERROR_AND_RET(false, expr->loc, "Reference to undefined symbol \'%s\'.", sym);

    switch(ident->kind)
    {
    case OBJ_ALIAS_EXPR:
        SIC_TODO();
    case OBJ_ENUM_VALUE:
        expr->type = ident->type;
        expr->kind = EXPR_CONSTANT;
        expr->expr.constant.kind = CONSTANT_INTEGER;
        expr->expr.constant.val.i = ident->enum_val.const_val;
        return true;
    case OBJ_FUNC:
        if(BIT_IS_UNSET(g_sema.ident_mask, IDENT_FUNC))
            ERROR_AND_RET(false, expr->loc, "Function identifier not allowed here, if you want the pointer use '&' before.");
        expr->type = ident->type;
        expr->kind = EXPR_IDENT;
        expr->expr.ident = ident;
        return true;
    case OBJ_BITFIELD:
    case OBJ_ENUM:
        if(BIT_IS_UNSET(g_sema.ident_mask, IDENT_ENUM))
            ERROR_AND_RET(false, expr->loc, "Type identifier not allowed in this context, expected expression.");
        expr->type = g_type_invalid;
        expr->kind = EXPR_TYPE_IDENT;
        expr->expr.ident = ident;
        return true;
    case OBJ_STRUCT:
    case OBJ_TYPE_ALIAS:
    case OBJ_TYPE_DISTINCT:
    case OBJ_UNION:
        ERROR_AND_RET(false, expr->loc, "Type identifier not an enum.");
    case OBJ_VAR:
        if(ident->var.kind == VAR_GLOBAL)
        {
            analyze_global_var(ident);
            if(ident->kind == OBJ_INVALID)
                return false;
        }

        // if(ident->attribs & ATTR_CONST)
        // {
        //     *expr_ref = ident->var.initial_val;
        //     return true;
        // }

        expr->type = ident->type;
        expr->kind = EXPR_IDENT;
        expr->expr.ident = ident;
        return true;
    case OBJ_INVALID:
        return false;
    }
    SIC_UNREACHABLE();
}

static bool analyze_ternary(ASTExpr* expr)
{
    ASTExprTernary* tern = &expr->expr.ternary;
    bool valid = true;
    g_sema.ident_mask = IDENT_VAR_ONLY;
    ANALYZE_EXPR_SET_VALID(&tern->cond_expr);
    if(tern->then_expr != NULL)
    {
        g_sema.ident_mask = IDENT_VAR_ONLY;
        ANALYZE_EXPR_SET_VALID(&tern->then_expr);
    }
    else
        tern->then_expr = tern->cond_expr;
    g_sema.ident_mask = IDENT_VAR_ONLY;
    ANALYZE_EXPR_SET_VALID(&tern->else_expr);
    IF_INVALID_RET();

    if(type_equal(tern->then_expr->type, tern->else_expr->type))
        goto EXIT;

    if(!arith_type_conv(expr, &tern->then_expr, &tern->else_expr))
        return false;
EXIT:
    expr->type = tern->else_expr->type;
    return implicit_cast(&tern->cond_expr, g_type_bool);
}

static bool analyze_unary(ASTExpr** expr_ref)
{
    ASTExpr* expr = *expr_ref;
    ASTExprUnary* unary = &expr->expr.unary;
    g_sema.ident_mask = unary->kind == UNARY_ADDR_OF ? 
                        IDENT_FUNC :
                        IDENT_VAR_ONLY;
    ANALYZE_EXPR_OR_RET(&unary->inner);
    ASTExpr* inner = unary->inner;
    
    switch(unary->kind)
    {
    case UNARY_INVALID:
        break;
    case UNARY_ADDR_OF:
        return analyze_addr_of(expr_ref, inner);
    case UNARY_BIT_NOT:
        return analyze_bit_not(expr, &unary->inner);
    case UNARY_DEC:
    case UNARY_INC:
        return analyze_incdec(expr, inner);
    case UNARY_DEREF:
        return analyze_deref(expr_ref, inner);
    case UNARY_LOG_NOT:
        return analyze_log_not(expr, inner);
    case UNARY_NEG:
        return analyze_negate(expr, inner);
    }
    SIC_UNREACHABLE();
}

static bool resolve_member(ASTExpr* expr, ASTExpr* parent)
{
    ASTExprUAccess* uaccess = &expr->expr.unresolved_access;
    Symbol member = uaccess->member_sym;
    SIC_ASSERT(parent->type->status == STATUS_RESOLVED);
    switch(parent->type->kind)
    {
    case TYPE_STATIC_ARRAY:
        if(member == g_sym_len)
        {
            expr->kind = EXPR_CONSTANT;
            expr->expr.constant.kind = CONSTANT_INTEGER;
            expr->expr.constant.val.i = parent->type->array.ss_size;
            expr->type = g_type_ulong;
            return true;
        }
        break;
    case TYPE_RUNTIME_ARRAY:
    case TYPE_ENUM:
        SIC_TODO();
    case TYPE_STRUCT:
    case TYPE_UNION: {
        ObjectDA* members = &parent->type->user_def->struct_.members;
        for(uint32_t i = 0; i < members->size; ++i)
            if(members->data[i]->symbol == member)
            {
                expr->kind = EXPR_MEMBER_ACCESS;
                expr->expr.member_access.parent_expr = parent;
                expr->expr.member_access.member = members->data[i];
                expr->expr.member_access.member_idx = i;
                expr->type = members->data[i]->type;
                return true;
            }
        break;
    }
    case TYPE_VOID:
    case TYPE_BOOL:
    case TYPE_BYTE:
    case TYPE_UBYTE:
    case TYPE_SHORT:
    case TYPE_USHORT:
    case TYPE_INT:
    case TYPE_UINT:
    case TYPE_LONG:
    case TYPE_ULONG:
    case TYPE_FLOAT:
    case TYPE_DOUBLE:
    case TYPE_POINTER:
    case TYPE_FUNC_PTR:
        break;
    case TYPE_INVALID:
    case TYPE_TYPEDEF:
    case TYPE_PRE_SEMA_ARRAY:
    case TYPE_PRE_SEMA_USER:
    case TYPE_AUTO:
    case TYPE_TYPEOF:
    case __TYPE_COUNT:
        SIC_UNREACHABLE();
    }
    sic_error_at(uaccess->member_loc, "Type \'%s\' has no member \'%s\'.",
                 type_to_string(parent->type), member);
    return NULL;
}

static bool analyze_unresolved_arrow(ASTExpr* expr)
{
    ASTExprUAccess* uaccess = &expr->expr.unresolved_access;
    g_sema.ident_mask = IDENT_VAR_ONLY;
    ANALYZE_EXPR_OR_RET(&uaccess->parent_expr);
    ASTExpr* parent = uaccess->parent_expr;

    if(parent->type->kind != TYPE_POINTER)
    {
        sic_error_at(expr->loc, "Arrow operator can only be used on pointers.");
        return false;
    }

    ASTExpr* deref = CALLOC_STRUCT(ASTExpr);
    deref->type = parent->type->pointer_base;
    deref->loc = expr->loc;
    deref->kind = EXPR_UNARY;
    deref->expr.unary.kind = UNARY_DEREF;
    deref->expr.unary.inner = uaccess->parent_expr;
    return resolve_member(expr, deref);
}

static bool analyze_unresolved_dot(ASTExpr* expr)
{
    ASTExprUAccess* uaccess = &expr->expr.unresolved_access;
    g_sema.ident_mask = IDENT_ENUM;
    ANALYZE_EXPR_OR_RET(&uaccess->parent_expr);
    ASTExpr* parent = uaccess->parent_expr;

    if(parent->kind == EXPR_TYPE_IDENT)
    {
        ObjectDA* values = &parent->expr.ident->enum_.values;
        for(uint32_t i = 0; i < values->size; ++i)
            if(values->data[i]->symbol == uaccess->member_sym)
            {
                expr->kind = EXPR_CONSTANT;
                expr->type = CALLOC_STRUCT(Type);
                expr->type->kind = TYPE_ENUM;
                expr->type->user_def = parent->expr.ident;
                expr->type->status = STATUS_RESOLVED;
                expr->expr.constant.kind = CONSTANT_INTEGER;
                expr->expr.constant.val.i = values->data[i]->enum_val.const_val;
                return true;
            }
        sic_error_at(uaccess->member_loc, "Enum \'%s\' has no value \'%s\'.",
                     parent->expr.ident->symbol, uaccess->member_sym);
        return false;
    }

    if(parent->type->kind == TYPE_POINTER)
    {
        ASTExpr* deref = CALLOC_STRUCT(ASTExpr);
        deref->type = parent->type->pointer_base;
        deref->loc = expr->loc;
        deref->kind = EXPR_UNARY;
        deref->expr.unary.kind = UNARY_DEREF;
        deref->expr.unary.inner = parent;
        parent = deref;
    }

    return resolve_member(expr, parent);
}

static bool analyze_ct_sizeof(ASTExpr* expr)
{
    if(!resolve_type(&expr->expr.ct_sizeof_type, RES_NORMAL, expr->loc, "Cannot take get size of type"))
        return false;
    
    uint32_t size = type_size(expr->expr.ct_sizeof_type);
    expr->kind = EXPR_CONSTANT;
    expr->expr.constant.kind = CONSTANT_INTEGER;
    expr->expr.constant.val.i = size;
    return true;
}

static bool analyze_add(ASTExpr* expr, ASTExpr** lhs, ASTExpr** rhs)
{
    ASTExpr* left = *lhs;
    ASTExpr* right = *rhs;
    bool left_is_pointer = type_is_pointer(left->type) || left->type->kind == TYPE_STATIC_ARRAY;
    bool right_is_pointer = type_is_pointer(right->type) || right->type->kind == TYPE_STATIC_ARRAY;
    if(right_is_pointer)
    {
        left_is_pointer = true;
        *lhs = right;
        *rhs = left;
        left = right;
        right = *rhs;
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
        if(left->type->kind == TYPE_STATIC_ARRAY && right->kind == EXPR_CONSTANT && 
           right->expr.constant.val.i >= left->type->array.ss_size)
        {
            sic_diagnostic_at(expr->loc, DIAG_WARNING,
                              "Array index will overflow the size of the array.");
            return false;
        }
        expr->type = left->type;
        return true;
    }

    if(!arith_type_conv(expr, lhs, rhs))
        return false;

    left = *lhs;
    right = *rhs;

    if(left->kind == EXPR_CONSTANT && right->kind == EXPR_CONSTANT)
    {
        expr->kind = EXPR_CONSTANT;
    }

    expr->type = left->type;
    return true;
}

static bool analyze_sub(ASTExpr* expr, ASTExpr** lhs, ASTExpr** rhs)
{
    ASTExpr* left = *lhs;
    ASTExpr* right = *rhs;
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

    if(!arith_type_conv(expr, lhs, rhs))
        return false;

    left = *lhs;
    right = *rhs;

    expr->type = left->type;
    return true;
}

static bool analyze_mul(ASTExpr* expr, ASTExpr** lhs, ASTExpr** rhs)
{
    if(!arith_type_conv(expr, lhs, rhs))
        return false;

    expr->type = (*lhs)->type;
    return true;
}

static bool analyze_div(ASTExpr* expr, ASTExpr** lhs, ASTExpr** rhs)
{
    if(!arith_type_conv(expr, lhs, rhs))
        return false;

    expr->type = (*lhs)->type;
    return true;
}

static bool analyze_logical(ASTExpr* expr, ASTExpr** lhs, ASTExpr** rhs)
{
    if(!implicit_cast(lhs, g_type_bool) || !implicit_cast(rhs, g_type_bool))
        return false;
    expr->type = g_type_bool;
    return true;
}

static bool analyze_comparison(ASTExpr* expr, ASTExpr** lhs, ASTExpr** rhs)
{
    ASTExpr* left = *lhs;
    ASTExpr* right = *rhs;
    expr->type = g_type_bool;
    bool left_is_pointer = type_is_pointer(left->type);
    if(type_is_pointer(right->type) && !left_is_pointer)
    {
        left_is_pointer = true;
        ASTExpr** temp = lhs;
        lhs = rhs;
        rhs = temp;
        left = right;
        right = *rhs;
    }
    if(left_is_pointer)
        return implicit_cast(rhs, left->type);

    right = *rhs;

    if(!type_is_numeric(left->type) || !type_is_numeric(right->type))
    {
        sic_error_at(expr->loc, "Invalid operand types %s and %s.",
                   type_to_string(left->type), type_to_string(right->type));
        return false;
    }

    return arith_type_conv(expr, lhs, rhs);
}

static bool analyze_shift(ASTExpr* expr, ASTExpr** lhs, ASTExpr** rhs)
{
    ASTExpr* left = *lhs;
    ASTExpr* right = *rhs;
    if(!type_is_integer(left->type) || !type_is_integer(right->type))
    {
        sic_error_at(expr->loc, "Invalid operand types %s and %s.",
                   type_to_string(left->type), type_to_string(right->type));
        return false;
    }

    if(type_size(left->type) != type_size(right->type) && !implicit_cast(rhs, type_to_unsigned(left->type)))
        return false;
    
    expr->type = left->type;
    if(right->kind == EXPR_CONSTANT)
    {
        if(right->expr.constant.val.i >= left->type->builtin.bit_size)
            sic_diagnostic_at(expr->loc, DIAG_WARNING, "Shift amount >= integer width.");

        if(left->kind == EXPR_CONSTANT)
        {
            BinaryOpKind kind = expr->expr.binary.kind;
            if(kind == BINARY_SHL)
            {
                // expr->
            }
        }

    }


    return true;
}

static bool analyze_bit_op(ASTExpr* expr, ASTExpr** lhs, ASTExpr** rhs)
{
    ASTExpr* left = *lhs;
    ASTExpr* right = *rhs;
    if(!type_is_integer(left->type) || !type_is_integer(right->type))
    {
        sic_error_at(expr->loc, "Invalid operand types %s and %s.",
                   type_to_string(left->type), type_to_string(right->type));
        return false;
    }

    promote_int_type(lhs);
    promote_int_type(rhs);
    expr->type = (*lhs)->type;
    return true;

}

static bool analyze_assign(ASTExpr* expr, ASTExpr** lhs, ASTExpr** rhs)
{
    ASTExpr* left = *lhs;
    if(!expr_ensure_lvalue(left))
        return false;
    expr->type = left->type;
    g_sema.ident_mask = IDENT_VAR_ONLY;
    return implicit_cast(rhs, left->type);
}

static bool analyze_op_assign(ASTExpr* expr, ASTExpr** lhs, ASTExpr** rhs)
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

    ASTExpr* new_rhs = MALLOC_STRUCT(ASTExpr);
    new_rhs->kind = EXPR_BINARY;
    new_rhs->loc = expr->loc;
    new_rhs->expr.binary.kind = conversion[expr->expr.binary.kind - BINARY_OP_ASSIGN_START];
    new_rhs->expr.binary.lhs = *lhs;
    new_rhs->expr.binary.rhs = *rhs;
    expr->expr.binary.kind = BINARY_ASSIGN;
    *rhs = new_rhs;
    analyze_binary(new_rhs);
    new_rhs->evaluated = true;
    return !expr_is_bad(new_rhs) && analyze_assign(expr, lhs, rhs);
}

static bool analyze_addr_of(ASTExpr** expr_ref, ASTExpr* inner)
{
    ASTExpr* expr = *expr_ref;
    if(inner->kind == EXPR_IDENT)
    {
        Object* ident = inner->expr.ident;
        switch(ident->kind)
        {
        case OBJ_FUNC:
            *expr_ref = inner;
            // expr->type = ident->type;
            // printf("here\n");
            return true;
        case OBJ_VAR:
            switch(ident->var.kind)
            {
            case VAR_GLOBAL:
            case VAR_LOCAL:
            case VAR_PARAM:
            default:
                SIC_UNREACHABLE();
            }
            break;
        default:
            sic_error_at(expr->loc, "Cannot take address of type object '%s'.", ident->symbol);
            return false;
        }
    }
    else if(inner->kind == EXPR_UNARY && inner->expr.unary.kind == UNARY_DEREF)
    {
        *expr_ref = inner->expr.unary.inner;
        return true;
    }
    else if(!expr_is_lvalue(inner))
    {
        sic_error_at(expr->loc, "Cannot take address of rvalue.");
        return false;
    }

    if(type_is_array(inner->type))
        expr->type = type_pointer_to(inner->type->array.elem_type);
    else
        expr->type = type_pointer_to(inner->type);
    return true;
}

static bool analyze_bit_not(ASTExpr* expr, ASTExpr** inner)
{
    ASTExpr* in = *inner;
    if(!type_is_integer(in->type))
    {
        sic_error_at(expr->loc, "Invalid operand type %s.", 
                   type_to_string(in->type));
        return false;
    }

    promote_int_type(inner);
    expr->type = (*inner)->type;
    return true;
}

static bool analyze_deref(ASTExpr** expr_ref, ASTExpr* inner)
{
    if(inner->kind == EXPR_UNARY && inner->expr.unary.kind == UNARY_ADDR_OF)
    {
        *expr_ref = inner->expr.unary.inner;
        return true;
    }
    if(inner->type->kind == TYPE_POINTER)
    {
        (*expr_ref)->type = inner->type->pointer_base;
        return true;
    }

    sic_error_at((*expr_ref)->loc, "Cannot dereference non-pointer type %s.",
                 type_to_string(inner->type));
    return false;
}

static bool analyze_incdec(ASTExpr* expr, ASTExpr* inner)
{
    if(!expr_ensure_lvalue(inner))
        return false;

    expr->type = inner->type;
    if(!type_is_integer(expr->type))
        SIC_TODO_MSG("Remove temporary");
    return true;
}

static bool analyze_log_not(ASTExpr* expr, ASTExpr* inner)
{
    (void)inner;
    expr->type = g_type_bool;
    return implicit_cast(&expr->expr.unary.inner, g_type_bool);
}

static bool analyze_negate(ASTExpr* expr, ASTExpr* inner)
{
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


static bool arith_type_conv(ASTExpr* parent, ASTExpr** expr1, ASTExpr** expr2)
{
    ASTExpr* e1 = *expr1;
    ASTExpr* e2 = *expr2;
    if(!type_is_numeric(e1->type) || !type_is_numeric(e2->type))
    {
        sic_error_at(parent->loc, "Invalid operand types %s and %s.",
                   type_to_string(e1->type), type_to_string(e2->type));
        return false;
    }

    if(e1->type->kind < e2->type->kind)
    {
        ASTExpr** temp = expr1;
        expr1 = expr2;
        expr2 = temp;
        e1 = e2;
        e2 = *expr2;
    }

    if(type_size(e1->type) >= 4)
    {
        if(!implicit_cast(expr2, e1->type))
            SIC_UNREACHABLE();
        return true;
    }

    promote_int_type(expr1);
    promote_int_type(expr2);
    SIC_ASSERT((*expr1)->type->kind == (*expr2)->type->kind);
    return true;
}

static void promote_int_type(ASTExpr** expr)
{
    SIC_ASSERT(type_is_integer((*expr)->type));
    if(type_size((*expr)->type) < 4 && !implicit_cast(expr, g_type_int))
        SIC_UNREACHABLE();
}
