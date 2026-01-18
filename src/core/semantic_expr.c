#include "semantics.h"

#define ANALYZE_EXPR_SET_VALID(expr) do { valid &= analyze_expr(expr); } while(0)
#define ANALYZE_EXPR_OR_RET(expr) do { if(!analyze_expr(expr)) return false; } while(0)
#define IF_INVALID_RET() do { if(!valid) return false; } while(0)
#define ERROR_AND_RET(loc, ...) do { sic_error_at(loc, __VA_ARGS__); return false; } while(0)

// Expr kind functions
static bool analyze_array_access(ASTExpr* expr);
static bool analyze_array_init_list(ASTExpr* expr);
static bool analyze_binary(ASTExpr* expr);
static bool analyze_call(ASTExpr* expr);
static bool analyze_ident(ASTExpr* expr);
static bool analyze_var_expr(ASTExpr* expr, ObjVar* var, bool lvalue);
static bool analyze_struct_init_list(ASTExpr* expr);
static bool analyze_ternary(ASTExpr* expr);
static bool analyze_unary(ASTExpr* expr);
static bool analyze_unresolved_arrow(ASTExpr* expr, bool lvalue);
static bool analyze_unresolved_dot(ASTExpr* expr, bool lvalue);
static bool analyze_ct_alignof(ASTExpr* expr);
static bool analyze_ct_offsetof(ASTExpr* expr);
static bool analyze_ct_sizeof(ASTExpr* expr);

// Binary functions
static bool analyze_add(ASTExpr* expr, ASTExpr** lhs, ASTExpr** rhs);
static bool analyze_sub(ASTExpr* expr, ASTExpr** lhs, ASTExpr** rhs);
static bool analyze_mul(ASTExpr* expr, ASTExpr** lhs, ASTExpr** rhs);
static bool analyze_div(ASTExpr* expr, ASTExpr** lhs, ASTExpr** rhs);
static bool analyze_mod(ASTExpr* expr, ASTExpr** lhs, ASTExpr** rhs);
static bool analyze_logical(ASTExpr* expr, ASTExpr** lhs, ASTExpr** rhs);
static bool analyze_comparison(ASTExpr* expr, ASTExpr** lhs, ASTExpr** rhs);
static bool analyze_eq_ne(ASTExpr* expr, ASTExpr** lhs, ASTExpr** rhs);
static bool analyze_lt_ge(ASTExpr* expr, ASTExpr** lhs, ASTExpr** rhs);
static bool analyze_le_gt(ASTExpr* expr, ASTExpr** lhs, ASTExpr** rhs);
static bool analyze_shift(ASTExpr* expr, ASTExpr** lhs, ASTExpr** rhs);
static bool analyze_bit_op(ASTExpr* expr, ASTExpr** lhs, ASTExpr** rhs);
static bool analyze_bit_or(ASTExpr* expr, ASTExpr** lhs, ASTExpr** rhs);
static bool analyze_bit_xor(ASTExpr* expr, ASTExpr** lhs, ASTExpr** rhs);
static bool analyze_bit_and(ASTExpr* expr, ASTExpr** lhs, ASTExpr** rhs);
static bool analyze_assign(ASTExpr* expr, ASTExpr** lhs, ASTExpr** rhs);
static bool analyze_op_assign(ASTExpr* expr, ASTExpr** lhs, ASTExpr** rhs);

// Unary functions
static bool analyze_addr_of(ASTExpr* expr, ASTExpr* inner);
static bool analyze_bit_not(ASTExpr* expr, ASTExpr** inner_ref);
static bool analyze_deref(ASTExpr* expr, ASTExpr* inner);
static bool analyze_incdec(ASTExpr* expr, ASTExpr* inner);
static bool analyze_log_not(ASTExpr* expr, ASTExpr* inner);
static bool analyze_negate(ASTExpr* expr, ASTExpr** inner_ref);

static bool arith_type_conv(ASTExpr* parent, ASTExpr** e1, ASTExpr** e2);
static void promote_int_type(ASTExpr** expr);
static void convert_to_const_zero(ASTExpr* expr, Type* type);

bool analyze_expr(ASTExpr* expr)
{
    if(expr->evaluated) return expr->kind != EXPR_INVALID;
    bool success;
    switch(expr->kind)
    {
    case EXPR_INVALID:
        success = false;
        break;
    case EXPR_ARRAY_ACCESS:
        success = analyze_array_access(expr);
        break;
    case EXPR_ARRAY_INIT_LIST:
        success = analyze_array_init_list(expr);
        break;
    case EXPR_BINARY:
        success = analyze_binary(expr);
        break;
    case EXPR_CAST:
        success = analyze_cast(expr);
        break;
    case EXPR_CONSTANT:
        success = true;
        break;
    case EXPR_FUNC_CALL:
        success = analyze_call(expr);
        break;
    case EXPR_POSTFIX: {
        ASTExpr* const inner = expr->expr.unary.inner;
        success = analyze_expr(inner) && analyze_incdec(expr, inner);
        break;
    }
    case EXPR_RANGE:
        SIC_TODO();
    case EXPR_STRUCT_INIT_LIST:
        success = analyze_struct_init_list(expr);
        break;
    case EXPR_TERNARY:
        success = analyze_ternary(expr);
        break;
    case EXPR_UNARY:
        success = analyze_unary(expr);
        break;
    case EXPR_UNRESOLVED_ARR:
        success = analyze_unresolved_arrow(expr, false);
        break;
    case EXPR_UNRESOLVED_DOT:
        success = analyze_unresolved_dot(expr, false);
        break;
    case EXPR_UNRESOLVED_IDENT:
        success = analyze_ident(expr);
        break;
    case EXPR_CT_ALIGNOF:
        success = analyze_ct_alignof(expr);
        break;
    case EXPR_CT_OFFSETOF:
        success = analyze_ct_offsetof(expr);
        break;
    case EXPR_CT_SIZEOF:
        success = analyze_ct_sizeof(expr);
        break;
    case EXPR_IDENT:
    case EXPR_MEMBER_ACCESS:
    case EXPR_TYPE_IDENT:
    case EXPR_ZEROED_OUT:
        SIC_UNREACHABLE();
    }
    SIC_ASSERT(!success || (expr->type != NULL && expr->type->status == STATUS_RESOLVED));
    expr->evaluated = true;
    if(!success) expr->kind = EXPR_INVALID;
    return success;
}

bool analyze_lvalue(ASTExpr* expr, bool allow_func)
{
    bool success;
    switch(expr->kind)
    {
    case EXPR_ARRAY_ACCESS:
        if(expr->evaluated) return true;
        success = analyze_array_access(expr);
        break;
    case EXPR_MEMBER_ACCESS:
        SIC_ASSERT(expr->evaluated);
        return true;
    case EXPR_IDENT:
        success = expr->expr.ident->kind == OBJ_VAR;
        break;
    case EXPR_UNARY: {
        if(expr->expr.unary.kind != UNARY_DEREF) goto ERR;
        if(expr->evaluated) return true;
        ASTExpr* inner = expr->expr.unary.inner;
        success = analyze_expr(inner) && analyze_deref(expr, inner);
        break;
    }
    case EXPR_UNRESOLVED_ARR:
        success = analyze_unresolved_arrow(expr, true);
        break;
    case EXPR_UNRESOLVED_DOT:
        success = analyze_unresolved_dot(expr, true);
        break;
    case EXPR_UNRESOLVED_IDENT: {
        Object* ident = find_obj(&expr->expr.pre_sema_ident);
        if(ident->kind == OBJ_FUNC && allow_func)
        {
            ObjFunc* func = obj_as_func(ident);
            if(!analyze_function(func)) return false;
            expr->type = func->func_type;
            expr->kind = EXPR_IDENT;
            expr->expr.ident = ident;
            expr->const_eval = true;
            return true;
        }
        if(ident->kind != OBJ_VAR) goto ERR;
        success = analyze_var_expr(expr, obj_as_var(ident), true);
        break;
    }
    default:
    ERR:
        sic_error_at(expr->loc, "Expression is not assignable.");
        success = false;
        break;
    }

    SIC_ASSERT(!success || (expr->type != NULL && expr->type->status == STATUS_RESOLVED));
    expr->evaluated = true;
    if(!success) expr->kind = EXPR_INVALID;
    return success;

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

static bool analyze_array_access(ASTExpr* expr)
{
    ASTExpr* arr_expr = expr->expr.array_access.array_expr;
    bool valid = true;
    ANALYZE_EXPR_SET_VALID(arr_expr);
    ANALYZE_EXPR_SET_VALID(expr->expr.array_access.index_expr);
    IF_INVALID_RET();
    Type* arr_t = arr_expr->type->canonical;

    if(arr_t->kind == TYPE_INIT_LIST)
    {
        ERROR_AND_RET(expr->loc, 
                      "Cannot access element of array literal. Please first assign the array to a const "
                      "literal or cast it to a typed array (i.e. int[*]), then perform the access.");
    }
    if(arr_t->kind == TYPE_POINTER)
    {
        expr->type = arr_t->pointer_base;
        if(!resolve_type(&expr->type, RES_NORMAL, expr->loc, "Cannot access elements of type")) return false;
    }
    else if(type_is_array(arr_t))
        expr->type = arr_t->array.elem_type;
    else
    {
        ERROR_AND_RET(expr->loc, "Attempted to access element of non-array and non-pointer type \'%s\'",
                      type_to_string(arr_expr->type));
    }

    if(!implicit_cast(&expr->expr.array_access.index_expr, g_type_usize))
        return false;

    ASTExpr* index_expr = expr->expr.array_access.index_expr;

    if(index_expr->kind == EXPR_CONSTANT)
    {
        uint64_t idx = index_expr->expr.constant.val.i.lo;
        if(arr_t->kind == TYPE_STATIC_ARRAY && idx >= arr_t->array.static_len)
        {
            sic_diagnostic_at(DIAG_WARNING, expr->loc,
                              "Index of access(%lu) will overflow the length of the array(%lu).",
                              idx, arr_t->array.static_len);
            return true;
        }

        if(arr_expr->kind == EXPR_ARRAY_INIT_LIST)
        {
            ArrInitList* list = &arr_expr->expr.array_init;
            for(uint32_t i = 0; i < list->size; ++i)
            {
                if(list->data[i].const_index == idx)
                {
                    expr->kind = EXPR_CAST;
                    expr->expr.cast.inner = list->data[i].init_value;
                    perform_cast(expr);
                    return true;
                }
            }

            convert_to_const_zero(expr, expr->type);
        }
    }
    
    return true;
}

static bool analyze_array_init_list(ASTExpr* expr)
{
    ArrInitList* list = &expr->expr.array_init;
    bool valid = true;
    bool is_constant = true;
    uint64_t next_index = 0;
    uint64_t min = UINT64_MAX;
    uint64_t max = 0;
    for(uint32_t i = 0; i < list->size; ++i)
    {
        ArrInitEntry* entry = list->data + i;
        if(entry->arr_index != NULL)
        {
            if(!implicit_cast(&entry->arr_index, g_type_usize))
                valid = false;
            else if(entry->arr_index->kind != EXPR_CONSTANT)
            {
                sic_error_at(entry->arr_index->loc, "Array index must be a constant value.");
                valid = false;
            }
            else
            {
                entry->const_index = entry->arr_index->expr.constant.val.i.lo;
                next_index = entry->const_index + 1;
            }
        }
        else
            entry->const_index = next_index++;


        if(entry->const_index >= min && entry->const_index <= max)
        {
            for(uint32_t j = 0; j < i; ++j)
                if(entry->const_index == list->data[j].const_index)
                {
                    sic_error_at(entry->init_value->loc, "Array index '%lu' initialized multiple times.", entry->const_index);
                    valid = false;
                    break;
                }
        }
        else
        {
            if(entry->const_index < min)
                min = entry->const_index;
            if(entry->const_index > max)
                max = entry->const_index;
        }

        if(!analyze_expr(entry->init_value))
            valid = false;
        else if(!entry->init_value->const_eval)
            is_constant = false;

    }

    expr->const_eval = is_constant;
    list->max = max;
    return valid;
}

static inline bool analyze_binary(ASTExpr* expr)
{
    bool valid = true;
    ASTExpr** lhs = &expr->expr.binary.lhs;
    ASTExpr** rhs = &expr->expr.binary.rhs;
    BinaryOpKind kind = expr->expr.binary.kind;
    if(kind == BINARY_ASSIGN)
        return analyze_assign(expr, lhs, rhs);

    ANALYZE_EXPR_SET_VALID(*lhs);
    ANALYZE_EXPR_SET_VALID(*rhs);
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
        return analyze_mod(expr, lhs, rhs);
    case BINARY_LOG_OR:
    case BINARY_LOG_AND:
        return analyze_logical(expr, lhs, rhs);
    case BINARY_EQ:
    case BINARY_NE:
        return analyze_eq_ne(expr, lhs, rhs);
    case BINARY_LT:
    case BINARY_GE:
        return analyze_lt_ge(expr, lhs, rhs);
    case BINARY_LE:
    case BINARY_GT:
        return analyze_le_gt(expr, lhs, rhs);
    case BINARY_SHL:
    case BINARY_LSHR:
    case BINARY_ASHR:
        return analyze_shift(expr, lhs, rhs);
    case BINARY_BIT_OR:
        return analyze_bit_or(expr, lhs, rhs);
    case BINARY_BIT_XOR:
        return analyze_bit_xor(expr, lhs, rhs);
    case BINARY_BIT_AND:
        return analyze_bit_and(expr, lhs, rhs);
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
    case BINARY_ASSIGN:
        break;
    }
    SIC_UNREACHABLE();
}

static bool analyze_call(ASTExpr* expr)
{
    ASTExprCall* call = &expr->expr.call;
    ANALYZE_EXPR_OR_RET(call->func_expr);
    
    Type* func_type = call->func_expr->type->canonical;
    if(func_type->kind != TYPE_FUNC_PTR)
        ERROR_AND_RET(expr->loc, "Attempted to call non-function.");

    FuncSignature* sig = func_type->func_ptr;
    if(call->args.size < sig->params.size)
    {
        ERROR_AND_RET(expr->loc, "Too few arguments passed to function. Expected %s%u, got %u.",
                      sig->is_var_arg ? "at least " : "", sig->params.size, call->args.size);
    }
    if(!sig->is_var_arg && call->args.size > sig->params.size)
    {
        ERROR_AND_RET(expr->loc, "Too many arguments passed to function. Expected %u, got %u.",
                      sig->params.size, call->args.size);
    }

    bool valid = true;
    for(uint32_t i = 0; i < sig->params.size; ++i)
    {
        ObjVar* param = sig->params.data[i];
        if(!implicit_cast(call->args.data + i, param->type_loc.type))
        {
            valid = false;
            continue;
        }
    }

    for(uint32_t i = sig->params.size; i < call->args.size; ++i)
    {
        ASTExpr** arg = call->args.data + i;
        if(!implicit_cast_vararg(arg))
        {
            valid = false;
            continue;
        }
    }


    expr->type = sig->ret_type.type;
    return valid;
}

static bool analyze_ident(ASTExpr* expr)
{
    Object* ident = find_obj(&expr->expr.pre_sema_ident);
    if(ident == NULL) return false;

    switch(ident->kind)
    {
    case OBJ_ENUM_VALUE: {
        ObjEnumValue* enum_val = obj_as_enum_value(ident);
        expr->type = enum_val->enum_type;
        expr->kind = EXPR_CONSTANT;
        expr->expr.constant.kind = CONSTANT_INTEGER;
        expr->expr.constant.val.i = enum_val->const_value;
        expr->const_eval = true;
        return true;
    }
    case OBJ_FUNC: {
        ObjFunc* func = obj_as_func(ident);
        if(!analyze_function(func)) return false;
        expr->type = func->func_type;
        expr->kind = EXPR_IDENT;
        expr->expr.ident = ident;
        return true;
    }
    case OBJ_BITFIELD:
    case OBJ_ENUM:
        expr->type = g_type_invalid;
        expr->kind = EXPR_TYPE_IDENT;
        expr->expr.ident = ident;
        return true;
    case OBJ_STRUCT:
    case OBJ_TYPEDEF:
    case OBJ_UNION:
        SIC_TODO_MSG("Type expressions.");
    case OBJ_VAR:
        return analyze_var_expr(expr, obj_as_var(ident), false);
    case OBJ_INVALID:
        return false;
    case OBJ_IMPORT:
    case OBJ_MODULE:
        break;
    }
    SIC_UNREACHABLE();
}

static bool analyze_var_expr(ASTExpr* expr, ObjVar* var, bool lvalue)
{
    if(var->kind == VAR_CONST)
        SIC_TODO();

    if(var->kind == VAR_GLOBAL)
    {
        if(!analyze_global_var(var)) return false;
        if(lvalue)
            expr->const_eval = true;
        else if(g_sema->in_global_init)
        {
            if(var->initial_val == NULL)
                convert_to_const_zero(expr, var->type_loc.type);
            else
                *expr = *var->initial_val;
        }
    }


    expr->type = var->type_loc.type;
    expr->kind = EXPR_IDENT;
    expr->expr.ident = &var->header;
    return true;

}

static bool analyze_struct_init_list(ASTExpr* expr)
{
    bool valid = true;
    bool seen_named = false;
    bool errored = false;
    StructInitList* list = &expr->expr.struct_init;
    for(uint32_t i = 0; i < list->size; ++i)
    {
        StructInitEntry* entry = list->data + i;
        if(!analyze_expr(entry->init_value)) valid = false;
        if(entry->member.sym != NULL)
            seen_named = true;
        else if(seen_named && !errored)
        {
            sic_error_at(entry->init_value->loc, "Unnamed members of initializer list must always precede named members.");
            errored = true;
            valid = false;
        }
    }
    return valid;
}

static bool analyze_ternary(ASTExpr* expr)
{
    ASTExprTernary* tern = &expr->expr.ternary;
    bool valid = true;
    ANALYZE_EXPR_SET_VALID(tern->cond_expr);
    if(tern->then_expr != NULL)
        ANALYZE_EXPR_SET_VALID(tern->then_expr);
    else
        tern->then_expr = tern->cond_expr;
    ANALYZE_EXPR_SET_VALID(tern->else_expr);
    IF_INVALID_RET();

    if(!type_equal(tern->then_expr->type, tern->else_expr->type) &&
       !arith_type_conv(expr, &tern->then_expr, &tern->else_expr))
        return false;

    expr->type = tern->else_expr->type;
    return implicit_cast(&tern->cond_expr, g_type_bool);
}

static bool analyze_unary(ASTExpr* expr)
{
    ASTExpr* inner = expr->expr.unary.inner;

    switch(expr->expr.unary.kind)
    {
    case UNARY_INVALID:
        break;
    case UNARY_ADDR_OF:
        return analyze_addr_of(expr, inner);
    case UNARY_BIT_NOT:
        ANALYZE_EXPR_OR_RET(inner);
        return analyze_bit_not(expr, &expr->expr.unary.inner);
    case UNARY_DEC:
    case UNARY_INC:
        ANALYZE_EXPR_OR_RET(inner);
        return analyze_incdec(expr, inner);
    case UNARY_DEREF:
        ANALYZE_EXPR_OR_RET(inner);
        return analyze_deref(expr, inner);
    case UNARY_LOG_NOT:
        ANALYZE_EXPR_OR_RET(inner);
        return analyze_log_not(expr, inner);
    case UNARY_NEG:
        ANALYZE_EXPR_OR_RET(inner);
        return analyze_negate(expr, &expr->expr.unary.inner);
    }
    SIC_UNREACHABLE();
}

static bool resolve_member(ASTExpr* expr, ASTExpr* parent, bool lvalue)
{
    ASTExprUAccess* uaccess = &expr->expr.unresolved_access;
    Symbol member = uaccess->member.sym;
    SIC_ASSERT(parent->type->status == STATUS_RESOLVED);
    Type* t = parent->type->canonical;
    switch(t->kind)
    {
    case TYPE_STATIC_ARRAY:
        if(member == g_sym_len)
        {
            if(lvalue) goto NO_STORAGE;
            expr->kind = EXPR_CONSTANT;
            expr->const_eval = true;
            expr->expr.constant.kind = CONSTANT_INTEGER;
            expr->expr.constant.val.i = t->array.static_len;
            expr->type = g_type_usize;
            return true;
        }
        break;
    case TYPE_RUNTIME_ARRAY:
        SIC_TODO();
    case TYPE_STRUCT:
    case TYPE_UNION: {
        const ObjVarDA members = t->struct_->members;
        for(uint32_t i = 0; i < members.size; ++i)
            if(members.data[i]->header.symbol == member)
            {
                expr->kind = EXPR_MEMBER_ACCESS;
                expr->expr.member_access.parent_expr = parent;
                expr->expr.member_access.member = members.data[i];
                expr->expr.member_access.member_idx = i;
                expr->type = members.data[i]->type_loc.type;
                return true;
            }
        break;
    }
    case TYPE_VOID:
    case INT_TYPES:
    case FLOAT_TYPES:
    case TYPE_POINTER:
    case TYPE_FUNC_PTR:
    case TYPE_ENUM:
    case TYPE_ENUM_DISTINCT:
        break;
    case TYPE_INVALID:
    case TYPE_ALIAS:
    case TYPE_ALIAS_DISTINCT:
    case TYPE_AUTO:
    case TYPE_INIT_LIST:
    case TYPE_PS_ARRAY:
    case TYPE_PS_USER:
    case TYPE_STRING_LIT:
    case TYPE_TYPEOF:
    case __TYPE_COUNT:
        SIC_UNREACHABLE();
    }

    sic_error_at(uaccess->member.loc, "Type \'%s\' has no member \'%s\'.",
                 type_to_string(parent->type), member);
    return NULL;

NO_STORAGE:
    sic_error_at(uaccess->member.loc, "Member \'%s\' has no storage, so it cannot be an lvalue.",
                 member);
    return NULL;
}

static bool analyze_unresolved_arrow(ASTExpr* expr, bool lvalue)
{
    ASTExpr* parent = expr->expr.unresolved_access.parent_expr;
    ANALYZE_EXPR_OR_RET(parent);
    Type* t = parent->type->canonical;

    if(t->kind != TYPE_POINTER)
    {
        sic_error_at(expr->loc, "Arrow operator can only be used on pointers.");
        return false;
    }

    ASTExpr* deref = CALLOC_STRUCT(ASTExpr);
    deref->loc = expr->loc;
    deref->kind = EXPR_UNARY;
    deref->expr.unary.kind = UNARY_DEREF;
    deref->expr.unary.inner = parent;

    ANALYZE_EXPR_OR_RET(deref);
    return resolve_member(expr, deref, lvalue);
}

static bool analyze_unresolved_dot(ASTExpr* expr, bool lvalue)
{
    ASTExprUAccess* uaccess = &expr->expr.unresolved_access;
    ASTExpr* parent = uaccess->parent_expr;
    ANALYZE_EXPR_OR_RET(parent);

    if(parent->kind == EXPR_TYPE_IDENT)
    {
        ObjEnum* enum_ = obj_as_enum(parent->expr.ident);
        const ObjEnumValueDA values = enum_->values;
        for(uint32_t i = 0; i < values.size; ++i)
            if(values.data[i]->header.symbol == uaccess->member.sym)
            {
                expr->const_eval = true;
                expr->kind = EXPR_CONSTANT;
                expr->type = enum_->type_ref;
                expr->expr.constant.kind = CONSTANT_INTEGER;
                expr->expr.constant.val.i = values.data[i]->const_value;
                return true;
            }
        sic_error_at(uaccess->member.loc, "Enum \'%s\' has no value \'%s\'.",
                     parent->expr.ident->symbol, uaccess->member.sym);
        return false;
    }

    if(parent->type->canonical->kind == TYPE_POINTER)
    {
        ASTExpr* deref = CALLOC_STRUCT(ASTExpr);
        deref->loc = expr->loc;
        deref->kind = EXPR_UNARY;
        deref->expr.unary.kind = UNARY_DEREF;
        deref->expr.unary.inner = parent;
        parent = deref;
        ANALYZE_EXPR_OR_RET(parent);
    }

    if(!resolve_member(expr, parent, lvalue)) return false;
    if(lvalue) expr->const_eval = parent->const_eval;
    return true;
}

static bool analyze_ct_alignof(ASTExpr* expr)
{
    if(!resolve_type(&expr->expr.ct_alignof.type, RES_NORMAL, expr->expr.ct_sizeof.loc, "Cannot get alignment of unresolved type"))
        return false;

    ByteSize align = type_alignment(expr->expr.ct_alignof.type);
    expr->type = g_type_usize;
    expr->kind = EXPR_CONSTANT;
    expr->expr.constant.kind = CONSTANT_INTEGER;
    expr->expr.constant.val.i = align;
    return true;
}

static bool analyze_ct_offsetof(ASTExpr* expr)
{
    if(!resolve_type(&expr->expr.ct_offsetof.struct_.type, RES_ALLOW_ALL, LOC_NULL, NULL))
    {

    }
    return true;
}

static bool analyze_ct_sizeof(ASTExpr* expr)
{
    if(!resolve_type(&expr->expr.ct_sizeof.type, RES_NORMAL, expr->expr.ct_sizeof.loc, "Cannot get size of unresolved type"))
        return false;
    
    uint32_t size = type_size(expr->expr.ct_sizeof.type);
    expr->type = g_type_usize;
    expr->kind = EXPR_CONSTANT;
    expr->expr.constant.kind = CONSTANT_INTEGER;
    expr->expr.constant.val.i = size;
    return true;
}

static bool analyze_add(ASTExpr* expr, ASTExpr** lhs, ASTExpr** rhs)
{
    ASTExpr* left = *lhs;
    ASTExpr* right = *rhs;
    Type* lt = left->type->canonical;
    Type* rt = right->type->canonical;
    bool left_is_pointer = lt->kind == TYPE_POINTER;
    bool right_is_pointer = rt->kind == TYPE_POINTER;
    if(right_is_pointer)
    {
        left_is_pointer = true;
        *lhs = right;
        *rhs = left;
        left = right;
        right = *rhs;
        rt = lt;
    }

    if(left_is_pointer)
    {
        if(!type_is_integer(rt))
        {
            sic_error_at(expr->loc, "Invalid operand types %s and %s.",
                         type_to_string(left->type), type_to_string(right->type));
            return false;
        }

        implicit_cast(rhs, type_is_unsigned(rt) ? g_type_uptr : g_type_iptr);
        expr->type = left->type;
        return true;
    }

    if(!arith_type_conv(expr, lhs, rhs))
        return false;

    left = *lhs;
    right = *rhs;

    expr->type = left->type;

    if(left->kind == EXPR_CONSTANT && right->kind == EXPR_CONSTANT)
    {
        expr->kind = EXPR_CONSTANT;
        expr->const_eval = true;
        if(left->expr.constant.kind == CONSTANT_FLOAT)
        {
            expr->expr.constant.kind = CONSTANT_FLOAT;
            expr->expr.constant.val.f = left->expr.constant.val.f + right->expr.constant.val.f;
        }
        else
        {
            expr->expr.constant.kind = CONSTANT_INTEGER;
            expr->expr.constant.val.i = left->expr.constant.val.i + right->expr.constant.val.i;
            const_int_correct(expr);
        }
        return true;
    }

    expr->const_eval = left->const_eval && right->const_eval;
    return true;
}

static bool analyze_sub(ASTExpr* expr, ASTExpr** lhs, ASTExpr** rhs)
{
    ASTExpr* left = *lhs;
    ASTExpr* right = *rhs;
    if(left->type->canonical->kind == TYPE_POINTER)
    {
        if(!type_is_integer(right->type->canonical))
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
    if(left->kind == EXPR_CONSTANT && right->kind == EXPR_CONSTANT)
    {
        expr->kind = EXPR_CONSTANT;
        expr->const_eval = true;
        if(left->expr.constant.kind == CONSTANT_FLOAT)
        {
            expr->expr.constant.kind = CONSTANT_FLOAT;
            expr->expr.constant.val.f = left->expr.constant.val.f - right->expr.constant.val.f;
        }
        else
        {
            expr->expr.constant.kind = CONSTANT_INTEGER;
            expr->expr.constant.val.i = left->expr.constant.val.i - right->expr.constant.val.i;
            const_int_correct(expr);
        }
        return true;
    }

    expr->const_eval = left->const_eval && right->const_eval;
    return true;
}

static bool analyze_mul(ASTExpr* expr, ASTExpr** lhs, ASTExpr** rhs)
{
    if(!arith_type_conv(expr, lhs, rhs))
        return false;

    ASTExpr* left = *lhs;
    ASTExpr* right = *rhs;

    expr->type = left->type;
    if(left->kind == EXPR_CONSTANT && right->kind == EXPR_CONSTANT)
    {
        expr->kind = EXPR_CONSTANT;
        expr->const_eval = true;
        if(left->expr.constant.kind == CONSTANT_FLOAT)
        {
            expr->expr.constant.kind = CONSTANT_FLOAT;
            expr->expr.constant.val.f = left->expr.constant.val.f * right->expr.constant.val.f;
        }
        else
        {
            expr->expr.constant.kind = CONSTANT_INTEGER;
            expr->expr.constant.val.i = left->expr.constant.val.i * right->expr.constant.val.i;
            const_int_correct(expr);
        }
        return true;
    }

    expr->const_eval = left->const_eval && right->const_eval;
    return true;
}

static bool analyze_div(ASTExpr* expr, ASTExpr** lhs, ASTExpr** rhs)
{
    if(!arith_type_conv(expr, lhs, rhs))
        return false;

    ASTExpr* left = *lhs;
    ASTExpr* right = *rhs;

    expr->type = left->type;
    if(left->kind == EXPR_CONSTANT && right->kind == EXPR_CONSTANT)
    {
        expr->kind = EXPR_CONSTANT;
        expr->const_eval = true;
        if(left->expr.constant.kind == CONSTANT_FLOAT)
        {
            double rval = right->expr.constant.val.f;
            if(rval == 0.0)
                ERROR_AND_RET(expr->loc, "Right side of division evaluates to 0.");
            expr->expr.constant.kind = CONSTANT_FLOAT;
            expr->expr.constant.val.f = left->expr.constant.val.f / rval;
        }
        else
        {
            uint64_t rval = right->expr.constant.val.i;
            if(rval == 0)
                ERROR_AND_RET(expr->loc, "Right side of division evaluates to 0.");
            expr->expr.constant.kind = CONSTANT_INTEGER;
            expr->expr.constant.val.i = left->expr.constant.val.i / rval;
            const_int_correct(expr);
        }
        return true;
    }

    expr->const_eval = left->const_eval && right->const_eval;
    return true;
}

static bool analyze_mod(ASTExpr* expr, ASTExpr** lhs, ASTExpr** rhs)
{
    ASTExpr* left = *lhs;
    ASTExpr* right = *rhs;
    if(!type_is_integer(left->type->canonical) || !type_is_integer(right->type->canonical))
    {
        sic_error_at(expr->loc, "Invalid operand types %s and %s.",
                     type_to_string(left->type), type_to_string(right->type));
        return false;
    }

    SIC_ASSERT(arith_type_conv(expr, lhs, rhs));

    left = *lhs;
    right = *rhs;

    expr->type = left->type;
    if(left->kind == EXPR_CONSTANT && right->kind == EXPR_CONSTANT)
    {
        uint64_t rval = right->expr.constant.val.i;
        if(rval == 0)
            ERROR_AND_RET(expr->loc, "Right side of modulo evaluates to 0.");
        expr->kind = EXPR_CONSTANT;
        expr->const_eval = true;
        expr->expr.constant.kind = CONSTANT_INTEGER;
        expr->expr.constant.val.i = left->expr.constant.val.i % rval;
        const_int_correct(expr);
        return true;
    }

    expr->const_eval = left->const_eval && right->const_eval;
    return true;
}

static bool analyze_logical(ASTExpr* expr, ASTExpr** lhs, ASTExpr** rhs)
{
    if(!implicit_cast(lhs, g_type_bool) || !implicit_cast(rhs, g_type_bool))
        return false;

    expr->type = g_type_bool;

    ASTExpr* left = *lhs;
    ASTExpr* right = *rhs;
    if(left->kind == EXPR_CONSTANT)
    {
        SIC_ASSERT(left->expr.constant.kind == CONSTANT_INTEGER);
        *expr = (expr->expr.binary.kind == BINARY_LOG_OR) ^ left->expr.constant.val.i ? *right : *left;
        return true;
    }

    if(right->kind == EXPR_CONSTANT && ((expr->expr.binary.kind == BINARY_LOG_OR) ^ right->expr.constant.val.i))
    {
        SIC_ASSERT(right->expr.constant.kind == CONSTANT_INTEGER);
        *expr = *left; 
    }
    return true;
}

static bool analyze_comparison(ASTExpr* expr, ASTExpr** lhs, ASTExpr** rhs)
{
    Type* lhs_type = (*lhs)->type;
    Type* rhs_type = (*rhs)->type;
    Type* lhs_ctype = lhs_type->canonical;
    Type* rhs_ctype = rhs_type->canonical;
    expr->type = g_type_bool;
    if(lhs_ctype->kind == TYPE_STRING_LIT || rhs_ctype->kind == TYPE_STRING_LIT)
    {
        // TODO: Remove this. It is temporarily here because the string type is not
        // fleshed out. There are two options: allow comparisons between strings
        // which would resolve to a strcmp under the hood, or disallow them entirely.
        sic_error_at(expr->loc, "String literals cannot be compared.");
        return false;
    }
    if(lhs_ctype->kind == TYPE_POINTER || lhs_ctype->kind == TYPE_FUNC_PTR)
        return implicit_cast(rhs, lhs_type);
    if(rhs_ctype->kind == TYPE_POINTER || rhs_ctype->kind == TYPE_FUNC_PTR)
        return implicit_cast(lhs, rhs_type);
    if(type_is_integer(lhs_ctype) && type_is_integer(rhs_ctype))
    {
    }

    return arith_type_conv(expr, lhs, rhs);
}

static bool analyze_eq_ne(ASTExpr* expr, ASTExpr** lhs, ASTExpr** rhs)
{
    if(!analyze_comparison(expr, lhs, rhs)) return false;

    ASTExpr* left = *lhs;
    ASTExpr* right = *rhs;
    Type* ty = left->type->canonical;
    if(left->kind == EXPR_CONSTANT && right->kind == EXPR_CONSTANT)
    {
        bool value;
        if(type_is_float(ty))
            value = left->expr.constant.val.f == right->expr.constant.val.f;
        else
            value = left->expr.constant.val.i == right->expr.constant.val.i;

        expr->kind = EXPR_CONSTANT;
        expr->expr.constant.kind = CONSTANT_INTEGER;
        expr->expr.constant.val.i = value ^ (expr->expr.binary.kind == BINARY_NE);
        expr->const_eval = true;
    }
    return true;
}

static bool analyze_lt_ge(ASTExpr* expr, ASTExpr** lhs, ASTExpr** rhs)
{
    if(!analyze_comparison(expr, lhs, rhs)) return false;

    ASTExpr* left = *lhs;
    ASTExpr* right = *rhs;
    Type* ty = left->type->canonical;
    if(left->kind == EXPR_CONSTANT && right->kind == EXPR_CONSTANT)
    {
        bool value;
        if(type_is_float(ty))
            value = left->expr.constant.val.f < right->expr.constant.val.f;
        else if(type_is_signed(ty))
            value = (int64_t)left->expr.constant.val.i < (int64_t)right->expr.constant.val.i;
        else
            value = left->expr.constant.val.i < right->expr.constant.val.i;

        expr->kind = EXPR_CONSTANT;
        expr->expr.constant.kind = CONSTANT_INTEGER;
        expr->expr.constant.val.i = value ^ (expr->expr.binary.kind == BINARY_GE);
        expr->const_eval = true;
    }
    return true;
}

static bool analyze_le_gt(ASTExpr* expr, ASTExpr** lhs, ASTExpr** rhs)
{
    if(!analyze_comparison(expr, lhs, rhs)) return false;

    ASTExpr* left = *lhs;
    ASTExpr* right = *rhs;
    Type* ty = left->type->canonical;
    if(left->kind == EXPR_CONSTANT && right->kind == EXPR_CONSTANT)
    {
        bool value;
        if(type_is_float(ty))
            value = left->expr.constant.val.f <= right->expr.constant.val.f;
        else if(type_is_signed(ty))
            value = (int64_t)left->expr.constant.val.i <= (int64_t)right->expr.constant.val.i;
        else
            value = left->expr.constant.val.i <= right->expr.constant.val.i;

        expr->kind = EXPR_CONSTANT;
        expr->expr.constant.kind = CONSTANT_INTEGER;
        expr->expr.constant.val.i = value ^ (expr->expr.binary.kind == BINARY_GT);
        expr->const_eval = true;
    }
    return true;
}

static bool analyze_shift(ASTExpr* expr, ASTExpr** lhs, ASTExpr** rhs)
{
    // VERY IMPORTANT: We should not cast the lhs to anything, the reason being
    // that it could result in weird behavior for certain integer types and combinations
    // where the top bits of smaller types dont get cut off and are instead carried over
    // when casted to larger types.
    // 
    // By making the parent expressions type the same as the lhs no matter what, we
    // avoid this problem entirely.
    ASTExpr* left = *lhs;
    ASTExpr* right = *rhs;
    Type* lhs_ctype = left->type->canonical;
    Type* rhs_ctype = right->type->canonical;

    if(!type_is_integer(lhs_ctype) || !type_is_integer(rhs_ctype))
    {
        sic_error_at(expr->loc, "Invalid operand types %s and %s.",
                     type_to_string(left->type), type_to_string(right->type));
        return false;
    }

    if(lhs_ctype->builtin.byte_size != rhs_ctype->builtin.byte_size)
    {
        right = CALLOC_STRUCT(ASTExpr);
        right->kind = EXPR_CAST;
        right->type = lhs_ctype;
        right->loc = (*rhs)->loc;
        right->expr.cast.inner = (*rhs);
        perform_cast(right);
        *rhs = right;
    }

    
    expr->type = left->type;
    if(right->kind == EXPR_CONSTANT)
    {
        if(right->expr.constant.val.i >= lhs_ctype->builtin.bit_size)
            sic_diagnostic_at(DIAG_WARNING, expr->loc, "Shift amount (%lu) >= integer width (%u).",
                              right->expr.constant.val.i, lhs_ctype->builtin.bit_size);

        if(left->kind == EXPR_CONSTANT)
        {
            expr->kind = EXPR_CONSTANT;
            expr->expr.constant.kind = CONSTANT_INTEGER;
            expr->const_eval = true;
            BinaryOpKind kind = expr->expr.binary.kind;
            if(kind == BINARY_SHL)
            {
                expr->expr.constant.val.i = left->expr.constant.val.i << right->expr.constant.val.i;
                const_int_correct(expr);
            }
            else if(kind == BINARY_LSHR)
                expr->expr.constant.val.i = left->expr.constant.val.i >> right->expr.constant.val.i;
            else
            {
                BitSize shift = lhs_ctype->builtin.bit_size;
                int64_t val = (int64_t)(left->expr.constant.val.i << shift);
                expr->expr.constant.val.i = val >> (right->expr.constant.val.i + shift);
                const_int_correct(expr);
            }
            return true;
        }
    }


    return true;
}

static bool analyze_bit_op(ASTExpr* expr, ASTExpr** lhs, ASTExpr** rhs)
{
    Type* lhs_type = (*lhs)->type;
    Type* rhs_type = (*rhs)->type;
    if(!type_is_integer(lhs_type->canonical) || !type_is_integer(rhs_type->canonical))
    {
        sic_error_at(expr->loc, "Invalid operand types %s and %s.",
                     type_to_string(lhs_type), type_to_string(rhs_type));
        return false;
    }

    if(!arith_type_conv(expr, lhs, rhs)) return false;
    expr->type = (*lhs)->type;
    return true;

}

static bool analyze_bit_or(ASTExpr* expr, ASTExpr** lhs, ASTExpr** rhs)
{
    if(!analyze_bit_op(expr, lhs, rhs)) return false;

    ASTExpr* left = *lhs;
    ASTExpr* right = *rhs;
    if(left->kind == EXPR_CONSTANT && right->kind == EXPR_CONSTANT)
    {
        expr->kind = EXPR_CONSTANT;
        expr->expr.constant.kind = CONSTANT_INTEGER;
        expr->expr.constant.val.i = left->expr.constant.val.i | right->expr.constant.val.i;
        expr->const_eval = true;
        const_int_correct(expr);
    }
    return true;
}

static bool analyze_bit_xor(ASTExpr* expr, ASTExpr** lhs, ASTExpr** rhs)
{
    if(!analyze_bit_op(expr, lhs, rhs)) return false;

    ASTExpr* left = *lhs;
    ASTExpr* right = *rhs;
    if(left->kind == EXPR_CONSTANT && right->kind == EXPR_CONSTANT)
    {
        expr->kind = EXPR_CONSTANT;
        expr->expr.constant.kind = CONSTANT_INTEGER;
        expr->expr.constant.val.i = left->expr.constant.val.i ^ right->expr.constant.val.i;
        expr->const_eval = true;
        const_int_correct(expr);
    }
    return true;
}
static bool analyze_bit_and(ASTExpr* expr, ASTExpr** lhs, ASTExpr** rhs)
{
    if(!analyze_bit_op(expr, lhs, rhs)) return false;

    ASTExpr* left = *lhs;
    ASTExpr* right = *rhs;
    if(left->kind == EXPR_CONSTANT && right->kind == EXPR_CONSTANT)
    {
        expr->kind = EXPR_CONSTANT;
        expr->expr.constant.kind = CONSTANT_INTEGER;
        expr->expr.constant.val.i = left->expr.constant.val.i & right->expr.constant.val.i;
        expr->const_eval = true;
        const_int_correct(expr);
    }
    return true;
}

static bool analyze_assign(ASTExpr* expr, ASTExpr** lhs, ASTExpr** rhs)
{
    if(!analyze_lvalue(expr, false)) return false;
    ASTExpr* left = *lhs;
    if(!expr_ensure_lvalue(left))
        return false;
    expr->type = left->type;
    return implicit_cast(rhs, left->type);
}

static bool analyze_op_assign(ASTExpr* expr, ASTExpr** lhs, ASTExpr** rhs)
{
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

    ASTExpr* new_rhs = CALLOC_STRUCT(ASTExpr);
    new_rhs->kind = EXPR_BINARY;
    new_rhs->loc = expr->loc;
    new_rhs->expr.binary.kind = conversion[expr->expr.binary.kind - BINARY_OP_ASSIGN_START];
    new_rhs->expr.binary.lhs = *lhs;
    new_rhs->expr.binary.rhs = *rhs;
    expr->expr.binary.kind = BINARY_ASSIGN;
    *rhs = new_rhs;
    return analyze_assign(expr, lhs, rhs);
}

static bool analyze_addr_of(ASTExpr* expr, ASTExpr* inner)
{
    if(!analyze_lvalue(inner, true)) return false;
    expr->const_eval = inner->const_eval;

    if(inner->kind == EXPR_IDENT && inner->expr.ident->kind == OBJ_FUNC)
    {
        expr->type = inner->type;
        return true;
    }

    if(inner->kind == EXPR_UNARY && inner->expr.unary.kind == UNARY_DEREF)
    {
        *expr = *inner->expr.unary.inner;
        return true;
    }

    expr->type = type_pointer_to(type_is_array(inner->type) ? inner->type->array.elem_type : inner->type);
    return true;
}

static bool analyze_bit_not(ASTExpr* expr, ASTExpr** inner)
{
    Type* type = (*inner)->type;
    if(!type_is_integer(type->canonical))
    {
        sic_error_at(expr->loc, "Invalid operand type %s.", 
                     type_to_string(type));
        return false;
    }

    promote_int_type(inner);
    ASTExpr* in = *inner;

    expr->type = in->type;
    if(in->kind == EXPR_CONSTANT)
    {
        SIC_ASSERT(in->expr.constant.kind == CONSTANT_INTEGER);
        expr->kind = EXPR_CONSTANT;
        expr->expr.constant.kind = CONSTANT_INTEGER;
        expr->expr.constant.val.i = ~in->expr.constant.val.i;
        expr->const_eval = true;
        const_int_correct(expr);
        return true;
    }

    expr->const_eval = in->const_eval;
    return true;
}

static bool analyze_deref(ASTExpr* expr, ASTExpr* inner)
{
    Type* ctype = inner->type->canonical;
    if(ctype->kind != TYPE_POINTER)
    {
        sic_error_at(expr->loc, "Cannot dereference non-pointer type %s.",
                     type_to_string(inner->type));
        return false;
    }

    if(inner->kind == EXPR_UNARY && inner->expr.unary.kind == UNARY_ADDR_OF)
    {
        *expr = *inner->expr.unary.inner;
        return true;
    }

    expr->type = ctype->pointer_base;
    return resolve_type(&expr->type, RES_NORMAL, expr->loc, "Cannot dereference pointer to type");
}

static bool analyze_incdec(ASTExpr* expr, ASTExpr* inner)
{
    if(!expr_ensure_lvalue(inner))
        return false;

    if(!type_is_integer(inner->type->canonical))
    {
        sic_error_at(expr->loc, "Invalid operand type %s.",
                     type_to_string(inner->type));
        return false;
    }
    expr->type = inner->type;
    return true;
}

static bool analyze_log_not(ASTExpr* expr, ASTExpr* inner)
{
    if(!implicit_cast(&expr->expr.unary.inner, g_type_bool)) 
        return false;

    expr->type = g_type_bool;
    if(inner->kind == EXPR_CONSTANT)
    {
        expr->kind = EXPR_CONSTANT;
        expr->const_eval = true;
        expr->expr.constant.kind = CONSTANT_INTEGER;
        expr->expr.constant.val.i = !inner->expr.constant.val.i;
    }
    return true;
}

static bool analyze_negate(ASTExpr* expr, ASTExpr** inner_ref)
{
    ASTExpr* inner = *inner_ref;
    Type* type = inner->type;
    Type* ctype = type->canonical;
    if(!type_is_numeric(ctype))
    {
        sic_error_at(expr->loc, "Cannot negate non-numeric type %s.",
                     type_to_string(type));
        return false;
    }


    if(inner->kind == EXPR_CONSTANT)
    {
        expr->kind = EXPR_CONSTANT;
        expr->const_eval = true;
        if(inner->expr.constant.kind == CONSTANT_FLOAT)
        {
            expr->expr.constant.val.f = -inner->expr.constant.val.f;
            expr->expr.constant.kind = CONSTANT_FLOAT;
            expr->type = type;
            return true;
        }

        expr->expr.constant.val.i = -inner->expr.constant.val.i;
        expr->expr.constant.kind = CONSTANT_INTEGER;
        switch(ctype->kind)
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
        case TYPE_BYTE:
        case TYPE_SHORT:
        case TYPE_INT:
        case TYPE_LONG:
        case TYPE_ULONG:
            expr->type = type;
            return true;
        default:
            SIC_UNREACHABLE();
        }
    }

    if(type_is_integer(ctype))
        promote_int_type(inner_ref);

    expr->type = (*inner_ref)->type;
    return true;
}


static bool arith_type_conv(ASTExpr* parent, ASTExpr** expr1, ASTExpr** expr2)
{
    Type* t1  = (*expr1)->type->canonical;
    Type* t2  = (*expr2)->type->canonical;
    if(!type_is_numeric(t1) || !type_is_numeric(t2))
    {
        sic_error_at(parent->loc, "Invalid operand types %s and %s.",
                     type_to_string((*expr1)->type), type_to_string((*expr2)->type));
        return false;
    }

    int w1 = type_is_float(t1) * 100 + t1->builtin.byte_size + type_is_unsigned(t1);
    int w2 = type_is_float(t2) * 100 + t2->builtin.byte_size + type_is_unsigned(t2);

    if(w1 < w2)
    {
        ASTExpr** temp = expr1;
        expr1 = expr2;
        expr2 = temp;
        t1 = t2;
        w1 = w2;
    }

    if(w1 < 100) // int
        promote_int_type(expr1);
    implicit_cast(expr2, (*expr1)->type);
    SIC_ASSERT(type_equal((*expr1)->type, (*expr2)->type));
    return true;
}

static void promote_int_type(ASTExpr** expr)
{
    Type* ctype = (*expr)->type->canonical;
    SIC_ASSERT(type_is_integer(ctype));
    if(ctype->builtin.byte_size < 4)
        implicit_cast_ensured(expr, g_type_int);
}

static void convert_to_const_zero(ASTExpr* expr, Type* type)
{
    expr->type = type;
    type = type_reduce(type);
    expr->const_eval = true;
    switch(type->kind)
    {
    case INT_TYPES:
        expr->kind = EXPR_CONSTANT;
        expr->expr.constant.kind = CONSTANT_INTEGER;
        expr->expr.constant.val.i = 0;
        return;
    case FLOAT_TYPES:
        expr->kind = EXPR_CONSTANT;
        expr->expr.constant.kind = CONSTANT_FLOAT;
        expr->expr.constant.val.f = 0.0;
        return;
    case TYPE_POINTER:
    case TYPE_FUNC_PTR:
        expr->kind = EXPR_CONSTANT;
        expr->expr.constant.kind = CONSTANT_POINTER;
        expr->expr.constant.val.i = 0;
        return;
    case TYPE_STATIC_ARRAY:
    case TYPE_STRUCT:
    case TYPE_UNION:
        expr->kind = EXPR_ZEROED_OUT;
        return;
    case TYPE_INVALID:
    case TYPE_VOID:
    case TYPE_RUNTIME_ARRAY:
    case TYPE_ALIAS:
    case TYPE_ALIAS_DISTINCT:
    case TYPE_ENUM:
    case TYPE_ENUM_DISTINCT:
    case TYPE_AUTO:
    case TYPE_INIT_LIST:
    case TYPE_PS_ARRAY:
    case TYPE_PS_USER:
    case TYPE_STRING_LIT:
    case TYPE_TYPEOF:
    case __TYPE_COUNT:
        break;
    }
    SIC_UNREACHABLE();
}
