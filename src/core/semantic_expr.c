#include "semantics.h"
#include <float.h>

static bool analyze_expr_dispatch(ASTExpr* expr);
static bool analyze_rvalue_dispatch(ASTExpr* expr, bool mutate);
static bool analyze_lvalue_dispatch(ASTExpr* expr, bool will_write);

static bool analyze_var_rvalue(ASTExpr* expr);

// Expr kind functions
static bool analyze_array_access(ASTExpr* expr);
static bool analyze_array_init_list(ASTExpr* expr);
static bool analyze_binary(ASTExpr* expr);
static bool analyze_call(ASTExpr* expr);
static bool analyze_ident(ASTExpr* expr);
static bool analyze_struct_init_list(ASTExpr* expr);
static bool analyze_ternary(ASTExpr* expr);
static bool analyze_unary(ASTExpr* expr);
static bool analyze_unresolved_arrow(ASTExpr* expr);
static bool analyze_unresolved_dot(ASTExpr* expr);
static bool analyze_ct_alignof(ASTExpr* expr);
static bool analyze_ct_offsetof(ASTExpr* expr);
static bool analyze_ct_sizeof(ASTExpr* expr);
static bool analyze_ct_type_max(ASTExpr* expr);
static bool analyze_ct_type_min(ASTExpr* expr);

// Binary functions
static bool analyze_add(ASTExpr* expr, ASTExpr* lhs, ASTExpr* rhs);
static bool analyze_sub(ASTExpr* expr, ASTExpr* lhs, ASTExpr* rhs);
static bool analyze_mul(ASTExpr* expr, ASTExpr* lhs, ASTExpr* rhs);
static bool analyze_div(ASTExpr* expr, ASTExpr* lhs, ASTExpr* rhs);
static bool analyze_mod(ASTExpr* expr, ASTExpr* lhs, ASTExpr* rhs);
static bool analyze_logical(ASTExpr* expr, ASTExpr* lhs, ASTExpr* rhs);
static bool analyze_comparison(ASTExpr* expr, ASTExpr* lhs, ASTExpr* rhs, bool is_eq_ne);
static bool analyze_eq_ne(ASTExpr* expr, ASTExpr* lhs, ASTExpr* rhs);
static bool analyze_lt_ge(ASTExpr* expr, ASTExpr* lhs, ASTExpr* rhs);
static bool analyze_le_gt(ASTExpr* expr, ASTExpr* lhs, ASTExpr* rhs);
static bool analyze_shift(ASTExpr* expr, ASTExpr* lhs, ASTExpr* rhs);
static bool analyze_bit_op(ASTExpr* expr, ASTExpr* lhs, ASTExpr* rhs);
static bool analyze_bit_or(ASTExpr* expr, ASTExpr* lhs, ASTExpr* rhs);
static bool analyze_bit_xor(ASTExpr* expr, ASTExpr* lhs, ASTExpr* rhs);
static bool analyze_bit_and(ASTExpr* expr, ASTExpr* lhs, ASTExpr* rhs);
static bool analyze_assign(ASTExpr* expr, ASTExpr* lhs, ASTExpr* rhs);
static bool analyze_op_assign(ASTExpr* expr, ASTExpr* lhs, ASTExpr* rhs);

// Unary functions
static bool analyze_addr_of(ASTExpr* expr, ASTExpr* inner);
static bool analyze_bit_not(ASTExpr* expr, ASTExpr** inner_ref);
static bool analyze_deref(ASTExpr* expr, ASTExpr* inner);
static bool analyze_incdec(ASTExpr* expr, ASTExpr* inner);
static bool analyze_log_not(ASTExpr* expr, ASTExpr** inner_ref);
static bool analyze_negate(ASTExpr* expr, ASTExpr** inner_ref);

static bool basic_arith_type_conv(ASTExpr* expr1, ASTExpr* expr2, SourceLoc loc);
static void convert_to_const_zero(ASTExpr* expr, Type* type);

bool analyze_expr(ASTExpr* expr)
{
    DBG_ASSERT(expr != NULL);
    if(expr->is_evaluated) return !expr_is_bad(expr);
    bool success = analyze_expr_dispatch(expr);
    DBG_ASSERT(!success || (expr->type != NULL && expr->type->status != STATUS_UNRESOLVED));
    expr->is_evaluated = true;
    if(!success) expr->kind = EXPR_INVALID;
    return success;
}

bool analyze_rvalue(ASTExpr* expr)
{
    DBG_ASSERT(expr != NULL);
    return analyze_expr(expr) && analyze_rvalue_dispatch(expr, true);
}

bool analyze_rvalue_no_mutate(ASTExpr* expr)
{
    DBG_ASSERT(expr != NULL);
    return analyze_expr(expr) && analyze_rvalue_dispatch(expr, false);
}

bool analyze_lvalue(ASTExpr* expr, bool will_write)
{
    DBG_ASSERT(expr != NULL);
    if(expr->is_evaluated) return !expr_is_bad(expr);
    bool success = analyze_lvalue_dispatch(expr, will_write);
    DBG_ASSERT(!success || (expr->type != NULL && expr->type->status == STATUS_RESOLVED));
    expr->is_evaluated = true;
    if(!success) expr->kind = EXPR_INVALID;
    return success;
}

static bool analyze_expr_dispatch(ASTExpr* expr)
{
    switch(expr->kind)
    {
    case EXPR_INVALID:
        return false;
    case EXPR_ARRAY_ACCESS:
        return analyze_array_access(expr);
    case EXPR_ARRAY_INIT_LIST:
        return analyze_array_init_list(expr);
    case EXPR_BINARY:
        return analyze_binary(expr);
    case EXPR_CAST:
        return analyze_explicit_cast(expr);
    case EXPR_CONSTANT:
        return true;
    case EXPR_FUNC_CALL:
        return analyze_call(expr);
    case EXPR_RANGE:
        SIC_TODO();
    case EXPR_STRUCT_INIT_LIST:
        return analyze_struct_init_list(expr);
    case EXPR_TERNARY:
        return analyze_ternary(expr);
    case EXPR_TUPLE:
        SIC_TODO();
    case EXPR_POSTFIX:
    case EXPR_UNARY:
        return analyze_unary(expr);
    case EXPR_UNRESOLVED_ARROW:
        return analyze_unresolved_arrow(expr);
    case EXPR_UNRESOLVED_DOT:
        return analyze_unresolved_dot(expr);
    case EXPR_UNRESOLVED_IDENT:
        return analyze_ident(expr);
    case EXPR_CT_ALIGNOF:
        return analyze_ct_alignof(expr);
    case EXPR_CT_OFFSETOF:
        return analyze_ct_offsetof(expr);
    case EXPR_CT_SIZEOF:
        return analyze_ct_sizeof(expr);
    case EXPR_CT_TYPE_MAX:
        return analyze_ct_type_max(expr);
    case EXPR_CT_TYPE_MIN:
        return analyze_ct_type_min(expr);
    case EXPR_FUNCTION:
    case EXPR_MEMBER_ACCESS:
    case EXPR_TYPE_IDENT:
    case EXPR_VAR:
    case EXPR_ZEROED_OUT:
        break;
    }
    SIC_UNREACHABLE();
}

static bool analyze_rvalue_dispatch(ASTExpr* expr, bool mutate)
{
    (void)mutate;
    // The expressions must always be evaluated prior to calling this function.
    DBG_ASSERT(expr->is_evaluated);
    switch(expr->kind)
    {
    case EXPR_INVALID:
        return false;
    case EXPR_ARRAY_ACCESS:
        return analyze_rvalue_dispatch(expr->expr.array_access.array_expr, mutate);
    case EXPR_FUNC_CALL:
        if(expr->type->canonical->kind == TYPE_VOID)
        {
            sic_error_at(expr->loc, "Function call does not return a value.");
            return false;
        }
        return true;
    case EXPR_FUNCTION:
        sic_error_at(expr->loc, "Functions cannot be used directly. If you want to get the address do '&func'.");
        return false;
    case EXPR_MEMBER_ACCESS:
        return analyze_rvalue_dispatch(expr->expr.member_access.parent_expr, mutate);
    case EXPR_RANGE:
    case EXPR_STRUCT_INIT_LIST:
    case EXPR_TUPLE:
        SIC_TODO();
    case EXPR_TYPE_IDENT:
        sic_error_at(expr->loc, "Types cannot be used as values.");
        return false;
    case EXPR_VAR:
        return analyze_var_rvalue(expr);
    case EXPR_ARRAY_INIT_LIST:
    case EXPR_BINARY:
    case EXPR_CAST:
    case EXPR_CONSTANT:
    case EXPR_POSTFIX:
    case EXPR_TERNARY:
    case EXPR_UNARY:
    case EXPR_ZEROED_OUT:
        return true;
    case EXPR_UNRESOLVED_ARROW:
    case EXPR_UNRESOLVED_DOT:
    case EXPR_UNRESOLVED_IDENT:
    case EXPR_CT_ALIGNOF:
    case EXPR_CT_OFFSETOF:
    case EXPR_CT_SIZEOF:
    case EXPR_CT_TYPE_MAX:
    case EXPR_CT_TYPE_MIN:
        break;
    }
    SIC_UNREACHABLE();
}

static bool analyze_lvalue_dispatch(ASTExpr* expr, bool will_write)
{
RETRY:
    switch(expr->kind)
    {
    case EXPR_INVALID:
        return false;
    case EXPR_ARRAY_ACCESS: {
        ASTExprAAccess access = expr->expr.array_access;
        if(!analyze_expr(expr)) return false;
        if(!analyze_lvalue_dispatch(access.array_expr, will_write)) return false;
        expr->is_const_eval = access.array_expr->is_const_eval && access.index_expr->is_const_eval;
        break;
    }
    case EXPR_FUNCTION:
        if(will_write)
        {
            sic_error_at(expr->loc, "Function identifiers are not mutable.");
            return false;
        }
        break;
    case EXPR_MEMBER_ACCESS:
        if(!analyze_lvalue_dispatch(expr->expr.member_access.parent_expr, will_write)) return false;
        expr->is_const_eval = expr->expr.member_access.parent_expr->is_const_eval;
        break;
    case EXPR_UNARY:
        if(!analyze_expr(expr)) return false;
        if(expr->expr.unary.kind != UNARY_DEREF) goto ERR;
        break;
    case EXPR_VAR: {
        ObjVar* var = expr->expr.var;
        if(var->binding_kind == VAR_BINDING_CT_CONST)
        {
            sic_error_at(expr->loc, "Variables declared with #const %s.", will_write ? "have no storage" : "cannot be modified");
            return false;
        }
        if(var->kind == VAR_GLOBAL)
            expr->is_const_eval = true;
        var->written = true;
        break;
    }
    case EXPR_UNRESOLVED_ARROW:
    case EXPR_UNRESOLVED_DOT:
        if(!analyze_expr(expr)) return false;
        goto RETRY;
    case EXPR_UNRESOLVED_IDENT:
        if(!analyze_ident(expr)) return false;
        goto RETRY;
    default:
        if(!analyze_expr(expr)) return false;
        goto ERR;
    }

    if(expr->type->kind != TYPE_INVALID && will_write && (expr->type->qualifiers & TYPE_QUAL_CONST))
    {
        sic_error_at(expr->loc, "Expression of type \'%s\' cannot be modified.", type_to_string(expr->type));
        return false;
    }

    return true;
ERR:
    sic_error_at(expr->loc, will_write ? "Expression is not assignable." : "Expression is not an lvalue.");
    return false;
}

static bool analyze_var_rvalue(ASTExpr* expr)
{
    ObjVar* var = expr->expr.var;
    var->read = true;
    switch(var->kind)
    {
    case VAR_GLOBAL:
        if(var->binding_kind == VAR_BINDING_CT_CONST || g_sema.in_global_init)
        {
            if(!analyze_global_var(var)) return false;
            // FIXME: This might need to be a deep copy. For now I am shallow copying
            // becuase I don't see a case where deep copying is strictly necessary.
            if(var->initial_val)
                expr_copy(expr, var->initial_val);
            else
                convert_to_const_zero(expr, var->type_loc.type);
        }
        return true;
    case VAR_LOCAL:
        if(var->binding_kind == VAR_BINDING_CT_CONST)
        {
            DBG_ASSERT(var->initial_val != NULL);
            expr_copy(expr, var->initial_val);
        }
        return true;
    default:
        return true;
    }
}


static bool analyze_array_access(ASTExpr* expr)
{
    ASTExpr* arr_expr = expr->expr.array_access.array_expr;
    ASTExpr* index_expr = expr->expr.array_access.index_expr;
    bool valid;
    valid = analyze_expr(arr_expr);
    valid &= implicit_cast(index_expr, g_type_usize);
    if(!valid) return false;
    Type* arr_t = type_reduce(arr_expr->type);
    Type* elem_type;

    switch(arr_t->kind)
    {
    case TYPE_POINTER:
        elem_type = arr_t->pointer_base;
        // TODO: Change this to have *[*]T and *[N]T be allowed, but *T not allowed (*T will mean pointer to SINGLE T).
        if(!resolve_type(&elem_type, TYPE_RES_NORMAL, expr->loc, "Cannot access elements of type %s.")) return false;
        if(!analyze_rvalue(arr_expr)) return false;
        break;
    case TYPE_STATIC_ARRAY:
        elem_type = type_apply_qualifiers(arr_t->array.elem_type, arr_expr->type->qualifiers);
        break;
    case TYPE_INIT_LIST:
        sic_error_at(expr->loc, 
                     "Cannot access element of array literal. Please first assign the array to a const "
                     "literal or cast it to a typed array (i.e. int[*]), then perform the access.");
        return false;
    default:
        sic_error_at(expr->loc, "Attempted to access element of non-array and non-pointer type \'%s\'",
                     type_to_string(arr_expr->type));
        return false;
    }


    if(index_expr->kind == EXPR_CONSTANT)
    {
        uint64_t idx = index_expr->expr.constant.i.lo;
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
                    expr_copy(expr, list->data[i].init_value);
                    return true;
                }
            }

            convert_to_const_zero(expr, elem_type);
            return true;
        }
    }

    expr->type = elem_type;
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
            if(!implicit_cast(entry->arr_index, g_type_usize))
                valid = false;
            else if(entry->arr_index->kind != EXPR_CONSTANT)
            {
                sic_error_at(entry->arr_index->loc, "Array index must be a constant value.");
                valid = false;
            }
            else
            {
                entry->const_index = entry->arr_index->expr.constant.i.lo;
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

        if(!analyze_rvalue(entry->init_value))
            valid = false;
        else if(!entry->init_value->is_const_eval)
            is_constant = false;

    }

    expr->is_const_eval = is_constant;
    list->max = max;
    return valid;
}

static inline bool analyze_binary(ASTExpr* expr)
{
    ASTExpr* lhs = expr->expr.binary.lhs;
    ASTExpr* rhs = expr->expr.binary.rhs;

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
    if(!analyze_expr(call->func_expr)) return false;
    if(call->func_expr->kind != EXPR_FUNCTION && !analyze_rvalue(call->func_expr)) return false;
    
    Type* func_type = call->func_expr->type->canonical;
    if(func_type->kind != TYPE_FUNC_PTR)
    {
        sic_error_at(expr->loc, "Attempted to call non-function.");
        return false;
    }

    FuncSignature* sig = func_type->func_ptr;
    if(call->args.size < sig->params.size)
    {
        sic_error_at(expr->loc, "Too few arguments passed to function. Expected %s%u, got %u.",
                     sig->is_var_arg ? "at least " : "", sig->params.size, call->args.size);
        return false;
    }
    if(!sig->is_var_arg && call->args.size > sig->params.size)
    {
        sic_error_at(expr->loc, "Too many arguments passed to function. Expected %u, got %u.",
                     sig->params.size, call->args.size);
        return false;
    }

    bool valid = true;
    for(uint32_t i = 0; i < sig->params.size; ++i)
    {
        ObjVar* param = sig->params.data[i];
        if(!implicit_cast(call->args.data[i], param->type_loc.type))
        {
            valid = false;
            continue;
        }
    }

    for(uint32_t i = sig->params.size; i < call->args.size; ++i)
    {
        if(!implicit_cast_vararg(call->args.data[i]))
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
    Object* ident = find_obj(&expr->expr.unresolved_ident);
    if(ident == NULL) return false;

    switch(ident->kind)
    {
    case OBJ_ENUM_VALUE: {
        ObjEnumValue* value = obj_as_enum_value(ident);
        expr->type = value->enum_type;
        convert_to_const_int(expr, value->const_value);
        return true;
    }
    case OBJ_FUNC: {
        ObjFunc* func = obj_as_func(ident);
        if(!analyze_function_signature(func)) return false;
        expr->type = func->func_type;
        expr->kind = EXPR_FUNCTION;
        expr->expr.function = func;
        expr->is_const_eval = true;
        func->used = true;
        return true;
    }
    case OBJ_BITFIELD:
    case OBJ_ENUM:
    case OBJ_STRUCT:
    case OBJ_TYPEDEF:
    case OBJ_UNION:
        expr->type = g_type_invalid;
        expr->kind = EXPR_TYPE_IDENT;
        expr->expr.type_ident = ident;
        return true;
    case OBJ_VAR: {
        ObjVar* var = obj_as_var(ident);
        expr->type = var->type_loc.type;
        expr->kind = EXPR_VAR;
        expr->expr.var = var;
        return true;
    }
    case OBJ_INVALID:
        return false;
    case OBJ_IMPORT:
    case OBJ_MODULE:
        break;
    }
    SIC_UNREACHABLE();
}

static bool analyze_struct_init_list(ASTExpr* expr)
{
    bool valid = true;
    bool seen_named = false;
    bool errored = false;
    bool is_const_eval = true;
    StructInitList* list = &expr->expr.struct_init;
    for(uint32_t i = 0; i < list->size; ++i)
    {
        StructInitEntry* entry = list->data + i;
        if(!analyze_expr(entry->init_value)) valid = false;
        else if(!entry->init_value->is_const_eval)
            is_const_eval = false;
        if(entry->unresolved_member.sym != NULL)
            seen_named = true;
        else if(seen_named && !errored)
        {
            sic_error_at(entry->init_value->loc, "Unnamed members of initializer list must always precede named members.");
            errored = true;
            valid = false;
        }
    }
    expr->is_const_eval = is_const_eval;
    return valid;
}

static bool analyze_ternary(ASTExpr* expr)
{
    ASTExprTernary* tern = &expr->expr.ternary;
    bool valid = true;
    valid &= analyze_rvalue(tern->cond_expr);
    if(tern->then_expr != NULL)
        valid &= analyze_rvalue(tern->then_expr);
    else
        tern->then_expr = tern->cond_expr;
    valid &= analyze_rvalue(tern->else_expr);
    if(!valid) return false;

    SIC_TODO();
    // if(!type_equal(tern->then_expr->type, tern->else_expr->type) &&
    //    !arith_type_conv(&tern->then_expr, &tern->else_expr, expr->loc))
    //     return false;

    expr->type = tern->else_expr->type;
    return implicit_cast(tern->cond_expr, g_type_bool);
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
        return analyze_bit_not(expr, &expr->expr.unary.inner);
    case UNARY_DEC:
    case UNARY_INC:
        return analyze_incdec(expr, inner);
    case UNARY_DEREF:
        return analyze_deref(expr, inner);
    case UNARY_LOG_NOT:
        return analyze_log_not(expr, &expr->expr.unary.inner);
    case UNARY_NEG:
        return analyze_negate(expr, &expr->expr.unary.inner);
    }
    SIC_UNREACHABLE();
}

static bool resolve_member(ASTExpr* expr)
{
    SymbolLoc member = expr->expr.unresolved_access.member;
    ASTExpr* parent = expr->expr.unresolved_access.parent_expr;
    DBG_ASSERT(parent->type->status == STATUS_RESOLVED);
    Type* t = parent->type->canonical;
    switch(t->kind)
    {
    case TYPE_STATIC_ARRAY:
        if(member.sym == g_sym_len)
        {
            expr->type = g_type_usize;
            convert_to_const_int(expr, i128_from_u64(t->array.static_len));
            return true;
        }
        break;
    case TYPE_SLICE:
        SIC_TODO();
    case TYPE_STRUCT:
    case TYPE_UNION: {
        const ObjVarDA members = t->struct_->members;
        for(uint32_t i = 0; i < members.size; ++i)
            if(members.data[i]->header.symbol == member.sym)
            {
                expr->kind = EXPR_MEMBER_ACCESS;
                expr->expr.member_access.parent_expr = parent;
                expr->expr.member_access.member = members.data[i];
                expr->expr.member_access.member_idx = i;
                expr->type = type_apply_qualifiers(members.data[i]->type_loc.type, parent->type->qualifiers);
                return true;
            }
        break;
    }
    case TYPE_VOID:
    case NUMERIC_TYPES:
    case TYPE_POINTER:
    case TYPE_FUNC_PTR:
    case TYPE_ALIAS_DISTINCT:
    case TYPE_ENUM_DISTINCT:
    case TYPE_INIT_LIST:
    case TYPE_STRING_LITERAL:
        break;
    case TYPE_INVALID:
    case TYPE_INFERRED_ARRAY:
    case TYPE_ALIAS:
    case TYPE_ENUM:
    case TYPE_UNRESOLVED_USER:
    case TYPE_TYPEOF:
    case __TYPE_COUNT:
        SIC_UNREACHABLE();
    }

    sic_error_at(member.loc, "Type \'%s\' has no member \'%s\'.",
                 type_to_string(parent->type), member.sym);
    return NULL;
}

static bool analyze_unresolved_arrow(ASTExpr* expr)
{
    ASTExpr* deref = CALLOC_STRUCT(ASTExpr);
    deref->loc = expr->loc;
    deref->kind = EXPR_UNARY;
    deref->expr.unary.kind = UNARY_DEREF;
    deref->expr.unary.inner = expr->expr.unresolved_access.parent_expr;
    expr->expr.unresolved_access.parent_expr = deref;

    if(!analyze_expr(deref)) return false;
    return resolve_member(expr);
}

static bool analyze_unresolved_dot(ASTExpr* expr)
{
    ASTExprUAccess* uaccess = &expr->expr.unresolved_access;
    ASTExpr* parent = uaccess->parent_expr;
    if(!analyze_expr(parent)) return false;

    if(parent->kind == EXPR_TYPE_IDENT)
    {
        ObjEnum* enum_ = obj_as_enum(parent->expr.type_ident);
        const ObjEnumValueDA values = enum_->values;
        for(uint32_t i = 0; i < values.size; ++i)
            if(values.data[i]->header.symbol == uaccess->member.sym)
            {
                expr->type = enum_->type_ref;
                convert_to_const_int(expr, values.data[i]->const_value);
                return true;
            }
        sic_error_at(uaccess->member.loc, "Enum \'%s\' has no value \'%s\'.",
                     enum_->header.symbol, uaccess->member.sym);
        return false;
    }

    if(parent->type->canonical->kind == TYPE_POINTER)
    {
        ASTExpr* deref = CALLOC_STRUCT(ASTExpr);
        deref->loc = expr->loc;
        deref->kind = EXPR_UNARY;
        deref->expr.unary.kind = UNARY_DEREF;
        deref->expr.unary.inner = parent;
        parent = uaccess->parent_expr = deref;
        if(!analyze_expr(parent)) return false;
    }

    if(!resolve_member(expr)) return false;
    return true;
}

static bool analyze_ct_alignof(ASTExpr* expr)
{
    if(!resolve_type(&expr->expr.ct_typearg.type, TYPE_RES_NORMAL, expr->expr.ct_typearg.loc, "Cannot get alignment of type"))
        return false;

    ByteSize align = type_alignment(expr->expr.ct_typearg.type);
    expr->type = g_type_usize;
    convert_to_const_int(expr, i128_from_u64(align));
    return true;
}

static bool analyze_ct_offsetof(ASTExpr* expr)
{
    ASTExprCTOffset* off = &expr->expr.ct_offsetof;
    if(!resolve_type(&off->struct_.type, TYPE_RES_NORMAL, off->struct_.loc, "Cannot get offsetof member with base type %s."))
        return false;
    if(off->struct_.type->canonical->kind != TYPE_STRUCT)
    {

    }
    return true;
}

static bool analyze_ct_sizeof(ASTExpr* expr)
{
    if(!resolve_type(&expr->expr.ct_typearg.type, TYPE_RES_NORMAL, expr->expr.ct_typearg.loc, "Cannot get size of type %s."))
        return false;
    
    uint32_t size = type_size(expr->expr.ct_typearg.type);
    expr->type = g_type_usize;
    convert_to_const_int(expr, i128_from_u64(size));
    return true;
}

static bool analyze_ct_type_max(ASTExpr* expr)
{
    if(!resolve_type(&expr->expr.ct_typearg.type, TYPE_RES_NORMAL, expr->expr.ct_typearg.loc, "Cannot get max of type %s."))
        return false;
    Type* type = expr->type = expr->expr.ct_typearg.type;

RETRY:
    switch(type->kind)
    {
    case TYPE_BYTE:
        convert_to_const_int(expr, i128_from_u64(INT8_MAX));
        return true;
    case TYPE_SHORT:
        convert_to_const_int(expr, i128_from_u64(INT16_MAX));
        return true;
    case TYPE_INT:
        convert_to_const_int(expr, i128_from_u64(INT32_MAX));
        return true;
    case TYPE_LONG:
        convert_to_const_int(expr, i128_from_u64(INT64_MAX));
        return true;
    case TYPE_INT128:
        convert_to_const_int(expr, INT128_MAX);
        return true;
    case TYPE_UBYTE:
        convert_to_const_int(expr, i128_from_u64(UINT8_MAX));
        return true;
    case TYPE_USHORT:
        convert_to_const_int(expr, i128_from_u64(UINT16_MAX));
        return true;
    case TYPE_UINT:
        convert_to_const_int(expr, i128_from_u64(UINT32_MAX));
        return true;
    case TYPE_ULONG:
        convert_to_const_int(expr, i128_from_u64(UINT64_MAX));
        return true;
    case TYPE_UINT128:
        convert_to_const_int(expr, UINT128_MAX);
        return true;
    case TYPE_FLOAT:
        convert_to_const_float(expr, (double)FLT_MAX);
        return true;
    case TYPE_DOUBLE:
        convert_to_const_float(expr, DBL_MAX);
        return true;
    case TYPE_ALIAS:
        type = type->typedef_->alias.type;
        goto RETRY;
    case TYPE_ENUM:
    case TYPE_ENUM_DISTINCT: {
        ObjEnum* enum_ = type->enum_;
        // FIXME: Figure out what we are doing with enums. Should they be converted to int with type enum,
        // or should they actually be their own type of constant.
        convert_to_const_int(expr, enum_->values.data[enum_->max_idx]->const_value);
        return true;
    }
    default:
        sic_error_at(expr->loc, "Cannot get max of type %s.", type_to_string(expr->type));
        return false;
    }
}

static bool analyze_ct_type_min(ASTExpr* expr)
{
    if(!resolve_type(&expr->expr.ct_typearg.type, TYPE_RES_NORMAL, expr->expr.ct_typearg.loc, "Cannot get min of type %s."))
        return false;
    Type* type = expr->type = expr->expr.ct_typearg.type;

RETRY:
    switch(type->kind)
    {
    case TYPE_BYTE:
        convert_to_const_int(expr, i128_from_s64(INT8_MIN));
        return true;
    case TYPE_SHORT:
        convert_to_const_int(expr, i128_from_s64(INT16_MIN));
        return true;
    case TYPE_INT:
        convert_to_const_int(expr, i128_from_s64(INT32_MIN));
        return true;
    case TYPE_LONG:
        convert_to_const_int(expr, i128_from_s64(INT64_MIN));
        return true;
    case TYPE_INT128:
        convert_to_const_int(expr, INT128_MIN);
        return true;
    case TYPE_UBYTE:
    case TYPE_USHORT:
    case TYPE_UINT:
    case TYPE_ULONG:
    case TYPE_UINT128:
        convert_to_const_int(expr, UINT128_MIN);
        return true;
    case TYPE_FLOAT:
        convert_to_const_float(expr, (double)-FLT_MAX);
        return true;
    case TYPE_DOUBLE:
        convert_to_const_float(expr, -DBL_MAX);
        return true;
    case TYPE_ALIAS:
        type = type->typedef_->alias.type;
        goto RETRY;
    case TYPE_ENUM:
    case TYPE_ENUM_DISTINCT: {
        ObjEnum* enum_ = type->enum_;
        // FIXME: Figure out what we are doing with enums. Should they be converted to int with type enum,
        // or should they actually be their own type of constant.
        convert_to_const_int(expr, enum_->values.data[enum_->min_idx]->const_value);
        return true;
    }
    default:
        sic_error_at(expr->loc, "Cannot get min of type %s.", type_to_string(expr->type));
        return false;
    }
}

static bool analyze_pointer_arith(ASTExpr* expr, ASTExpr* pointer, ASTExpr* rhs)
{
    Type* lct = pointer->type->canonical;
    DBG_ASSERT(lct->kind == TYPE_POINTER);
    Type* rt = rhs->type->canonical;
    if(!type_is_integer(rt))
    {
        sic_error_at(expr->loc, "Invalid operand types %s and %s.",
                     type_to_string(pointer->type), type_to_string(rhs->type));
        return false;
    }
    // Make sure pointer's inner type is resolved so we can get its size.
    Type* inner_ty = lct->pointer_base;
    if(!resolve_type(&inner_ty, TYPE_RES_NORMAL, expr->loc, "Cannot perform pointer-arithmetic on pointer to %s.")) 
        return false;

    expr->type = pointer->type;
    // if(pointer->kind == EXPR_CONSTANT && rhs->kind == EXPR_CONSTANT)
    // {
    //     Int128 actual_val_to_add = i128_mult64(rhs->expr.constant.i, type_size(pointer->type->pointer_base));
    //     DBG_ASSERT(expr->kind == EXPR_BINARY);
    //     if(expr->expr.binary.kind == BINARY_SUB)
    //         actual_val_to_add = i128_neg(actual_val_to_add);
    //     convert_to_const_pointer(expr, i128_add(pointer->expr.constant.i, actual_val_to_add));
    //     return true;
    // }
    //
    return implicit_cast(rhs, type_is_unsigned(rt) ? g_type_uptr : g_type_iptr);
}

static bool analyze_add(ASTExpr* expr, ASTExpr* lhs, ASTExpr* rhs)
{
    bool valid = true;
    valid &= analyze_rvalue(lhs);
    valid &= analyze_rvalue(rhs);
    if(!valid) return false;
    Type* lt = lhs->type->canonical;
    Type* rt = lhs->type->canonical;
    bool lhs_is_pointer = lt->kind == TYPE_POINTER;
    bool rhs_is_pointer = rt->kind == TYPE_POINTER;

    // if(type_is_int_literal(lt) && type_is_int_literal(rt))
    // {
    //     Int128 l = lhs->expr.constant.i;
    //     Int128 r = rhs->expr.constant.i;
    //     if(lt == rt) expr->type = lt;
    //     else 
    //     {
    //         if(lt == g_type_neg_int_lit)
    //             expr->type = i128_ucmp(i128_neg(l), r) > 0 ? lt : rt;
    //         else
    //             expr->type = i128_ucmp(l, i128_neg(r)) >= 0 ? lt : rt;
    //     }
    //     convert_to_const_int(expr, i128_add(l, r));
    //     return true;
    // }
    //
    if(rhs_is_pointer)
    {
        lhs_is_pointer = true;
        expr->expr.binary.lhs = rhs;
        expr->expr.binary.rhs = lhs;
        lhs = rhs;
        rhs = expr->expr.binary.lhs;
    }

    if(lhs_is_pointer)
        return analyze_pointer_arith(expr, lhs, rhs);

    if(!basic_arith_type_conv(lhs, rhs, expr->loc))
        return false;

    expr->type = lhs->type;
    // if(lhs->kind == EXPR_CONSTANT && rhs->kind == EXPR_CONSTANT)
    // {
    //     if(lhs->expr.constant.kind == CONSTANT_FLOAT)
    //         convert_to_const_float(expr, lhs->expr.constant.f + rhs->expr.constant.f);
    //     else
    //         convert_to_const_int(expr, i128_add(lhs->expr.constant.i, rhs->expr.constant.i));
    //     return true;
    // }
    // expr->is_const_eval = lhs->is_const_eval && rhs->is_const_eval;
    return true;
}

static bool analyze_sub(ASTExpr* expr, ASTExpr* lhs, ASTExpr* rhs)
{
    bool valid = true;
    valid &= analyze_rvalue(lhs);
    valid &= analyze_rvalue(rhs);
    if(!valid) return false;

    // Type* lt = lhs->type;
    // Type* rt = rhs->type;
    // if(type_is_int_literal(lt) && type_is_int_literal(rt))
    // {
    //     Int128 l = lhs->expr.constant.i;
    //     Int128 r = rhs->expr.constant.i;
    //     if(lt != rt) expr->type = lt;
    //     else 
    //     {
    //         if(lt == g_type_neg_int_lit)
    //             expr->type = i128_scmp(l, r) >= 0 ? g_type_pos_int_lit : lt;
    //         else
    //             expr->type = i128_ucmp(l, r) >= 0 ? lt : g_type_neg_int_lit;
    //     }
    //     convert_to_const_int(expr, i128_sub(l, r));
    //     return true;
    // }
    if(lhs->type->canonical->kind == TYPE_POINTER)
        return analyze_pointer_arith(expr, lhs, rhs);

    if(!basic_arith_type_conv(lhs, rhs, expr->loc)) return false;

    expr->type = lhs->type;
    // if(lhs->kind == EXPR_CONSTANT && rhs->kind == EXPR_CONSTANT)
    // {
    //     if(lhs->expr.constant.kind == CONSTANT_FLOAT)
    //         convert_to_const_float(expr, lhs->expr.constant.f - rhs->expr.constant.f);
    //     else
    //         convert_to_const_int(expr, i128_sub(lhs->expr.constant.i, rhs->expr.constant.i));
    //     return true;
    // }
    // expr->is_const_eval = lhs->is_const_eval && rhs->is_const_eval;
    return true;
}

static bool analyze_mul(ASTExpr* expr, ASTExpr* lhs, ASTExpr* rhs)
{
    bool valid = true;
    valid &= analyze_rvalue(lhs);
    valid &= analyze_rvalue(rhs);
    if(!valid) return false;
    if(!basic_arith_type_conv(lhs, rhs, expr->loc)) return false;

    expr->type = lhs->type;
    // if(lhs->kind == EXPR_CONSTANT && rhs->kind == EXPR_CONSTANT)
    // {
    //     if(lhs->expr.constant.kind == CONSTANT_FLOAT)
    //         convert_to_const_float(expr, lhs->expr.constant.f * rhs->expr.constant.f);
    //     else
    //         convert_to_const_int(expr, i128_mult(lhs->expr.constant.i, rhs->expr.constant.i));
    //     return true;
    // }
    //
    // expr->is_const_eval = lhs->is_const_eval && rhs->is_const_eval;
    return true;
}

static bool analyze_div(ASTExpr* expr, ASTExpr* lhs, ASTExpr* rhs)
{
    bool valid = true;
    valid &= analyze_rvalue(lhs);
    valid &= analyze_rvalue(rhs);
    if(!valid) return false;
    if(!basic_arith_type_conv(lhs, rhs, expr->loc)) return false;

    expr->type = lhs->type;
    // if(lhs->kind == EXPR_CONSTANT && rhs->kind == EXPR_CONSTANT)
    // {
    //     if(lhs->expr.constant.kind == CONSTANT_FLOAT)
    //     {
    //         double rval = rhs->expr.constant.f;
    //         if(rval == 0.0)
    //         {
    //             sic_error_at(expr->loc, "Right side of division evaluates to 0.");
    //             return false;
    //         }
    //         convert_to_const_float(expr, lhs->expr.constant.f / rval);
    //     }
    //     else
    //     {
    //         Int128 rval = rhs->expr.constant.i;
    //         if(rval.hi == 0 && rval.lo == 0)
    //         {
    //             sic_error_at(expr->loc, "Right side of division evaluates to 0.");
    //             return false;
    //         }
    //         convert_to_const_int(expr, i128_div(lhs->expr.constant.i, rval, expr->type->canonical->kind));
    //     }
    //     return true;
    // }
    //
    // expr->is_const_eval = lhs->is_const_eval && rhs->is_const_eval;
    return true;
}

static bool analyze_mod(ASTExpr* expr, ASTExpr* lhs, ASTExpr* rhs)
{
    bool valid = true;
    valid &= analyze_rvalue(lhs);
    valid &= analyze_rvalue(rhs);
    if(!valid) return false;

    if(!type_is_integer(lhs->type->canonical) || !type_is_integer(rhs->type->canonical))
    {
        sic_error_at(expr->loc, "Invalid operand types %s and %s.",
                     type_to_string(lhs->type), type_to_string(rhs->type));
        return false;
    }

    if(!basic_arith_type_conv(lhs, rhs, expr->loc)) return false;

    expr->type = lhs->type;
    // if(lhs->kind == EXPR_CONSTANT && rhs->kind == EXPR_CONSTANT)
    // {
    //     Int128 rval = rhs->expr.constant.i;
    //     if(rval.hi == 0 && rval.lo == 0)
    //     {
    //         sic_error_at(expr->loc, "Right side of modulo evaluates to 0.");
    //         return false;
    //     }
    //     convert_to_const_int(expr, i128_rem(lhs->expr.constant.i, rval, rhs->type->kind));
    //     return true;
    // }
    //
    // expr->is_const_eval = lhs->is_const_eval && rhs->is_const_eval;
    return true;
}

static bool analyze_logical(ASTExpr* expr, ASTExpr* lhs, ASTExpr* rhs)
{
    bool valid = true;
    valid &= analyze_rvalue(lhs);
    valid &= analyze_rvalue(rhs);
    if(!valid) return false;

    if(!implicit_cast(lhs, g_type_bool) || !implicit_cast(rhs, g_type_bool))
        return false;

    expr->type = g_type_bool;

    // if(lhs->kind == EXPR_CONSTANT)
    // {
    //     DBG_ASSERT(lhs->expr.constant.kind == CONSTANT_BOOL);
    //     expr_copy(expr, (expr->expr.binary.kind == BINARY_LOG_OR) ^ lhs->expr.constant.b ? rhs : lhs);
    //     return true;
    // }
    //
    // if(rhs->kind == EXPR_CONSTANT && ((expr->expr.binary.kind == BINARY_LOG_OR) ^ rhs->expr.constant.b))
    // {
    //     DBG_ASSERT(rhs->expr.constant.kind == CONSTANT_BOOL);
    //     expr_copy(expr, lhs);
    //     return true;
    // }
    //
    // expr->is_const_eval = lhs->is_const_eval && rhs->is_const_eval;
    return true;
}

static bool analyze_comparison(ASTExpr* expr, ASTExpr* lhs, ASTExpr* rhs, bool is_eq_ne)
{
    TypeKind lkind = lhs->type->canonical->kind;
    TypeKind rkind = rhs->type->canonical->kind;
    expr->type = g_type_bool;
    if(lkind == TYPE_STRING_LITERAL || rkind == TYPE_STRING_LITERAL)
    {
        // TODO: Remove this. It is temporarily here because the string type is not
        // fleshed out. There are two options: allow comparisons between strings
        // which would resolve to a strcmp under the hood, or disallow them entirely.
        //
        sic_error_at(expr->loc, "String literals cannot be compared.");
        return false;
    }

    switch(lkind)
    {
    case TYPE_BOOL:
        if(rkind != TYPE_BOOL || !is_eq_ne) goto ERR;
        return true;
    case TYPE_CHAR:
    case TYPE_CHAR16:
    case TYPE_CHAR32:
        if(rkind != lkind) goto ERR;
        return true;
    case INT_TYPES:
        if(!type_kind_is_integer(rkind)) goto ERR;
        return basic_arith_type_conv(lhs, rhs, expr->loc);
    case FLOAT_TYPES:
        if(!type_kind_is_float(rkind)) goto ERR;
        return basic_arith_type_conv(lhs, rhs, expr->loc);
    case TYPE_POINTER:
    case TYPE_FUNC_PTR:
        if(rkind != TYPE_POINTER && rkind != TYPE_FUNC_PTR) goto ERR;
        if(!can_cast(rhs, lhs->type, true)) goto ERR;
        perform_cast(rhs, lhs->type);
        return true;
    case TYPE_ALIAS_DISTINCT:
    case TYPE_ENUM_DISTINCT:
        if(!type_equal(lhs->type, rhs->type)) goto ERR;
        return true;
    default:
        goto ERR;
    }

ERR:
    sic_error_at(expr->loc, "Invalid operand types %s and %s.",
                 type_to_string(lhs->type), type_to_string(rhs->type));
    return false;
}

static bool analyze_eq_ne(ASTExpr* expr, ASTExpr* lhs, ASTExpr* rhs)
{
    bool valid = true;
    valid &= analyze_rvalue(lhs);
    valid &= analyze_rvalue(rhs);
    if(!valid) return false;
    if(!analyze_comparison(expr, lhs, rhs, true)) return false;

    // Type* ty = lhs->type->canonical;
    // if(lhs->kind == EXPR_CONSTANT && rhs->kind == EXPR_CONSTANT)
    // {
    //     bool value;
    //     if(type_is_float(ty))
    //         value = lhs->expr.constant.f == rhs->expr.constant.f;
    //     else
    //         value = i128_ucmp(lhs->expr.constant.i, rhs->expr.constant.i) == 0;
    //
    //     convert_to_const_bool(expr, value ^ (expr->expr.binary.kind == BINARY_NE));
    //     return true;
    // }
    //
    // expr->is_const_eval = lhs->is_const_eval && rhs->is_const_eval;
    return true;
}

static bool analyze_lt_ge(ASTExpr* expr, ASTExpr* lhs, ASTExpr* rhs)
{
    bool valid = true;
    valid &= analyze_rvalue(lhs);
    valid &= analyze_rvalue(rhs);
    if(!valid) return false;
    if(!analyze_comparison(expr, lhs, rhs, false)) return false;

    // Type* ty = lhs->type->canonical;
    // if(lhs->kind == EXPR_CONSTANT && rhs->kind == EXPR_CONSTANT)
    // {
    //     bool value;
    //     if(type_is_float(ty))
    //         value = lhs->expr.constant.f < rhs->expr.constant.f;
    //     else
    //         value = i128_cmp(lhs->expr.constant.i, rhs->expr.constant.i, ty->kind) < 0;
    //
    //     convert_to_const_bool(expr, value ^ (expr->expr.binary.kind == BINARY_GE));
    //     return true;
    // }
    //
    // expr->is_const_eval = lhs->is_const_eval && rhs->is_const_eval;
    return true;
}

static bool analyze_le_gt(ASTExpr* expr, ASTExpr* lhs, ASTExpr* rhs)
{
    bool valid = true;
    valid &= analyze_rvalue(lhs);
    valid &= analyze_rvalue(rhs);
    if(!valid) return false;
    if(!analyze_comparison(expr, lhs, rhs, false)) return false;

    // Type* ty = lhs->type->canonical;
    // if(lhs->kind == EXPR_CONSTANT && rhs->kind == EXPR_CONSTANT)
    // {
    //     bool value;
    //     if(type_is_float(ty))
    //         value = lhs->expr.constant.f <= rhs->expr.constant.f;
    //     else
    //         value = i128_cmp(lhs->expr.constant.i, rhs->expr.constant.i, ty->kind) <= 0;
    //
    //     convert_to_const_bool(expr, value ^ (expr->expr.binary.kind == BINARY_GT));
    //     return true;
    // }
    //
    // expr->is_const_eval = lhs->is_const_eval && rhs->is_const_eval;
    return true;
}

static bool analyze_shift(ASTExpr* expr, ASTExpr* lhs, ASTExpr* rhs)
{
    bool valid = true;
    valid &= analyze_rvalue(lhs);
    valid &= analyze_rvalue(rhs);
    if(!valid) return false;

    Type* lhs_ctype = lhs->type->canonical;
    Type* rhs_ctype = rhs->type->canonical;

    if(!type_is_integer(lhs_ctype) || !type_is_integer(rhs_ctype))
    {
        sic_error_at(expr->loc, "Invalid operand types %s and %s.",
                     type_to_string(lhs->type), type_to_string(rhs->type));
        return false;
    }

    if(type_is_int_literal(rhs_ctype) || lhs_ctype->builtin.byte_size != rhs_ctype->builtin.byte_size)
        perform_cast(rhs, lhs_ctype);

    
    expr->type = lhs->type;
    // if(rhs->kind == EXPR_CONSTANT)
    // {
    //     if(i128_ucmp(rhs->expr.constant.i, i128_from_u64(lhs_ctype->builtin.bit_size)) >= 0)
    //         sic_diagnostic_at(DIAG_WARNING, expr->loc, "Shift amount exceeds integer width (%u).",
    //                           lhs_ctype->builtin.bit_size);
    //
    //     if(lhs->kind == EXPR_CONSTANT)
    //     {
    //         Int128 new_val;
    //         BinaryOpKind kind = expr->expr.binary.kind;
    //         if(kind == BINARY_SHL)
    //             new_val = i128_shl(lhs->expr.constant.i, rhs->expr.constant.i);
    //         else if(kind == BINARY_LSHR)
    //             new_val = i128_lshr(lhs->expr.constant.i, rhs->expr.constant.i);
    //         else
    //         {
    //             BitSize shift = 128 - lhs_ctype->builtin.bit_size;
    //             new_val = i128_shl64(lhs->expr.constant.i, shift);
    //             new_val = i128_ashr(new_val, i128_add64(rhs->expr.constant.i, shift));
    //         }
    //         convert_to_const_int(expr, new_val);
    //         return true;
    //     }
    // }
    //
    // expr->is_const_eval = lhs->is_const_eval && rhs->is_const_eval;
    return true;
}

static bool analyze_bit_op(ASTExpr* expr, ASTExpr* lhs, ASTExpr* rhs)
{
    Type* lhs_type = lhs->type;
    Type* rhs_type = rhs->type;
    if(!type_is_integer(lhs_type->canonical) || !type_is_integer(rhs_type->canonical))
    {
        sic_error_at(expr->loc, "Invalid operand types %s and %s.",
                     type_to_string(lhs_type), type_to_string(rhs_type));
        return false;
    }

    if(!basic_arith_type_conv(lhs, rhs, expr->loc)) return false;
    expr->type = lhs->type;
    return true;

}

static bool analyze_bit_or(ASTExpr* expr, ASTExpr* lhs, ASTExpr* rhs)
{
    bool valid = true;
    valid &= analyze_rvalue(lhs);
    valid &= analyze_rvalue(rhs);
    if(!valid) return false;
    if(!analyze_bit_op(expr, lhs, rhs)) return false;

    // if(lhs->kind == EXPR_CONSTANT && rhs->kind == EXPR_CONSTANT)
    // {
    //     convert_to_const_int(expr, i128_or(lhs->expr.constant.i, rhs->expr.constant.i));
    //     return true;
    // }
    //
    // expr->is_const_eval = lhs->is_const_eval && rhs->is_const_eval;
    return true;
}

static bool analyze_bit_xor(ASTExpr* expr, ASTExpr* lhs, ASTExpr* rhs)
{
    bool valid = true;
    valid &= analyze_rvalue(lhs);
    valid &= analyze_rvalue(rhs);
    if(!valid) return false;
    if(!analyze_bit_op(expr, lhs, rhs)) return false;

    // if(lhs->kind == EXPR_CONSTANT && rhs->kind == EXPR_CONSTANT)
    // {
    //     convert_to_const_int(expr, i128_xor(lhs->expr.constant.i, rhs->expr.constant.i));
    //     return true;
    // }
    //
    // expr->is_const_eval = lhs->is_const_eval && rhs->is_const_eval;
    return true;
}
static bool analyze_bit_and(ASTExpr* expr, ASTExpr* lhs, ASTExpr* rhs)
{
    bool valid = true;
    valid &= analyze_rvalue(lhs);
    valid &= analyze_rvalue(rhs);
    if(!valid) return false;
    if(!analyze_bit_op(expr, lhs, rhs)) return false;

    // if(lhs->kind == EXPR_CONSTANT && rhs->kind == EXPR_CONSTANT)
    // {
    //     convert_to_const_int(expr, i128_and(lhs->expr.constant.i, rhs->expr.constant.i));
    //     return true;
    // }
    //
    return true;
}

static bool analyze_assign(ASTExpr* expr, ASTExpr* lhs, ASTExpr* rhs)
{
    if(!analyze_lvalue(lhs, true)) return false;
    expr->type = lhs->type;
    return implicit_cast(rhs, lhs->type);
}

static bool analyze_op_assign(ASTExpr* expr, ASTExpr* lhs, ASTExpr* rhs)
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
    // FIXME: Not sure if this works. We might need to make a deep copy of lhs here
    new_rhs->expr.binary.lhs = lhs;
    new_rhs->expr.binary.rhs = rhs;
    expr->expr.binary.kind = BINARY_ASSIGN;
    expr->expr.binary.rhs = new_rhs;
    return analyze_assign(expr, lhs, new_rhs);
}

static bool analyze_addr_of(ASTExpr* expr, ASTExpr* inner)
{
    if(!analyze_lvalue(inner, false)) return false;
    expr->is_const_eval = inner->is_const_eval;

    if(inner->kind == EXPR_FUNCTION)
    {
        expr->type = inner->type;
        return true;
    }

    expr->type = type_pointer_to(type_is_array(inner->type) ? inner->type->array.elem_type : inner->type);
    return true;
}

static bool analyze_bit_not(ASTExpr* expr, ASTExpr** inner)
{
    if(!analyze_rvalue(*inner)) return false;
    Type* type = (*inner)->type;
    if(!type_is_integer(type->canonical))
    {
        sic_error_at(expr->loc, "Invalid operand type %s.", 
                     type_to_string(type));
        return false;
    }

    // promote_int_type(inner);
    ASTExpr* in = *inner;

    expr->type = in->type;
    if(in->kind == EXPR_CONSTANT)
    {
        DBG_ASSERT(in->expr.constant.kind == CONSTANT_INTEGER);
        convert_to_const_int(expr, i128_not(in->expr.constant.i));
        return true;
    }

    expr->is_const_eval = in->is_const_eval;
    return true;
}

static bool analyze_deref(ASTExpr* expr, ASTExpr* inner)
{
    if(!analyze_rvalue(inner)) return false;
    Type* ctype = inner->type->canonical;
    if(ctype->kind != TYPE_POINTER)
    {
        sic_error_at(expr->loc, "Cannot dereference non-pointer type %s.",
                     type_to_string(inner->type));
        return false;
    }

    expr->type = ctype->pointer_base;
    if(!resolve_type(&expr->type, TYPE_RES_NORMAL, expr->loc, "Cannot dereference pointer to type %s."))
        return false;
    return true;
}

static bool analyze_incdec(ASTExpr* expr, ASTExpr* inner)
{
    if(!analyze_lvalue(inner, true) || !analyze_rvalue_no_mutate(inner))
        return false;

    Type* ty = inner->type;

    if(!type_is_integer(ty->canonical))
    {
        sic_error_at(expr->loc, "Invalid operand type %s.",
                     type_to_string(ty));
        return false;
    }
    expr->type = ty;
    return true;
}

static bool analyze_log_not(ASTExpr* expr, ASTExpr** inner_ref)
{
    if(!implicit_cast(*inner_ref, g_type_bool)) 
        return false;

    ASTExpr* inner = *inner_ref;

    expr->type = g_type_bool;
    if(inner->kind == EXPR_CONSTANT)
    {
        DBG_ASSERT(inner->expr.constant.kind == CONSTANT_BOOL);
        convert_to_const_bool(expr, !inner->expr.constant.b);
        return true;
    }

    expr->is_const_eval = inner->is_const_eval;
    return true;
}

static bool analyze_negate(ASTExpr* expr, ASTExpr** inner_ref)
{
    ASTExpr* inner = *inner_ref;
    if(!analyze_rvalue(inner)) return false;
    Type* type = inner->type;
    Type* ctype = type->canonical;
    if(!type_is_numeric(ctype))
    {
        sic_error_at(expr->loc, "Cannot negate non-numeric type %s.",
                     type_to_string(type));
        return false;
    }


    // if(type_is_integer(ctype))
        // promote_int_type(inner_ref);

    inner = *inner_ref;
    expr->type = inner->type;

    if(inner->kind == EXPR_CONSTANT)
    {
        if(inner->expr.constant.kind == CONSTANT_FLOAT)
            convert_to_const_float(expr, -inner->expr.constant.f);
        else
            convert_to_const_int(expr, i128_neg(inner->expr.constant.i));

        return true;
    }

    expr->is_const_eval = inner->is_const_eval;
    return true;
}


// For add/sub/mult/div/mod
static bool basic_arith_type_conv(ASTExpr* expr1, ASTExpr* expr2, SourceLoc loc)
{
    Type* t1  = expr1->type->canonical;
    Type* t2  = expr2->type->canonical;
    if(type_is_int_literal(t1))
        return implicit_cast(expr1, expr2->type);
    else if(type_is_int_literal(t2))
        return implicit_cast(expr2, expr1->type);
    TypeKind k1 = t1->kind;
    TypeKind k2 = t2->kind;

    if(!type_kind_is_numeric(k1) || !type_kind_is_numeric(k2))
    {
        sic_error_at(loc, "Invalid operand types %s and %s.",
                     type_to_string(expr1->type), type_to_string(expr2->type));
        return false;
    }

    if(type_kind_is_integer(k1) && type_kind_is_integer(k2) && type_kind_is_signed(k1) != type_kind_is_signed(k2))
    {
        sic_error_at(loc, "You cannot combine signed and unsigned integer types.");
        return false;
    }

    if(k1 < k2)
    {
        ASTExpr* temp = expr1;
        expr1 = expr2;
        expr2 = temp;
        t1 = t2;
        k1 = k2;
    }

    perform_cast(expr2, expr1->type);
    DBG_ASSERT(type_equal(expr1->type, expr2->type));
    return true;
}

void resolve_int_lit_type(ASTExpr* lit)
{
    DBG_ASSERT(lit->kind == EXPR_CONSTANT && lit->expr.constant.kind == CONSTANT_INTEGER);
    DBG_ASSERT(type_is_int_literal(lit->type));
    Int128 val = lit->expr.constant.i;
    if(lit->type == g_type_neg_int_lit)
    {
        DBG_ASSERT(i128_is_neg(val));
        if(i128_scmp(val, i128_from_s64(INT64_MIN)) < 0)
        {
            lit->type = g_type_int128;
            return;
        }
        lit->type = i128_scmp(val, i128_from_s64(INT32_MIN)) < 0 ? g_type_long : g_type_int;
        return;
    }

    if(i128_is_neg(val))
    {
        lit->type = g_type_uint128;
        return;
    }

    if(val.hi > 0 || val.lo > (uint64_t)INT64_MAX)
    {
        lit->type = g_type_int128;
        return;
    }

    lit->type = val.lo > (uint64_t)INT32_MAX ? g_type_long : g_type_int;
}

static void convert_to_const_zero(ASTExpr* expr, Type* type)
{
    expr->type = type;
    type = type_reduce(type);
    switch(type->kind)
    {
    case TYPE_BOOL:
        convert_to_const_bool(expr, false);
        return;
    case CHAR_TYPES:
    case INT_TYPES:
        convert_to_const_int(expr, UINT128_MIN);
        return;
    case FLOAT_TYPES:
        convert_to_const_float(expr, 0.0);
        return;
    case TYPE_POINTER:
    case TYPE_FUNC_PTR:
        convert_to_const_pointer(expr, UINT128_MIN);
        return;
    case TYPE_STATIC_ARRAY:
    case TYPE_STRUCT:
    case TYPE_UNION:
        expr->kind = EXPR_ZEROED_OUT;
        expr->is_const_eval = true;
        return;
    case TYPE_SLICE:
        SIC_TODO();
    case TYPE_INVALID:
    case TYPE_VOID:
    case TYPE_ALIAS:
    case TYPE_ALIAS_DISTINCT:
    case TYPE_ENUM:
    case TYPE_ENUM_DISTINCT:
    case SEMA_ONLY_TYPES:
        break;
    }
    SIC_UNREACHABLE();
}
