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
static bool analyze_conditional(ASTExpr* expr);
static bool analyze_ident(ASTExpr* expr);
static bool analyze_struct_init_list(ASTExpr* expr);
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
static bool analyze_eq_ne(ASTExpr* expr, ASTExpr* lhs, ASTExpr* rhs, BinaryOpKind kind);
static bool analyze_lt_ge(ASTExpr* expr, ASTExpr* lhs, ASTExpr* rhs, BinaryOpKind kind);
static bool analyze_le_gt(ASTExpr* expr, ASTExpr* lhs, ASTExpr* rhs, BinaryOpKind kind);
static bool analyze_shift(ASTExpr* expr, ASTExpr* lhs, ASTExpr* rhs, BinaryOpKind kind);
static bool analyze_bit_op(ASTExpr* expr, ASTExpr* lhs, ASTExpr* rhs);
static bool analyze_bit_or(ASTExpr* expr, ASTExpr* lhs, ASTExpr* rhs);
static bool analyze_bit_xor(ASTExpr* expr, ASTExpr* lhs, ASTExpr* rhs);
static bool analyze_bit_and(ASTExpr* expr, ASTExpr* lhs, ASTExpr* rhs);
static bool analyze_assign(ASTExpr* expr, ASTExpr* lhs, ASTExpr* rhs);
static bool analyze_op_assign(ASTExpr* expr, ASTExpr* lhs, ASTExpr* rhs);

// Unary functions
static bool analyze_addr_of(ASTExpr* expr, ASTExpr* inner);
static bool analyze_bit_not(ASTExpr* expr, ASTExpr* inner);
static bool analyze_deref(ASTExpr* expr, ASTExpr* inner);
static bool analyze_incdec(ASTExpr* expr, ASTExpr* inner);
static bool analyze_log_not(ASTExpr* expr, ASTExpr* inner);
static bool analyze_negate(ASTExpr* expr, ASTExpr* inner);

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
    return analyze_expr(expr) && analyze_lvalue_dispatch(expr, will_write);
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
    case EXPR_CONDITIONAL:
        return analyze_conditional(expr);
    case EXPR_CONSTANT:
        return true;
    case EXPR_FUNC_CALL:
        return analyze_call(expr);
    case EXPR_RANGE:
        SIC_TODO();
    case EXPR_STRUCT_INIT_LIST:
        return analyze_struct_init_list(expr);
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
    case EXPR_POINTER_OFFSET:
    case EXPR_MEMBER_ACCESS:
    case EXPR_METHOD:
    case EXPR_TYPE_IDENT:
    case EXPR_VAR:
    case EXPR_ZEROED_OUT:
        break;
    }
    SIC_UNREACHABLE();
}

static bool analyze_rvalue_dispatch(ASTExpr* expr, bool mutate)
{
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
    case EXPR_METHOD:
        sic_error_at(expr->loc, "Functions cannot be used as values directly. If you want to get the address do '&func'.");
        return false;
    case EXPR_MEMBER_ACCESS:
        return analyze_rvalue_dispatch(expr->expr.member_access.parent_expr, mutate);
    case EXPR_RANGE:
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
    case EXPR_CONDITIONAL:
    case EXPR_CONSTANT:
    case EXPR_POINTER_OFFSET:
    case EXPR_POSTFIX:
    case EXPR_STRUCT_INIT_LIST:
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
    switch(expr->kind)
    {
    case EXPR_INVALID:
        return false;
    case EXPR_ARRAY_ACCESS: {
        ASTExprAAccess* access = &expr->expr.array_access;
        if(!analyze_lvalue_dispatch(access->array_expr, will_write)) return false;
        expr->is_const_eval = access->array_expr->is_const_eval && access->index_expr->is_const_eval;
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
    case EXPR_METHOD:
        sic_error_at(expr->loc, will_write ? "Methods are not mutable." : 
                                             "You cannot get the address of a method accessed "
                                             "from an instance. Instead, use '%s.%s'.", 
                     type_to_string(expr->expr.method_access.parent_expr->type->canonical),
                     expr->expr.method_access.method->header.sym);
        return false;
    case EXPR_TYPE_IDENT:
        sic_error_at(expr->loc, "Type identifier not expected or allowed here.");
        return false;
    case EXPR_UNARY:
        if(!analyze_expr(expr)) return false;
        if(expr->expr.unary.kind != UNARY_DEREF) goto ERR;
        break;
    case EXPR_VAR: {
        ObjVar* var = expr->expr.var;
        if(var->binding_kind == VAR_BINDING_CT_CONST)
        {
            sic_error_at(expr->loc, "Variables declared with #const %s.", 
                         will_write ? "cannot be modified" : "have no storage");
            return false;
        }
        if(var->kind == VAR_GLOBAL)
            expr->is_const_eval = true;
        var->written = true;
        if(!will_write)
            var->read = true; // For address of we count it as a read and a write because we dont track the pointer
        break;
    }
    default:
        goto ERR;
    case EXPR_UNRESOLVED_ARROW:
    case EXPR_UNRESOLVED_DOT:
    case EXPR_UNRESOLVED_IDENT:
    case EXPR_CT_ALIGNOF:
    case EXPR_CT_OFFSETOF:
    case EXPR_CT_SIZEOF:
    case EXPR_CT_TYPE_MAX:
    case EXPR_CT_TYPE_MIN:
        SIC_UNREACHABLE();
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
    Type* arr_type = type_reduce(arr_expr->type);
    Type* elem_type;
    ArrayLength bounds_check;

    switch(arr_type->kind)
    {
    case TYPE_POINTER_SINGLE:
        sic_error_at(expr->loc, "Array access syntax is not allowed for pointer to single element. Use *[N]T for pointer to multiple elements.");
        return false;
    case TYPE_POINTER_MULTI:
        elem_type = arr_type->pointer.base;
        if(!resolve_type(&elem_type, TYPE_RES_NORMAL, expr->loc, "Cannot access elements of type %s.")) return false;
        if(!analyze_rvalue(arr_expr)) return false;
        bounds_check = arr_type->pointer.static_len;
        break;
    case TYPE_STATIC_ARRAY:
        elem_type = type_apply_qualifiers(arr_type->array.elem_type, arr_expr->type->qualifiers);
        bounds_check = arr_type->array.static_len;
        break;
    case TYPE_INIT_LIST:
        sic_error_at(expr->loc, 
                     "Cannot access element of array literal. Please first assign the array to a const "
                     "literal or cast it to a typed array (i.e. [*]int), then perform the access.");
        return false;
    default:
        sic_error_at(expr->loc, "Attempted to access element of non-array and non-pointer type \'%s\'",
                     type_to_string(arr_expr->type));
        return false;
    }


    if(index_expr->kind == EXPR_CONSTANT)
    {
        ArrayLength idx = index_expr->expr.constant.i.lo;
        if(bounds_check != 0 && idx >= bounds_check)
        {
            sic_diagnostic_at(DIAG_WARNING, expr->loc, "Index of access (%lu) will overflow the length of the %s (%lu).",
                              idx, arr_type->kind == TYPE_STATIC_ARRAY ? "array" : "pointer", bounds_check);
        }
        else if(arr_expr->kind == EXPR_ARRAY_INIT_LIST)
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

    BinaryOpKind kind = expr->expr.binary.kind;
    switch(kind)
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
        return analyze_eq_ne(expr, lhs, rhs, kind);
    case BINARY_LT:
    case BINARY_GE:
        return analyze_lt_ge(expr, lhs, rhs, kind);
    case BINARY_LE:
    case BINARY_GT:
        return analyze_le_gt(expr, lhs, rhs, kind);
    case BINARY_SHL:
    case BINARY_LSHR:
    case BINARY_ASHR:
        return analyze_shift(expr, lhs, rhs, kind);
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
    ASTExpr* func_expr = expr->expr.call.func_expr;
    ASTExprDA* args = &expr->expr.call.args;
    if(!analyze_expr(func_expr)) return false;
    Type* func_type;

    switch(func_expr->kind)
    {
    case EXPR_FUNCTION:
        func_type = func_expr->type;
        DBG_ASSERT(func_type->kind == TYPE_FUNC_PTR);
        break;
    case EXPR_METHOD: {
        ASTExpr* parent = func_expr->expr.method_access.parent_expr;
        ObjFunc* method = func_expr->expr.method_access.method;
        func_type = func_expr->type;
        const ObjVarDA params = method->signature.params;
        DBG_ASSERT(func_type->kind == TYPE_FUNC_PTR);
        if(method->is_static)
        {
            sic_error_at(expr->loc, "Cannot call static method '%s', on instance of '%s'.",
                         method->header.sym, type_to_string(parent->type));
            return false;
        }
        if(params.data[0]->type_loc.type->canonical->kind == TYPE_POINTER_SINGLE)
        {
            ASTExpr* inner = parent;
            parent = CALLOC_STRUCT(ASTExpr);
            parent->kind = EXPR_UNARY;
            parent->loc = inner->loc;
            parent->expr.unary.inner = inner;
            parent->expr.unary.kind = UNARY_ADDR_OF;
        }

        da_reserve(args, args->size + 1);
        memcpy(&args->data[1], &args->data[0], sizeof(ASTExpr*) * args->size);
        args->size++;
        args->data[0] = parent;
        func_expr->kind = EXPR_FUNCTION;
        func_expr->expr.function = method;
        break;
    }
    default:
        if(!analyze_rvalue(func_expr)) return false;
        func_type = func_expr->type->canonical;
        if(func_type->kind != TYPE_FUNC_PTR)
        {
            sic_error_at(expr->loc, "Left side of call expression must be a function.");
            return false;
        }
    }
    

    FuncSignature* sig = func_type->func_ptr;
    if(args->size < sig->params.size)
    {
        sic_error_at(expr->loc, "Too few arguments passed to function. Expected %s%u, got %u.",
                     sig->is_var_arg ? "at least " : "", sig->params.size, args->size);
        return false;
    }
    if(!sig->is_var_arg && args->size > sig->params.size)
    {
        sic_error_at(expr->loc, "Too many arguments passed to function. Expected %u, got %u.",
                     sig->params.size, args->size);
        return false;
    }

    bool valid = true;
    for(uint32_t i = 0; i < sig->params.size; ++i)
    {
        ObjVar* param = sig->params.data[i];
        if(!implicit_cast(args->data[i], param->type_loc.type))
        {
            valid = false;
            continue;
        }
    }

    for(uint32_t i = sig->params.size; i < args->size; ++i)
    {
        if(!implicit_cast_vararg(args->data[i]))
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
    const ModulePath path = expr->expr.unresolved_ident;
    if(g_sema.cur_enum != NULL && path.size == 1)
    {
        for(uint32_t i = 0; i < g_sema.cur_enum->values.size; ++i)
        {
            ObjEnumValue* value = g_sema.cur_enum->values.data[i];
            if(value->header.sym == path.data[0].sym)
            {
                if(!analyze_enum_value(g_sema.cur_enum, i))
                {
                    check_circular_def(&value->header, expr->loc);
                    return false;
                }
                convert_to_const_int(expr, g_sema.cur_enum->type_ref, value->const_value);
                return true;
            }
        }
    }

    Object* ident = find_obj(&expr->expr.unresolved_ident);
    if(ident == NULL) return false;

    switch(ident->kind)
    {
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
    case OBJ_ENUM_VALUE:
    case OBJ_IMPORT:
    case OBJ_MODULE:
        break;
    }
    SIC_UNREACHABLE();
}

static bool analyze_struct_init_list(ASTExpr* expr)
{
    const StructInitList list = expr->expr.struct_init;
    bool valid = true;
    bool is_named = list.size > 0 && list.data[0].unresolved_member.sym != NULL;
    bool errored = false;
    bool is_const_eval = true;
    for(uint32_t i = 0; i < list.size; ++i)
    {
        StructInitEntry* entry = list.data + i;
        if(!analyze_rvalue(entry->init_value)) valid = false;
        else if(!entry->init_value->is_const_eval)
            is_const_eval = false;
        if(!errored)
        {
            if(is_named == (entry->unresolved_member.sym == NULL))
            {
                sic_error_at(entry->init_value->loc, "Unnamed members of initializer list cannot be combined with named members.");
                errored = true;
                valid = false;
                continue;
            }
            if(is_named)
            {
                for(uint32_t j = 0; j < i; ++j)
                {
                    if(list.data[j].unresolved_member.sym == entry->unresolved_member.sym)
                    {
                        sic_error_at(entry->unresolved_member.loc, "Duplicate initializer list member \'%s\'.",
                                     entry->unresolved_member.sym);
                        sic_diagnostic_at(DIAG_NOTE, list.data[j].unresolved_member.loc, "Previous definition here.");
                        valid = false;
                    }
                }
            }

        }
    }
    expr->is_const_eval = is_const_eval;
    return valid;
}

static bool analyze_conditional(ASTExpr* expr)
{
    ASTExprCond* cond= &expr->expr.conditional;
    bool valid = true;
    valid &= analyze_rvalue(cond->cond_expr);
    valid &= analyze_rvalue(cond->then_expr);
    valid &= analyze_rvalue(cond->else_expr);
    if(!valid) return false;

    SIC_TODO();
    // if(!type_equal(tern->then_expr->type, tern->else_expr->type) &&
    //    !arith_type_conv(&tern->then_expr, &tern->else_expr, expr->loc))
    //     return false;

    expr->type = cond->else_expr->type;
    return implicit_cast(cond->cond_expr, g_type_bool);
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
        return analyze_bit_not(expr, inner);
    case UNARY_DEC:
    case UNARY_INC:
        return analyze_incdec(expr, inner);
    case UNARY_DEREF:
        return analyze_deref(expr, inner);
    case UNARY_LOG_NOT:
        return analyze_log_not(expr, inner);
    case UNARY_NEG:
        return analyze_negate(expr, inner);
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
            convert_to_const_int(expr, g_type_usize, i128_from_u64(t->array.static_len));
            return true;
        }
        break;
    case TYPE_POINTER_MULTI:
        if(member.sym == g_sym_len && t->pointer.static_len != 0)
        {
            convert_to_const_int(expr, g_type_usize, i128_from_u64(t->pointer.static_len));
            return true;
        }
        break;
    case TYPE_SLICE:
        SIC_TODO();
    case TYPE_STRUCT:
    case TYPE_UNION: {
        const ObjVarDA members = obj_as_struct(t->user_def)->members;
        const ObjFuncDA methods = obj_as_struct(t->user_def)->methods;
        for(uint32_t i = 0; i < members.size; ++i)
        {
            if(members.data[i]->header.sym == member.sym)
            {
                expr->kind = EXPR_MEMBER_ACCESS;
                expr->expr.member_access.parent_expr = parent;
                expr->expr.member_access.member = members.data[i];
                expr->expr.member_access.member_idx = i;
                expr->type = type_apply_qualifiers(members.data[i]->type_loc.type, parent->type->qualifiers);
                return true;
            }
        }
        for(uint32_t i = 0; i < methods.size; ++i)
        {
            if(methods.data[i]->header.sym == member.sym)
            {
                expr->kind = EXPR_METHOD;
                expr->expr.method_access.parent_expr = parent;
                expr->expr.method_access.method = methods.data[i];
                expr->type = methods.data[i]->func_type;
                return true;
            }
        }
        break;
    }
    case TYPE_VOID:
    case NUMERIC_TYPES:
    case TYPE_POINTER_SINGLE:
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
        Object* type_ident = parent->expr.type_ident;
    RETRY:
        switch(type_ident->kind)
        {
        case OBJ_BITFIELD:
            SIC_TODO();
        case OBJ_ENUM: {
            ObjEnum* enum_ = obj_as_enum(type_ident);
            const ObjEnumValueDA values = enum_->values;
            for(uint32_t i = 0; i < values.size; ++i)
                if(values.data[i]->header.sym == uaccess->member.sym)
                {
                    if(!analyze_enum_value(enum_, i))
                    {
                        check_circular_def(&values.data[i]->header, expr->loc);
                        return false;
                    }
                    convert_to_const_int(expr, enum_->type_ref, values.data[i]->const_value);
                    return true;
                }
            sic_error_at(uaccess->member.loc, "Enum \'%s\' has no value \'%s\'.",
                         type_ident->sym, uaccess->member.sym);
            return false;
        }
        case OBJ_STRUCT: {
            if(!analyze_type_obj(type_ident)) return false;
            ObjStruct* struct_ = obj_as_struct(type_ident);
            const ObjVarDA members = struct_->members;
            for(uint32_t i = 0; i < members.size; ++i)
                if(members.data[i]->header.sym == uaccess->member.sym)
                {
                    sic_error_at(uaccess->member.loc, "Struct member must be accessed with an instance.");
                    return false;
                }
            const ObjFuncDA methods = struct_->methods;
            for(uint32_t i = 0; i < methods.size; ++i)
            {
                ObjFunc* func = methods.data[i];
                if(func->header.sym == uaccess->member.sym)
                {
                    expr->type = func->func_type;
                    expr->kind = EXPR_FUNCTION;
                    expr->expr.function = func;
                    expr->is_const_eval = true;
                    func->used = true;
                    return true;
                }
            }
            sic_error_at(uaccess->member.loc, "Struct \'%s\' has no method \'%s\'.",
                         type_ident->sym, uaccess->member.sym);
            return false;
        }
        case OBJ_TYPEDEF:
            if(!analyze_type_obj(type_ident)) return false;
            type_ident = obj_as_typedef(type_ident)->alias.type->user_def;
            goto RETRY;
        case OBJ_UNION:
            SIC_TODO();
        default:
            SIC_UNREACHABLE();
        }
    }

    if(parent->type->canonical->kind == TYPE_POINTER_SINGLE)
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
    convert_to_const_int(expr, g_type_usize, i128_from_u64(align));
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
    convert_to_const_int(expr, g_type_usize, i128_from_u64(size));
    return true;
}

static bool analyze_ct_type_max(ASTExpr* expr)
{
    if(!resolve_type(&expr->expr.ct_typearg.type, TYPE_RES_NORMAL, expr->expr.ct_typearg.loc, "Cannot get max of type %s."))
        return false;
    Type* type = expr->expr.ct_typearg.type;

    Type* t = type;
RETRY:
    switch(t->kind)
    {
    case INT_TYPES:
        convert_to_const_int(expr, type, int_max(t->kind));
        return true;
    case TYPE_FLOAT:
        convert_to_const_float(expr, type, (double)FLT_MAX);
        return true;
    case TYPE_DOUBLE:
        convert_to_const_float(expr, type, DBL_MAX);
        return true;
    case TYPE_ALIAS:
        t = obj_as_typedef(t->user_def)->alias.type;
        goto RETRY;
    case TYPE_ENUM:
    case TYPE_ENUM_DISTINCT: {
        ObjEnum* enum_ = obj_as_enum(t->user_def);
        // This line ensures that the enum has its min and max set.
        if(!analyze_type_obj(&enum_->header)) return false;
        // FIXME: Figure out what we are doing with enums. Should they be converted to int with type enum,
        // or should they actually be their own type of constant.
        convert_to_const_int(expr, type, enum_->values.data[enum_->max_idx]->const_value);
        return true;
    }
    default:
        sic_error_at(expr->loc, "Cannot get max of type %s.", type_to_string(type));
        return false;
    }
}

static bool analyze_ct_type_min(ASTExpr* expr)
{
    if(!resolve_type(&expr->expr.ct_typearg.type, TYPE_RES_NORMAL, expr->expr.ct_typearg.loc, "Cannot get min of type %s."))
        return false;
    Type* type = expr->expr.ct_typearg.type;

    Type* t = type;
RETRY:
    switch(t->kind)
    {
    case INT_TYPES:
        convert_to_const_int(expr, type, int_min(t->kind));
        return true;
    case TYPE_FLOAT:
        convert_to_const_float(expr, type, (double)-FLT_MAX);
        return true;
    case TYPE_DOUBLE:
        convert_to_const_float(expr, type, -DBL_MAX);
        return true;
    case TYPE_ALIAS:
        t = obj_as_typedef(t->user_def)->alias.type;
        goto RETRY;
    case TYPE_ENUM:
    case TYPE_ENUM_DISTINCT: {
        ObjEnum* enum_ = obj_as_enum(t->user_def);
        // This line ensures that the enum has its min and max set.
        if(!analyze_type_obj(&enum_->header)) return false;
        // FIXME: Figure out what we are doing with enums. Should they be converted to int with type enum,
        // or should they actually be their own type of constant.
        convert_to_const_int(expr, type, enum_->values.data[enum_->min_idx]->const_value);
        return true;
    }
    default:
        sic_error_at(expr->loc, "Cannot get min of type %s.", type_to_string(type));
        return false;
    }
}

static bool analyze_pointer_arith(ASTExpr* expr, ASTExpr* pointer, ASTExpr* offset, BinaryOpKind kind)
{
    Type* ptr_type = pointer->type->canonical;
    Type* offset_type = offset->type->canonical;
    DBG_ASSERT(type_is_pointer(ptr_type));
    if(!type_is_integer(offset_type))
    {
        sic_error_at(expr->loc, "Invalid operand types %s and %s.",
                     type_to_string(pointer->type), type_to_string(offset->type));
        return false;
    }
    if(ptr_type->kind == TYPE_POINTER_MULTI)
    {
        sic_error_at(expr->loc, "Cannot perform pointer-arithmetic on multi-pointer. If you want the "
                                "address of a specific element do something like '&ptr[elem]'.");
        return false;
    }
    // Make sure pointer's inner type is resolved so we can get its size.
    Type* inner_type = ptr_type->pointer.base;
    if(!resolve_type(&inner_type, TYPE_RES_NORMAL, expr->loc, "Cannot perform pointer-arithmetic on pointer to %s.")) 
        return false;

    if(!implicit_cast(offset, type_is_unsigned(offset_type) ? g_type_uptr : g_type_iptr)) return false;

    expr->type = pointer->type;
    if(pointer->kind == EXPR_CONSTANT && offset->kind == EXPR_CONSTANT)
    {
        Int128 actual_val_to_add = i128_mult64(offset->expr.constant.i, type_size(inner_type));
        DBG_ASSERT(expr->kind == EXPR_BINARY);
        if(kind == BINARY_SUB)
            actual_val_to_add = i128_neg(actual_val_to_add);
        convert_to_const_pointer(expr, expr->type, i128_add(pointer->expr.constant.i, actual_val_to_add));
        return true;
    }

    if(pointer->kind == EXPR_POINTER_OFFSET)
    {
        expr_copy(expr, pointer);
        *pointer = (ASTExpr) {
            .kind = EXPR_BINARY,
            .loc = expr->loc,
            .expr.binary = {
                .lhs = expr->expr.pointer_offset.offset,
                .rhs = offset,
                .kind = kind,
            }
        };
        if(!analyze_rvalue(pointer)) return false;
        expr->expr.pointer_offset.offset = pointer;
    }
    else
    {
        expr->kind = EXPR_POINTER_OFFSET;
        expr->expr.pointer_offset.pointer = pointer;
        expr->expr.pointer_offset.offset = offset;
    }

    expr->is_const_eval = pointer->is_const_eval & offset->is_const_eval;
    return true;
}

static bool analyze_add(ASTExpr* expr, ASTExpr* lhs, ASTExpr* rhs)
{
    bool valid = true;
    valid &= analyze_rvalue(lhs);
    valid &= analyze_rvalue(rhs);
    if(!valid) return false;
    Type* lt = lhs->type->canonical;
    Type* rt = rhs->type->canonical;

    // Untyped int literal case
    if(type_is_int_literal(lt) && type_is_int_literal(rt))
    {
        Int128 l = lhs->expr.constant.i;
        Int128 r = rhs->expr.constant.i;
        Int128 value = i128_add(l, r);
        bool is_bit_int = false;
        if(lhs->expr.constant.is_bit_int || rhs->expr.constant.is_bit_int)
        {
            expr->type = g_type_pos_int_lit;
            is_bit_int = true;
        }
        else if(lt == rt)
        {
            expr->type = lt;
            if(lt == g_type_neg_int_lit)
            {
                if(i128_scmp(value, l) > 0)
                {
                    sic_diagnostic_at(DIAG_WARNING, expr->loc, "Addition between int literals underflows.");
                    expr->type = g_type_uint128;
                }
            }
            else
            {
                if(i128_ucmp(value, l) < 0)
                {
                    sic_diagnostic_at(DIAG_WARNING, expr->loc, "Addition between int literals overflows.");
                    expr->type = g_type_uint128;
                }
            }
        }
        else 
        {
            if(lt == g_type_neg_int_lit)
                expr->type = i128_ucmp(i128_neg(l), r) > 0 ? lt : rt;
            else
                expr->type = i128_ucmp(l, i128_neg(r)) >= 0 ? lt : rt;
        }
        convert_to_const_int(expr, expr->type, value);
        expr->expr.constant.is_bit_int = is_bit_int;
        return true;
    }

    if(type_is_pointer(lt))
        return analyze_pointer_arith(expr, lhs, rhs, BINARY_ADD);

    if(type_is_pointer(rt))
        return analyze_pointer_arith(expr, rhs, lhs, BINARY_ADD);


    if(!basic_arith_type_conv(lhs, rhs, expr->loc))
        return false;

    expr->type = lhs->type;
    if(lhs->kind == EXPR_CONSTANT && rhs->kind == EXPR_CONSTANT)
    {
        if(lhs->expr.constant.kind == CONSTANT_FLOAT)
            convert_to_const_float(expr, expr->type, lhs->expr.constant.f + rhs->expr.constant.f);
        else
            convert_to_const_int(expr, expr->type, i128_add(lhs->expr.constant.i, rhs->expr.constant.i));
        return true;
    }
    expr->is_const_eval = lhs->is_const_eval & rhs->is_const_eval;
    return true;
}

static bool analyze_sub(ASTExpr* expr, ASTExpr* lhs, ASTExpr* rhs)
{
    bool valid = true;
    valid &= analyze_rvalue(lhs);
    valid &= analyze_rvalue(rhs);
    if(!valid) return false;

    Type* lt = lhs->type->canonical;
    Type* rt = rhs->type->canonical;

    if(type_is_int_literal(lt) && type_is_int_literal(rt))
    {
        Int128 l = lhs->expr.constant.i;
        Int128 r = rhs->expr.constant.i;
        Int128 value = i128_sub(l, r);
        bool is_bit_int = false;
        if(lhs->expr.constant.is_bit_int)
        {
            expr->type = g_type_pos_int_lit;
            is_bit_int = true;
        }
        else if(lt != rt)
        {
            expr->type = lt;
            if(lt == g_type_neg_int_lit)
            {
                if(i128_scmp(value, l) > 0)
                {
                    sic_diagnostic_at(DIAG_WARNING, expr->loc, "Subtraction between int literals underflows.");
                    expr->type = g_type_uint128;
                }
            }
            else
            {
                if(i128_ucmp(value, l) < 0)
                {
                    sic_diagnostic_at(DIAG_WARNING, expr->loc, "Subtraction between int literals overflows.");
                    expr->type = g_type_uint128;
                }
            }
        }
        else 
        {
            if(lt == g_type_neg_int_lit)
                expr->type = i128_scmp(l, r) < 0 ? lt : rt;
            else
                expr->type = i128_ucmp(l, r) >= 0 ? lt : rt;
        }
        convert_to_const_int(expr, expr->type, value);
        expr->expr.constant.is_bit_int = is_bit_int;
        return true;
    }

    if(type_is_pointer(lt))
        return analyze_pointer_arith(expr, lhs, rhs, BINARY_SUB);

    if(!basic_arith_type_conv(lhs, rhs, expr->loc)) return false;

    expr->type = lhs->type;
    if(lhs->kind == EXPR_CONSTANT && rhs->kind == EXPR_CONSTANT)
    {
        if(lhs->expr.constant.kind == CONSTANT_FLOAT)
            convert_to_const_float(expr, expr->type, lhs->expr.constant.f - rhs->expr.constant.f);
        else
            convert_to_const_int(expr, expr->type, i128_sub(lhs->expr.constant.i, rhs->expr.constant.i));
        return true;
    }
    expr->is_const_eval = lhs->is_const_eval & rhs->is_const_eval;
    return true;
}

static bool analyze_mul(ASTExpr* expr, ASTExpr* lhs, ASTExpr* rhs)
{
    bool valid = true;
    valid &= analyze_rvalue(lhs);
    valid &= analyze_rvalue(rhs);
    if(!valid) return false;

    Type* lt = lhs->type->canonical;
    Type* rt = rhs->type->canonical;

    if(type_is_int_literal(lt) && type_is_int_literal(rt))
    {
        Int128 l = lhs->expr.constant.i;
        Int128 r = rhs->expr.constant.i;
        if(i128_is_zero(l) || i128_is_zero(r))
            expr->type = g_type_pos_int_lit;
        else if(lt != rt)
        {
            expr->type = g_type_neg_int_lit;
            bool underflows;
            if(lt == g_type_neg_int_lit)
                underflows = i128_ucmp(r, i128_sdiv(INT128_MIN, l)) > 0;
            else
                underflows = i128_ucmp(l, i128_sdiv(INT128_MIN, r)) > 0;

            if(underflows)
            {
                sic_diagnostic_at(DIAG_WARNING, expr->loc, "Multiplication between int literals underflows.");
                expr->type = g_type_uint128;
            }

        }
        else 
        {
            expr->type = g_type_pos_int_lit;
            bool overflows;
            if(lt == g_type_neg_int_lit)
                overflows = i128_ucmp(i128_neg(l), i128_udiv(UINT128_MAX, i128_neg(r))) > 0;
            else
                overflows = i128_ucmp(l, i128_udiv(UINT128_MAX, r)) > 0;

            if(overflows)
            {
                sic_diagnostic_at(DIAG_WARNING, expr->loc, "Multiplication between int literals overflows.");
                expr->type = g_type_uint128;
            }
        }
        convert_to_const_int(expr, expr->type, i128_mult(l, r));
        return true;
    }
    if(!basic_arith_type_conv(lhs, rhs, expr->loc)) return false;

    expr->type = lhs->type;
    if(lhs->kind == EXPR_CONSTANT && rhs->kind == EXPR_CONSTANT)
    {
        if(lhs->expr.constant.kind == CONSTANT_FLOAT)
            convert_to_const_float(expr, expr->type, lhs->expr.constant.f * rhs->expr.constant.f);
        else
            convert_to_const_int(expr, expr->type, i128_mult(lhs->expr.constant.i, rhs->expr.constant.i));
        return true;
    }

    expr->is_const_eval = lhs->is_const_eval & rhs->is_const_eval;
    return true;
}

static bool analyze_div(ASTExpr* expr, ASTExpr* lhs, ASTExpr* rhs)
{
    bool valid = true;
    valid &= analyze_rvalue(lhs);
    valid &= analyze_rvalue(rhs);
    Type* lt = lhs->type->canonical;
    Type* rt = rhs->type->canonical;
    if(type_is_int_literal(lt) && type_is_int_literal(rt))
    {
        Int128 l = lhs->expr.constant.i;
        Int128 r = rhs->expr.constant.i;
        Int128 value;
        if(i128_is_zero(r))
        {
            sic_error_at(expr->loc, "Right side of integer division evaluates to 0.");
            return false;
        }

        if(i128_is_zero(l))
        {
            expr->type = g_type_pos_int_lit;
            value = UINT128_MIN;
        }
        else if(lt != rt)
        {
            expr->type = g_type_neg_int_lit;
            if(lt == g_type_neg_int_lit)
                value = i128_udiv(i128_neg(l), r);
            else
                value = i128_udiv(l, i128_neg(r));
            value = i128_neg(value);
        }
        else 
        {
            expr->type = g_type_pos_int_lit;
            if(lt == g_type_neg_int_lit)
                value = i128_sdiv(l, r);
            else
                value = i128_udiv(l, r);
        }
        convert_to_const_int(expr, expr->type, value);
        return true;
    }
    if(!valid) return false;
    if(!basic_arith_type_conv(lhs, rhs, expr->loc)) return false;

    expr->type = lhs->type;
    if(lhs->kind == EXPR_CONSTANT && rhs->kind == EXPR_CONSTANT)
    {
        if(lhs->expr.constant.kind == CONSTANT_FLOAT)
            convert_to_const_float(expr, expr->type, lhs->expr.constant.f / rhs->expr.constant.f);
        else
        {
            Int128 rval = rhs->expr.constant.i;
            if(i128_is_zero(rval))
            {
                sic_error_at(expr->loc, "Right side of integer division evaluates to 0.");
                return false;
            }
            convert_to_const_int(expr, expr->type, i128_div(lhs->expr.constant.i, rval, expr->type->canonical->kind));
        }
        return true;
    }

    expr->is_const_eval = lhs->is_const_eval & rhs->is_const_eval;
    return true;
}

static bool analyze_mod(ASTExpr* expr, ASTExpr* lhs, ASTExpr* rhs)
{
    bool valid = true;
    valid &= analyze_rvalue(lhs);
    valid &= analyze_rvalue(rhs);
    if(!valid) return false;
    Type* lt = lhs->type->canonical;
    Type* rt = rhs->type->canonical;
    if(type_is_int_literal(lt) && type_is_int_literal(rt))
    {
        Int128 l = lhs->expr.constant.i;
        Int128 r = rhs->expr.constant.i;
        if(rt == g_type_neg_int_lit) r = i128_neg(r);
        Int128 value;
        if(i128_is_zero(r))
        {
            sic_error_at(expr->loc, "Right side of modulo evaluates to 0.");
            return false;
        }

        if(lt == g_type_pos_int_lit)
        {
            expr->type = g_type_pos_int_lit;
            value = i128_urem(l, r);
        }
        else 
        {
            expr->type = g_type_neg_int_lit;
            value = i128_srem(l, r);
        }
        convert_to_const_int(expr, expr->type, value);
        return true;
    }

    if(!type_is_integer(lt) || !type_is_integer(rt))
    {
        sic_error_at(expr->loc, "Invalid operand types %s and %s.",
                     type_to_string(lhs->type), type_to_string(rhs->type));
        return false;
    }

    if(!basic_arith_type_conv(lhs, rhs, expr->loc)) return false;

    expr->type = lhs->type;
    if(lhs->kind == EXPR_CONSTANT && rhs->kind == EXPR_CONSTANT)
    {
        Int128 rval = rhs->expr.constant.i;
        if(i128_is_zero(rval))
        {
            sic_error_at(expr->loc, "Right side of modulo evaluates to 0.");
            return false;
        }
        convert_to_const_int(expr, expr->type, i128_rem(lhs->expr.constant.i, rval, expr->type->canonical->kind));
        return true;
    }

    expr->is_const_eval = lhs->is_const_eval & rhs->is_const_eval;
    return true;
}

static bool analyze_logical(ASTExpr* expr, ASTExpr* lhs, ASTExpr* rhs)
{
    bool valid = true;
    valid &= implicit_cast(lhs, g_type_bool);
    valid &= implicit_cast(rhs, g_type_bool);
    if(!valid) return false;

    expr->type = g_type_bool;

    if(lhs->kind == EXPR_CONSTANT)
    {
        DBG_ASSERT(lhs->expr.constant.kind == CONSTANT_BOOL);
        expr_copy(expr, (expr->expr.binary.kind == BINARY_LOG_OR) ^ lhs->expr.constant.b ? rhs : lhs);
        return true;
    }

    if(rhs->kind == EXPR_CONSTANT && ((expr->expr.binary.kind == BINARY_LOG_OR) ^ rhs->expr.constant.b))
    {
        DBG_ASSERT(rhs->expr.constant.kind == CONSTANT_BOOL);
        expr_copy(expr, lhs);
        return true;
    }

    expr->is_const_eval = lhs->is_const_eval & rhs->is_const_eval;
    return true;
}

static bool analyze_comparison(ASTExpr* expr, ASTExpr* lhs, ASTExpr* rhs, bool is_eq_ne)
{
    Type* lt = lhs->type->canonical;
    Type* rt = rhs->type->canonical;
    TypeKind lkind = lt->kind;
    TypeKind rkind = rt->kind;
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
        if(rkind != TYPE_BOOL || !is_eq_ne) break;
        return true;
    case TYPE_CHAR:
    case TYPE_CHAR16:
    case TYPE_CHAR32:
        if(rkind != lkind) break;
        return true;
    case INT_TYPES:
        if(!type_kind_is_integer(rkind)) break;
        return basic_arith_type_conv(lhs, rhs, expr->loc);
    case FLOAT_TYPES:
        if(!type_kind_is_float(rkind)) break;
        return basic_arith_type_conv(lhs, rhs, expr->loc);
    case TYPE_POINTER_SINGLE:
    case TYPE_POINTER_MULTI:
        if(lt->pointer.base->canonical->kind == TYPE_VOID) return true;
        if(!type_kind_is_pointer(rkind)) break;
        if(rt->pointer.base->canonical->kind == TYPE_VOID || type_equal(lt->pointer.base, rt->pointer.base)) return true;
        break;
    case TYPE_FUNC_PTR:
        if(type_kind_is_pointer(rkind) && rt->pointer.base->canonical->kind == TYPE_VOID) return true;
        return type_equal(lt, rt);
    case TYPE_ALIAS_DISTINCT:
    case TYPE_ENUM_DISTINCT:
        if(!type_equal(lt, rt)) break;
        return true;
    default:
        break;
    }

    sic_error_at(expr->loc, "Invalid operand types %s and %s.",
                 type_to_string(lhs->type), type_to_string(rhs->type));
    return false;
}

static bool analyze_eq_ne(ASTExpr* expr, ASTExpr* lhs, ASTExpr* rhs, BinaryOpKind kind)
{
    bool valid = true;
    valid &= analyze_rvalue(lhs);
    valid &= analyze_rvalue(rhs);
    if(!valid) return false;
    Type* lt = lhs->type->canonical;
    Type* rt = rhs->type->canonical;

    // Untyped int literal case
    if(type_is_int_literal(lt) && type_is_int_literal(rt))
    {
        Int128 l = lhs->expr.constant.i;
        Int128 r = rhs->expr.constant.i;
        bool value = lt == rt && i128_ucmp(l, r) == 0;
        convert_to_const_bool(expr, g_type_bool, value ^ (kind == BINARY_NE));
        return true;
    }
    if(!analyze_comparison(expr, lhs, rhs, true)) return false;

    lt = lhs->type->canonical;
    if(lhs->kind == EXPR_CONSTANT && rhs->kind == EXPR_CONSTANT)
    {
        bool value;
        if(type_is_float(lt))
            value = lhs->expr.constant.f == rhs->expr.constant.f;
        else
            value = i128_ucmp(lhs->expr.constant.i, rhs->expr.constant.i) == 0;

        convert_to_const_bool(expr, g_type_bool, value ^ (kind == BINARY_NE));
        return true;
    }

    expr->is_const_eval = lhs->is_const_eval & rhs->is_const_eval;
    return true;
}

static bool analyze_lt_ge(ASTExpr* expr, ASTExpr* lhs, ASTExpr* rhs, BinaryOpKind kind)
{
    bool valid = true;
    valid &= analyze_rvalue(lhs);
    valid &= analyze_rvalue(rhs);
    if(!valid) return false;
    Type* lt = lhs->type->canonical;
    Type* rt = rhs->type->canonical;
    // Untyped int literal case
    if(type_is_int_literal(lt) && type_is_int_literal(rt))
    {
        Int128 l = lhs->expr.constant.i;
        Int128 r = rhs->expr.constant.i;
        bool value;
        if(lt == g_type_neg_int_lit)
            value = rt == g_type_pos_int_lit || i128_ucmp(l, r) < 0;
        else
            value = rt == g_type_pos_int_lit && i128_ucmp(l, r) < 0;
        convert_to_const_bool(expr, g_type_bool, value ^ (kind == BINARY_GE));
        return true;
    }
    if(!analyze_comparison(expr, lhs, rhs, false)) return false;

    lt = lhs->type->canonical;
    if(lhs->kind == EXPR_CONSTANT && rhs->kind == EXPR_CONSTANT)
    {
        bool value;
        if(type_is_float(lt))
            value = lhs->expr.constant.f < rhs->expr.constant.f;
        else
            value = i128_cmp(lhs->expr.constant.i, rhs->expr.constant.i, lt->kind) < 0;

        convert_to_const_bool(expr, g_type_bool, value ^ (kind == BINARY_GE));
        return true;
    }

    expr->is_const_eval = lhs->is_const_eval & rhs->is_const_eval;
    return true;
}

static bool analyze_le_gt(ASTExpr* expr, ASTExpr* lhs, ASTExpr* rhs, BinaryOpKind kind)
{
    bool valid = true;
    valid &= analyze_rvalue(lhs);
    valid &= analyze_rvalue(rhs);
    if(!valid) return false;
    Type* lt = lhs->type->canonical;
    Type* rt = rhs->type->canonical;
    // Untyped int literal case
    if(type_is_int_literal(lt) && type_is_int_literal(rt))
    {
        Int128 l = lhs->expr.constant.i;
        Int128 r = rhs->expr.constant.i;
        bool value;
        if(lt == g_type_neg_int_lit)
            value = rt == g_type_pos_int_lit || i128_ucmp(l, r) <= 0;
        else
            value = rt == g_type_pos_int_lit && i128_ucmp(l, r) <= 0;
        convert_to_const_bool(expr, g_type_bool, value ^ (kind == BINARY_GT));
        return true;
    }
    if(!analyze_comparison(expr, lhs, rhs, false)) return false;

    lt = lhs->type->canonical;
    if(lhs->kind == EXPR_CONSTANT && rhs->kind == EXPR_CONSTANT)
    {
        bool value;
        if(type_is_float(lt))
            value = lhs->expr.constant.f <= rhs->expr.constant.f;
        else
            value = i128_cmp(lhs->expr.constant.i, rhs->expr.constant.i, lt->kind) <= 0;

        convert_to_const_bool(expr, g_type_bool, value ^ (kind == BINARY_GT));
        return true;
    }

    expr->is_const_eval = lhs->is_const_eval & rhs->is_const_eval;
    return true;
}

static bool analyze_shift(ASTExpr* expr, ASTExpr* lhs, ASTExpr* rhs, BinaryOpKind kind)
{
    bool valid = true;
    valid &= analyze_rvalue(lhs);
    valid &= analyze_rvalue(rhs);
    if(!valid) return false;

    expr->type = lhs->type;
    Type* lt = lhs->type->canonical;
    Type* rt = rhs->type->canonical;
    if(!type_is_integer(lt) || !type_is_integer(rt))
    {
        sic_error_at(expr->loc, "Invalid operand types %s and %s.",
                     type_to_string(lhs->type), type_to_string(rhs->type));
        return false;
    }

    if(type_is_int_literal(lt) && kind == BINARY_ASHR)
    {
        sic_error_at(expr->loc, "You cannot perform an arithmetic shift right on an "
                                "untyped int literal. Please assign a type first.");
        return false;
    }

    if(rhs->kind == EXPR_CONSTANT)
    {
        Int128 r = rhs->expr.constant.i;
        if(type_is_signed(rt) && i128_is_neg(r))
        {
            sic_error_at(expr->loc, "Shift amount should not be negative.");
            return false;
        }
        if(i128_ucmp64(rhs->expr.constant.i, lt->builtin.bit_size) >= 0)
            sic_diagnostic_at(DIAG_WARNING, expr->loc, "Shift amount >= integer width.");

        if(lhs->kind == EXPR_CONSTANT)
        {
            if(i128_is_zero(r)) 
            {
                expr_copy(expr, lhs);
                return true;
            }
            Int128 new_val;
            if(kind == BINARY_SHL)
                new_val = i128_shl(lhs->expr.constant.i, rhs->expr.constant.i);
            else if(kind == BINARY_LSHR)
                new_val = i128_lshr(lhs->expr.constant.i, rhs->expr.constant.i);
            else
            {
                BitSize shift = 128 - lt->builtin.bit_size;
                new_val = i128_shl64(lhs->expr.constant.i, shift);
                new_val = i128_ashr(new_val, i128_add64(rhs->expr.constant.i, shift));
            }
            convert_to_const_int(expr, expr->type, new_val);
            if(type_is_int_literal(lt))
            {
                expr->type = g_type_pos_int_lit;
                expr->expr.constant.is_bit_int = true;
            }
            return true;
        }
    }

    if(!implicit_cast(rhs, type_to_unsigned(lt))) return false;
    

    expr->is_const_eval = lhs->is_const_eval & rhs->is_const_eval;
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
    Type* lt = lhs->type->canonical;
    Type* rt = rhs->type->canonical;
    // Untyped int literal case
    if(type_is_int_literal(lt) && type_is_int_literal(rt))
    {
        convert_to_const_int(expr, g_type_pos_int_lit, i128_or(lhs->expr.constant.i, rhs->expr.constant.i));
        expr->expr.constant.is_bit_int = true;
        return true;
    }
    if(!analyze_bit_op(expr, lhs, rhs)) return false;

    if(lhs->kind == EXPR_CONSTANT && rhs->kind == EXPR_CONSTANT)
    {
        convert_to_const_int(expr, expr->type, i128_or(lhs->expr.constant.i, rhs->expr.constant.i));
        return true;
    }

    expr->is_const_eval = lhs->is_const_eval & rhs->is_const_eval;
    return true;
}

static bool analyze_bit_xor(ASTExpr* expr, ASTExpr* lhs, ASTExpr* rhs)
{
    bool valid = true;
    valid &= analyze_rvalue(lhs);
    valid &= analyze_rvalue(rhs);
    if(!valid) return false;
    Type* lt = lhs->type->canonical;
    Type* rt = rhs->type->canonical;
    // Untyped int literal case
    if(type_is_int_literal(lt) && type_is_int_literal(rt))
    {
        convert_to_const_int(expr, g_type_pos_int_lit, i128_xor(lhs->expr.constant.i, rhs->expr.constant.i));
        expr->expr.constant.is_bit_int = true;
        return true;
    }
    if(!analyze_bit_op(expr, lhs, rhs)) return false;

    if(lhs->kind == EXPR_CONSTANT && rhs->kind == EXPR_CONSTANT)
    {
        convert_to_const_int(expr, expr->type, i128_xor(lhs->expr.constant.i, rhs->expr.constant.i));
        return true;
    }

    expr->is_const_eval = lhs->is_const_eval & rhs->is_const_eval;
    return true;
}
static bool analyze_bit_and(ASTExpr* expr, ASTExpr* lhs, ASTExpr* rhs)
{
    bool valid = true;
    valid &= analyze_rvalue(lhs);
    valid &= analyze_rvalue(rhs);
    if(!valid) return false;
    Type* lt = lhs->type->canonical;
    Type* rt = rhs->type->canonical;
    // Untyped int literal case
    if(type_is_int_literal(lt) && type_is_int_literal(rt))
    {
        convert_to_const_int(expr, g_type_pos_int_lit, i128_and(lhs->expr.constant.i, rhs->expr.constant.i));
        expr->expr.constant.is_bit_int = true;
        return true;
    }
    if(!analyze_bit_op(expr, lhs, rhs)) return false;

    if(lhs->kind == EXPR_CONSTANT && rhs->kind == EXPR_CONSTANT)
    {
        convert_to_const_int(expr, expr->type, i128_and(lhs->expr.constant.i, rhs->expr.constant.i));
        return true;
    }

    expr->is_const_eval = lhs->is_const_eval & rhs->is_const_eval;
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

    // FIXME: Not sure if this works. We might need to make a deep copy of lhs here
    ASTExpr* new_rhs = CALLOC_STRUCT(ASTExpr);
    new_rhs->kind = EXPR_BINARY;
    new_rhs->loc = expr->loc;
    new_rhs->expr.binary.kind = conversion[expr->expr.binary.kind - BINARY_OP_ASSIGN_START];
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

    Type* inner_type = inner->type;
    if(inner->kind == EXPR_FUNCTION)
    {
        expr->type = inner_type;
        return true;
    }

    inner_type = inner_type->canonical;
    if(inner_type->kind == TYPE_STATIC_ARRAY)
    {
        expr->type = type_pointer_to_multi(inner_type->array.elem_type, NULL);
        expr->type->pointer.static_len = inner_type->array.static_len;
    }
    else
    {
        expr->type = type_pointer_to_single(inner->type);
    }
    return true;
}

static bool analyze_bit_not(ASTExpr* expr, ASTExpr* inner)
{
    if(!analyze_rvalue(inner)) return false;
    Type* type = inner->type;
    if(!type_is_integer(type->canonical))
    {
        sic_error_at(expr->loc, "Invalid operand type %s.", 
                     type_to_string(type));
        return false;
    }

    expr->type = inner->type;
    if(inner->kind == EXPR_CONSTANT)
    {
        DBG_ASSERT(inner->expr.constant.kind == CONSTANT_INTEGER);
        convert_to_const_int(expr, expr->type, i128_not(inner->expr.constant.i));
        return true;
    }

    expr->is_const_eval = inner->is_const_eval;
    return true;
}

static bool analyze_deref(ASTExpr* expr, ASTExpr* inner)
{
    if(!analyze_rvalue(inner)) return false;
    Type* ctype = inner->type->canonical;
    if(ctype->kind == TYPE_POINTER_MULTI)
    {
        sic_error_at(expr->loc, "Dereferencing not allowed for pointer to multiple elements. "
                                "Use array access syntax (e.g. ptr[0]).");
        return false;

    }
    else if(ctype->kind != TYPE_POINTER_SINGLE)
    {
        sic_error_at(expr->loc, "Cannot dereference non-pointer type %s.",
                     type_to_string(inner->type));
        return false;
    }

    expr->type = ctype->pointer.base;
    return resolve_type(&expr->type, TYPE_RES_NORMAL, expr->loc, "Cannot dereference pointer to type %s.");
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

static bool analyze_log_not(ASTExpr* expr, ASTExpr* inner)
{
    if(!implicit_cast(inner, g_type_bool)) 
        return false;

    expr->type = g_type_bool;
    if(inner->kind == EXPR_CONSTANT)
    {
        DBG_ASSERT(inner->expr.constant.kind == CONSTANT_BOOL);
        convert_to_const_bool(expr, g_type_bool, !inner->expr.constant.b);
        return true;
    }

    expr->is_const_eval = inner->is_const_eval;
    return true;
}

static bool analyze_negate(ASTExpr* expr, ASTExpr* inner)
{
    if(!analyze_rvalue(inner)) return false;
    Type* type = inner->type;
    Type* ctype = type->canonical;

    if(type_is_int_literal(ctype))
    {
        Int128 i = inner->expr.constant.i;
        if(ctype == g_type_pos_int_lit && i128_ucmp(i, INT128_MIN) > 0)
            sic_diagnostic_at(DIAG_WARNING, expr->loc, "Negation of integer literal underflows.");
        convert_to_const_int(expr, ctype == g_type_pos_int_lit ? g_type_neg_int_lit : g_type_pos_int_lit, i128_neg(i));
    }

    if(!type_is_numeric(ctype))
    {
        sic_error_at(expr->loc, "Cannot negate non-numeric type %s.",
                     type_to_string(type));
        return false;
    }

    expr->type = inner->type;
    if(inner->kind == EXPR_CONSTANT)
    {
        if(inner->expr.constant.kind == CONSTANT_FLOAT)
            convert_to_const_float(expr, expr->type, -inner->expr.constant.f);
        else
            convert_to_const_int(expr, expr->type, i128_neg(inner->expr.constant.i));

        return true;
    }

    expr->is_const_eval = inner->is_const_eval;
    return true;
}

static bool resolve_int_lit(ASTExpr* lit, bool to_signed)
{
    Type* type = lit->type;
    DBG_ASSERT(type_is_int_literal(type));
    Int128 value = lit->expr.constant.i;
    if(to_signed)
    {
        if(i128_is_neg(value))
        {
            if(!type_is_signed(type)) return false;
            value = i128_not(value);
        }
        if(i128_ucmp64(value, INT64_MAX) > 0)
            return true;
        if(i128_ucmp64(value, INT32_MAX) > 0)
            lit->type = g_type_long;
        else if(i128_ucmp64(value, INT16_MAX) > 0)
            lit->type = g_type_int;
        else if(i128_ucmp64(value, INT8_MAX) > 0)
            lit->type = g_type_short;
        else
            lit->type = g_type_byte;
    }
    else
    {
        if(i128_is_neg(value))
            return type_is_unsigned(type);
        if(value.hi != 0)
            return true;
        if(i128_ucmp64(value, UINT32_MAX) > 0)
            lit->type = g_type_ulong;
        else if(i128_ucmp64(value, UINT16_MAX) > 0)
            lit->type = g_type_uint;
        else if(i128_ucmp64(value, UINT8_MAX) > 0)
            lit->type = g_type_ushort;
        else
            lit->type = g_type_ubyte;
    }
    return true;
}

// For add/sub/mult/div/mod
static bool basic_arith_type_conv(ASTExpr* expr1, ASTExpr* expr2, SourceLoc loc)
{
    Type* t1  = expr1->type->canonical;
    Type* t2  = expr2->type->canonical;
    TypeKind k1 = t1->kind;
    TypeKind k2 = t2->kind;

    if(!type_kind_is_numeric(k1) || !type_kind_is_numeric(k2))
    {
        sic_error_at(loc, "Invalid operand types %s and %s.",
                     type_to_string(expr1->type), type_to_string(expr2->type));
        return false;
    }

    if(type_kind_is_integer(k1) && type_kind_is_integer(k2))
    {
        bool t1_signed = type_kind_is_signed(k1);
        bool t2_signed = type_kind_is_signed(k2);
        if(type_is_int_literal(t1))
        {
            // Both expressions should not be literals. This should be handled elsewhere
            DBG_ASSERT(!type_is_int_literal(t2));
            if(!resolve_int_lit(expr1, t2_signed)) goto INT_ERR;
            t1 = expr1->type;
            k1 = t1->kind;
        }
        else if(type_is_int_literal(t2))
        {
            if(!resolve_int_lit(expr2, t1_signed)) goto INT_ERR;
            t2 = expr2->type;
            k2 = t2->kind;
        }
        else if(t1_signed != t2_signed)
            goto INT_ERR;
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

INT_ERR:
    sic_error_at(loc, "You cannot combine signed and unsigned integer types.");
    return false;
}

static void convert_to_const_zero(ASTExpr* expr, Type* type)
{
    Type* t = type;
RETRY:
    switch(t->kind)
    {
    case TYPE_BOOL:
        convert_to_const_bool(expr, type, false);
        return;
    case CHAR_TYPES:
    case INT_TYPES:
        convert_to_const_int(expr, type, UINT128_MIN);
        return;
    case FLOAT_TYPES:
        convert_to_const_float(expr, type, 0.0);
        return;
    case TYPE_POINTER_SINGLE:
    case TYPE_POINTER_MULTI:
    case TYPE_FUNC_PTR:
        convert_to_const_pointer(expr, type, UINT128_MIN);
        return;
    case TYPE_STATIC_ARRAY:
    case TYPE_ALIAS:
    case TYPE_ALIAS_DISTINCT:
        t = obj_as_typedef(t->user_def)->alias.type;
        goto RETRY;
    case TYPE_ENUM:
    case TYPE_ENUM_DISTINCT:
        t = obj_as_enum(t->user_def)->underlying.type;
        goto RETRY;
    case TYPE_STRUCT:
    case TYPE_UNION:
        expr->type = type;
        expr->kind = EXPR_ZEROED_OUT;
        expr->is_const_eval = true;
        return;
    case TYPE_SLICE:
        SIC_TODO();
    case TYPE_INVALID:
    case TYPE_VOID:
    case SEMA_ONLY_TYPES:
        break;
    }
    SIC_UNREACHABLE();
}
