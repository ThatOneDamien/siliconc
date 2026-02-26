#include "semantics.h"

#define CAST_ERROR(...) do { if(!params->silent) sic_error_at(params->cast_loc, __VA_ARGS__); return false; } while(0)

typedef struct CastParams CastParams;
typedef struct CastRule   CastRule;

struct CastParams
{
    ASTExpr*     cast;
    ASTExpr*     inner;
    SourceLoc    cast_loc;
    Type*        from;
    Type*        to;
    Type*        fromc; // Equivalent to from->canonical
    Type*        toc;   // Equivalent to to->canonical
    bool         explicit;
    bool         silent;
};

struct CastRule
{
    bool (*able)(const CastParams* const params);
    void (*conversion)(const CastParams* const params);
};

static CastGroup s_type_to_group[__TYPE_COUNT];
static CastRule s_rule_table[__CAST_GROUP_COUNT][__CAST_GROUP_COUNT];

static inline CastRule get_cast_rule(TypeKind from_kind, TypeKind to_kind);
static inline ASTExpr* new_cast(ASTExpr* inner);

static inline bool can_cast_params(const CastParams* const params)
{
    // The types being equal should be handled before calling this function
    DBG_ASSERT(!type_equal(params->fromc, params->toc));

    CastRule rule = get_cast_rule(params->fromc->kind, params->toc->kind);
    if(rule.able == NULL)
    {
        CAST_ERROR("Casting from %s to %s is not allowed.",
                   type_to_string(params->from), type_to_string(params->to));
    }

    return rule.able(params);
}

static inline void perform_cast_params(const CastParams* const params)
{
    CastRule rule = get_cast_rule(params->fromc->kind, params->toc->kind);
    rule.conversion(params);
}

bool can_cast(ASTExpr* expr, Type* to, bool silent)
{
    CastParams params;
    params.cast = NULL;
    params.inner = expr;
    params.cast_loc = expr->loc;
    params.from = expr->type;
    params.to = to;
    params.fromc = params.from->canonical;
    params.toc = params.to->canonical;
    params.explicit = false;
    params.silent = silent;
    return can_cast_params(&params);
}

void perform_cast(ASTExpr* expr, Type* to)
{
    // We only need to fill out the required parts of the params needed for conversion.
    CastParams params;
    params.cast = NULL;
    params.inner = expr;
    params.from = expr->type;
    params.to = to;
    params.fromc = params.from->canonical;
    params.toc = params.to->canonical;
    params.explicit = false;
    perform_cast_params(&params);
}

bool analyze_explicit_cast(ASTExpr* cast)
{
    ASTExpr* inner = cast->expr.cast.inner;
    if(!analyze_expr(inner) || !resolve_type(&cast->type, RES_ALLOW_VOID, cast->loc, "Cannot cast to type"))
        return false;

    CastParams params;
    params.cast      = cast;
    params.inner     = inner;
    params.cast_loc  = cast->loc;
    params.from      = inner->type;
    params.to        = cast->type;
    params.fromc     = type_reduce(params.from);
    params.toc       = type_reduce(params.to);
    params.explicit  = true;
    params.silent    = false;

    if(type_equal(params.fromc, params.toc))
    {
        // Essentially delete the cast expression if it does nothing.
        expr_copy(cast, inner);
        cast->type = params.to;
        return true;
    }

    if(!can_cast_params(&params)) return false;
    perform_cast_params(&params);
    cast->type = params.to;
    return true;
}

bool implicit_cast(ASTExpr* expr, Type* desired)
{
    DBG_ASSERT(desired->status == STATUS_RESOLVED);
    if(!analyze_expr(expr) || type_is_bad(desired))
        return false;

    CastParams params;
    params.cast      = NULL;
    params.inner     = expr;
    params.cast_loc  = expr->loc;
    params.from      = expr->type;
    params.to        = desired;
    params.fromc     = params.from->canonical;
    params.toc       = params.to->canonical;
    params.explicit  = false;
    params.silent    = false;

    if(type_equal(params.fromc, params.toc))
        return true;

    if(!can_cast_params(&params)) return false;
    perform_cast_params(&params);
    expr->type = desired;
    return true;
}

bool implicit_cast_vararg(ASTExpr* arg)
{
    if(!analyze_expr(arg)) return false;
    CastParams params;
    params.cast = NULL;
    params.inner = arg;
    params.cast_loc = arg->loc;
    params.from = arg->type;
    params.fromc = arg->type->canonical;
    params.explicit = false;
    params.silent = false;

    switch(arg->type->kind)
    {
    case TYPE_BOOL:
    case TYPE_CHAR:
    case TYPE_BYTE:
    case TYPE_UBYTE:
    case TYPE_SHORT:
    case TYPE_USHORT:
        params.to = params.toc = g_type_int;
        break;
    case TYPE_FLOAT:
        params.to = params.toc = g_type_double;
        break;
    case TYPE_INIT_LIST:
        sic_error_at(arg->loc, "Cannot pass initializer list as a variadic argument.");
        return false;
    case TYPE_STRING_LITERAL:
        params.to = params.toc = type_pointer_to(g_type_char);
        break;
    default:
        return true;
    }

    perform_cast_params(&params);
    return true;
}



static bool rule_not_defined(UNUSED const CastParams* const params) { SIC_UNREACHABLE(); }
static bool rule_always(UNUSED const CastParams* const params) { return true; }
static bool rule_explicit_only(const CastParams* const params)
{
    if(params->explicit) return true;
    CAST_ERROR("%s cannot be implicitly converted to %s.",
               type_to_string(params->from), type_to_string(params->to));
}

static bool rule_int_to_int(const CastParams* const params)
{
    if(params->explicit) return true;
    if(type_is_int_literal(params->fromc))
    {
        if(!i128_fits(params->inner->expr.constant.i, params->fromc, params->toc->kind))
        {
            CAST_ERROR("Int literal value cannot be represented by type %s.", type_to_string(params->to));
        }
        return true;
    }

    if(type_is_signed(params->fromc) != type_is_signed(params->toc))
    {
        CAST_ERROR("Casting between signed and unsigned integers requires explicit cast.");
    }

    if(type_size(params->fromc) < type_size(params->toc)) return true;

    CAST_ERROR("Narrowing from %s to %s requires explicit cast.",
               type_to_string(params->from), type_to_string(params->to));
}

static bool rule_size_change(const CastParams* const params)
{
    if(params->explicit || type_size(params->fromc) < type_size(params->toc))
        return true;

    CAST_ERROR("Narrowing from %s to %s requires explicit cast.",
               type_to_string(params->from), type_to_string(params->to));
}

static bool rule_ptr_to_ptr(const CastParams* const params)
{
    if(params->explicit) return true;

    Type* from_ptr = params->fromc->pointer_base;
    Type* to_ptr   = params->toc->pointer_base;

    // Going from *const T to *T is illegal.
    if((from_ptr->qualifiers & TYPE_QUAL_CONST) && 
       (to_ptr->qualifiers & TYPE_QUAL_CONST) == 0)
    {
        CAST_ERROR("Casting from %s to %s disregards 'const' qualifier.", 
                   type_to_string(params->from), type_to_string(params->to));
    }

    from_ptr = from_ptr->canonical;
    to_ptr = to_ptr->canonical;
    if((params->fromc->kind == TYPE_POINTER && from_ptr->kind == TYPE_VOID) || 
       (params->toc->kind == TYPE_POINTER && to_ptr->kind == TYPE_VOID))
       return true;


    bool from_is_arr = type_is_array(from_ptr);
    if(type_is_array(to_ptr))
    {
        if(from_is_arr)
            goto ERR;
        from_is_arr = true;
        Type* temp = from_ptr;
        from_ptr = to_ptr;
        to_ptr = temp;
    }

    if(from_is_arr && !type_equal(from_ptr->array.elem_type, to_ptr))
        goto ERR;

    return true;
ERR:
    CAST_ERROR("Unable to implicitly cast between pointer types %s and %s.",
               type_to_string(params->from), type_to_string(params->to));
}

static bool rule_str_to_ptr(const CastParams* const params)
{
    TypeKind kind = params->toc->pointer_base->canonical->kind;

    if(params->fromc->kind != TYPE_STRING_LITERAL)
    {
        CAST_ERROR("Casting from %s to %s is not allowed.",
                   type_to_string(params->from), type_to_string(params->to));
    }

    if(kind != TYPE_CHAR && kind != TYPE_UBYTE && kind != TYPE_BYTE)
    {
        CAST_ERROR("String literal can only be casted to char*, ubyte*, or byte*.");
    }
    return true;
}

static bool rule_init_list_to_arr(const CastParams* const params)
{
    if(params->toc->kind == TYPE_RUNTIME_ARRAY)
    {
        // NOTE: This can be improved later on if I add some runtime checks. Don't plan on doing that
        //       atm, but if I do, revisit this.
        CAST_ERROR("Cannot cast to runtime array because its length is not known at compile-time.");
    }
    uint64_t arr_size = params->toc->array.static_len; 
    Type* elem_type = params->toc->array.elem_type->canonical;
    if(params->fromc->kind == TYPE_STRING_LITERAL)
    {
        DBG_ASSERT(params->inner->kind == EXPR_CONSTANT && params->inner->expr.constant.kind == CONSTANT_STRING);
        const ConstString str = params->inner->expr.constant.str;
        if(elem_type->kind != str.kind)
        {
            printf("%d %d\n", elem_type->kind, str.kind);
            CAST_ERROR("String literal cannot be converted to type %s.", type_to_string(params->to));
        }
        if(arr_size < str.len)
        {
            CAST_ERROR("Length of array(%lu) is smaller than length of string literal(%u).",
                       arr_size, str.len);
        }
        return true;
    }

    if(params->inner->kind != EXPR_ARRAY_INIT_LIST)
    {
        CAST_ERROR("Casting from %s to %s is not allowed.",
                   type_to_string(params->from), type_to_string(params->to));
    }


    CastParams sub_params;
    sub_params.cast = NULL;
    sub_params.to = params->toc->array.elem_type;
    sub_params.toc = sub_params.to->canonical;
    sub_params.explicit = false;
    sub_params.silent = params->silent;
    ArrInitList* list = &params->inner->expr.array_init;
    for(uint32_t i = 0; i < list->size; ++i)
    {
        ArrInitEntry* const elem = list->data + i;
        if(elem->const_index >= arr_size)
        {
            if(!params->silent)
            {
                sic_error_at(elem->init_value->loc, "Index of array constant(%lu) exceeds bounds of array(%lu).",
                             elem->const_index, arr_size);
            }
            return false;
        }

        sub_params.inner = elem->init_value;
        sub_params.cast_loc = elem->init_value->loc;
        sub_params.from = elem->init_value->type;
        sub_params.fromc = sub_params.from->canonical;
        
        if(!can_cast_params(&sub_params)) return false;
        perform_cast_params(&sub_params);
    }

    return true;
}

static bool rule_init_list_to_struct(const CastParams* const params)
{
    if(params->inner->kind != EXPR_STRUCT_INIT_LIST)
    {
        CAST_ERROR("Casting from %s to %s is not allowed.",
                   type_to_string(params->from), type_to_string(params->to));
    }

    // ObjStruct* struct_ = params->to->canonical->struct_;
    StructInitList* list = &params->inner->expr.struct_init;
    for(uint32_t i = 0; i < list->size; ++i)
    {
        StructInitEntry* entry = list->data + i;
        for(uint32_t j = 0; j < i; ++j)
        {
            if(list->data[j].unresolved_member.sym == entry->unresolved_member.sym)
            {
                sic_error_at(list->data[j].unresolved_member.loc, 
                             "Duplicate assignment of member \'%s\'.", 
                             entry->unresolved_member.sym);
                sic_diagnostic_at(DIAG_NOTE, entry->unresolved_member.loc, "Previous assignment here.");
                return false;
            }
        }
        // if(entry-)
    }

    return true;
}

static bool rule_distinct(const CastParams* const params)
{
    DBG_ASSERT(!params->explicit);
    CAST_ERROR("Casting from %s to %s requires an explicit cast.",
               type_to_string(params->from), type_to_string(params->to));
}

// Casting functions

static void cast_any_to_void(UNUSED const CastParams* const params)
{
    SIC_TODO_MSG("Cast any to void");
}


static void cast_bool_to_int(const CastParams* const params)
{
    if(params->inner->kind == EXPR_CONSTANT)
    {
        convert_to_const_int(params->cast == NULL ? params->inner : params->cast, i128_from_u64(params->inner->expr.constant.b));
        return;
    }

    ASTExpr* const cast = params->cast == NULL ? new_cast(params->inner) : params->cast;
    cast->is_const_eval = params->inner->is_const_eval;
    cast->expr.cast.kind = CAST_UINT_WIDEN;
}


static void cast_int_to_bool(const CastParams* const params)
{
    if(params->inner->kind == EXPR_CONSTANT)
    {
        bool value = !i128_is_zero(params->inner->expr.constant.i);
        convert_to_const_bool(params->cast == NULL ? params->inner : params->cast, value);
        return;
    }

    ASTExpr* const cast = params->cast == NULL ? new_cast(params->inner) : params->cast;
    cast->is_const_eval = params->inner->is_const_eval;
    cast->expr.cast.kind = CAST_INT_TO_BOOL;
}

static void cast_int_to_int(const CastParams* const params)
{
    uint32_t from_bit_width = params->fromc->builtin.bit_size;
    uint32_t to_bit_width = params->toc->builtin.bit_size;
    if(params->inner->kind == EXPR_CONSTANT)
    {
        Int128 val = params->inner->expr.constant.i;
        ASTExpr* const cast = params->cast == NULL ? params->inner : params->cast;
        convert_to_constant(cast, CONSTANT_INTEGER);
        if(to_bit_width == 128)
        {
            cast->expr.constant.i = val;
            return;
        }

        uint32_t shift = 128 - to_bit_width; 
        bool to_signed = type_is_signed(params->toc);
        if(from_bit_width < to_bit_width) // Extending
        {
            if(to_signed || type_is_unsigned(params->fromc))
            {
                cast->expr.constant.i = val;
                return;
            }
            cast->expr.constant.i = i128_lshr64(i128_shl64(val, shift), shift);
            return;
        }

        if(to_signed)
            cast->expr.constant.i = i128_ashr64(i128_shl64(val, shift), shift);
        else
            cast->expr.constant.i = i128_lshr64(i128_shl64(val, shift), shift);
        return;
    }

    CastKind kind = from_bit_width > to_bit_width ? 
                        CAST_INT_TRUNCATE : 
                        (type_is_signed(params->fromc) ? CAST_SINT_WIDEN : CAST_UINT_WIDEN);

    if(from_bit_width == to_bit_width || (params->inner->kind == EXPR_CAST && params->inner->expr.cast.kind == kind))
    {
        if(params->cast != NULL)
           expr_copy(params->cast, params->inner); 
        return;
    }

    ASTExpr* const cast = params->cast == NULL ? new_cast(params->inner) : params->cast;

    cast->is_const_eval = params->inner->is_const_eval;
    cast->expr.cast.kind = kind;
}

static void cast_int_to_float(const CastParams* const params)
{
    if(params->inner->kind == EXPR_CONSTANT)
    {
        double value = i128_to_float(params->inner->expr.constant.i, params->fromc->kind);
        convert_to_const_float(params->cast == NULL ? params->inner : params->cast, value);
        return;
    }

    ASTExpr* const cast = params->cast == NULL ? new_cast(params->inner) : params->cast;
    cast->is_const_eval = params->inner->is_const_eval;
    cast->expr.cast.kind = type_is_signed(params->fromc) ? CAST_SINT_TO_FLOAT : CAST_UINT_TO_FLOAT;
}

static void cast_int_to_ptr(const CastParams* const params)
{
    if(params->inner->kind == EXPR_CONSTANT)
    {
        convert_to_const_pointer(params->cast == NULL ? params->inner : params->cast, params->inner->expr.constant.i);
        return;
    }

    ASTExpr* const cast = params->cast == NULL ? new_cast(params->inner) : params->cast;
    cast->is_const_eval = params->inner->is_const_eval;
    cast->expr.cast.kind = CAST_INT_TO_PTR;
}

static void cast_float_to_float(const CastParams* const params)
{
    if(params->inner->kind == EXPR_CONSTANT)
    {
        if(params->cast != NULL)
            convert_to_const_float(params->cast, params->inner->expr.constant.f);
        return;
    }

    ASTExpr* const cast = params->cast == NULL ? new_cast(params->inner) : params->cast;
    cast->is_const_eval = params->inner->is_const_eval;
    cast->expr.cast.kind = type_size(params->fromc) < type_size(params->toc) ? CAST_FLOAT_WIDEN : CAST_FLOAT_TRUNCATE;
}

static void cast_float_to_bool(const CastParams* const params)
{
    if(params->inner->kind == EXPR_CONSTANT)
    {
        bool value = params->inner->expr.constant.f != 0.0;
        convert_to_const_bool(params->cast == NULL ? params->inner : params->cast, value);
        return;
    }

    ASTExpr* const cast = params->cast == NULL ? new_cast(params->inner) : params->cast;
    cast->is_const_eval = params->inner->is_const_eval;
    cast->expr.cast.kind = CAST_FLOAT_TO_BOOL;
}

static void cast_float_to_int(const CastParams* const params)
{
    if(params->inner->kind == EXPR_CONSTANT)
    {
        Int128 value = i128_from_double(params->inner->expr.constant.f, params->toc->kind);
        convert_to_const_int(params->cast == NULL ? params->inner : params->cast, value);
        return;
    }

    ASTExpr* const cast = params->cast == NULL ? new_cast(params->inner) : params->cast;
    cast->is_const_eval = params->inner->is_const_eval;
    cast->expr.cast.kind = type_is_signed(params->toc) ? CAST_FLOAT_TO_SINT : CAST_FLOAT_TO_UINT;
}

static void cast_ptr_to_bool(const CastParams* const params)
{
    if(params->inner->kind == EXPR_CONSTANT)
    {
        bool value = !i128_is_zero(params->inner->expr.constant.i);
        convert_to_const_bool(params->cast == NULL ? params->inner : params->cast, value);
        return;
    }

    ASTExpr* const cast = params->cast == NULL ? new_cast(params->inner) : params->cast;
    cast->is_const_eval = params->inner->is_const_eval;
    cast->expr.cast.kind = CAST_PTR_TO_BOOL;
}

static void cast_ptr_to_int(const CastParams* const params)
{
    if(params->inner->kind == EXPR_CONSTANT)
    {
        convert_to_const_int(params->cast == NULL ? params->inner : params->cast, params->inner->expr.constant.i);
        return;
    }

    ASTExpr* const cast = params->cast == NULL ? new_cast(params->inner) : params->cast;
    cast->is_const_eval = params->inner->is_const_eval;
    cast->expr.cast.kind = CAST_PTR_TO_INT;
}

static void cast_ptr_to_ptr(const CastParams* const params)
{
    if(params->inner->kind == EXPR_CONSTANT)
    {
        if(params->cast != NULL)
            convert_to_const_pointer(params->cast, params->inner->expr.constant.i);
        return;
    }

    if(params->cast == NULL) return;
    expr_copy(params->cast, params->inner);
}

static void cast_init_list(const CastParams* const params)
{
    if(params->cast == NULL) return;
    expr_copy(params->cast, params->inner);
}

#define NOALLW { NULL                    , NULL }
#define NOTDEF { rule_not_defined        , NULL }
#define TOVOID { rule_explicit_only      , cast_any_to_void }
#define BOOINT { rule_explicit_only      , cast_bool_to_int }
#define CHABOO { rule_explicit_only      , cast_int_to_bool }
#define CHAINT { rule_explicit_only      , cast_int_to_int }
#define CHAFLT { rule_explicit_only      , cast_int_to_float }
#define INTBOO { rule_explicit_only      , cast_int_to_bool }
#define INTINT { rule_int_to_int         , cast_int_to_int }
#define INTFLT { rule_always             , cast_int_to_float }
#define INTPTR { rule_explicit_only      , cast_int_to_ptr }
#define FLTFLT { rule_size_change        , cast_float_to_float }
#define FLTBOO { rule_explicit_only      , cast_float_to_bool }
#define FLTINT { rule_explicit_only      , cast_float_to_int }
#define PTRBOO { rule_explicit_only      , cast_ptr_to_bool }
#define PTRINT { rule_explicit_only      , cast_ptr_to_int }
#define PTRPTR { rule_ptr_to_ptr         , cast_ptr_to_ptr }
#define STRPTR { rule_str_to_ptr         , cast_init_list }
#define ILSARR { rule_init_list_to_arr   , cast_init_list }
#define ILSSTU { rule_init_list_to_struct, cast_init_list }
#define DISTIN { rule_distinct           , NULL }

static CastRule s_rule_table[__CAST_GROUP_COUNT][__CAST_GROUP_COUNT] = {
    // FROM              TO:   VOID    BOOL    CHAR    INT     FLOAT   PTR     ARRAY   STRUCT  INITLS  DIST
    [CAST_GROUP_VOID]      = { NOTDEF, NOALLW, NOTDEF, NOALLW, NOALLW, NOALLW, NOALLW, NOALLW, NOTDEF, NOALLW },
    [CAST_GROUP_BOOL]      = { TOVOID, NOTDEF, NOALLW, BOOINT, NOALLW, NOALLW, NOALLW, NOALLW, NOTDEF, DISTIN },
    [CAST_GROUP_CHAR]      = { TOVOID, CHABOO, INTINT, CHAINT, CHAFLT, NOALLW, NOALLW, NOALLW, NOTDEF, DISTIN },
    [CAST_GROUP_INT]       = { TOVOID, INTBOO, CHAINT, INTINT, INTFLT, INTPTR, NOALLW, NOALLW, NOTDEF, DISTIN },
    [CAST_GROUP_FLOAT]     = { TOVOID, FLTBOO, FLTINT, FLTINT, FLTFLT, NOALLW, NOALLW, NOALLW, NOTDEF, DISTIN },
    [CAST_GROUP_PTR]       = { TOVOID, PTRBOO, NOALLW, PTRINT, NOALLW, PTRPTR, NOALLW, NOALLW, NOTDEF, DISTIN },
    [CAST_GROUP_ARRAY]     = { TOVOID, NOALLW, NOALLW, NOALLW, NOALLW, NOALLW, NOALLW, NOALLW, NOTDEF, DISTIN },
    [CAST_GROUP_STRUCT]    = { TOVOID, NOALLW, NOALLW, NOALLW, NOALLW, NOALLW, NOALLW, NOALLW, NOTDEF, DISTIN },
    [CAST_GROUP_INIT_LIST] = { TOVOID, NOALLW, NOALLW, NOALLW, NOALLW, STRPTR, ILSARR, ILSSTU, NOTDEF, DISTIN },
    [CAST_GROUP_DISTINCT]  = { TOVOID, DISTIN, DISTIN, DISTIN, DISTIN, DISTIN, DISTIN, DISTIN, DISTIN, DISTIN },
};

// We add +1 so that by default all the unspecified types get 0 which is the INVALID group.
static CastGroup s_type_to_group[__TYPE_COUNT] = {
    [TYPE_VOID]            = CAST_GROUP_VOID + 1,
    [TYPE_BOOL]            = CAST_GROUP_BOOL + 1,
    [TYPE_CHAR]            = CAST_GROUP_CHAR + 1,
    [TYPE_CHAR16]          = CAST_GROUP_CHAR + 1,
    [TYPE_CHAR32]          = CAST_GROUP_CHAR + 1,
    [TYPE_BYTE]            = CAST_GROUP_INT + 1,
    [TYPE_UBYTE]           = CAST_GROUP_INT + 1,
    [TYPE_SHORT]           = CAST_GROUP_INT + 1,
    [TYPE_USHORT]          = CAST_GROUP_INT + 1,
    [TYPE_INT]             = CAST_GROUP_INT + 1,
    [TYPE_UINT]            = CAST_GROUP_INT + 1,
    [TYPE_LONG]            = CAST_GROUP_INT + 1,
    [TYPE_ULONG]           = CAST_GROUP_INT + 1,
    [TYPE_INT128]          = CAST_GROUP_INT + 1,
    [TYPE_UINT128]         = CAST_GROUP_INT + 1,
    [TYPE_FLOAT]           = CAST_GROUP_FLOAT + 1,
    [TYPE_DOUBLE]          = CAST_GROUP_FLOAT + 1,
    [TYPE_POINTER]         = CAST_GROUP_PTR + 1,
    [TYPE_FUNC_PTR]        = CAST_GROUP_PTR + 1,
    [TYPE_STATIC_ARRAY]    = CAST_GROUP_ARRAY + 1,
    [TYPE_RUNTIME_ARRAY]   = CAST_GROUP_ARRAY + 1,
    [TYPE_ALIAS_DISTINCT]  = CAST_GROUP_DISTINCT + 1,
    [TYPE_ENUM_DISTINCT]   = CAST_GROUP_DISTINCT + 1,
    [TYPE_STRUCT]          = CAST_GROUP_STRUCT + 1,
    [TYPE_UNION]           = CAST_GROUP_STRUCT + 1,
    [TYPE_INIT_LIST]       = CAST_GROUP_INIT_LIST + 1,
    [TYPE_STRING_LITERAL]  = CAST_GROUP_INIT_LIST + 1,
};

static inline CastRule get_cast_rule(TypeKind from_kind, TypeKind to_kind)
{
    CastGroup from = s_type_to_group[from_kind] - 1;
    CastGroup to = s_type_to_group[to_kind] - 1;
    DBG_ASSERT(from != CAST_GROUP_INVALID && to != CAST_GROUP_INVALID);
    return s_rule_table[from][to];
}

static inline ASTExpr* new_cast(ASTExpr* inner)
{
    ASTExpr* new_inner = MALLOC_STRUCT(ASTExpr);
    expr_copy(new_inner, inner);
    inner->kind = EXPR_CAST;
    inner->expr.cast.inner = new_inner;
    return inner;
}
