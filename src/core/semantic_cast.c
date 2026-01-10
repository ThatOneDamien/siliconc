#include "semantics.h"

#define CAST_ERROR(...) do { if(!params->silent) sic_error_at(params->cast_loc, __VA_ARGS__); } while(0)

typedef struct CastParams CastParams;
typedef struct CastRule   CastRule;

struct CastParams
{
    ASTExpr*     inner;
    SourceLoc    cast_loc;
    Type*        from;
    Type*        to;
    TypeKind     from_kind;
    TypeKind     to_kind;
    bool         explicit;
    bool         silent;
};

struct CastRule
{
    bool (*able)(CastParams* params);
    void (*conversion)(ASTExpr* cast, ASTExpr* inner, Type* from, Type* to);
};

static CastGroup s_type_to_group[__TYPE_COUNT];
static CastRule s_rule_table[__CAST_GROUP_COUNT][__CAST_GROUP_COUNT];

static inline CastRule get_cast_rule(TypeKind from_kind, TypeKind to_kind);

bool analyze_cast(ASTExpr* cast)
{
    ASTExpr* inner = cast->expr.cast.inner;
    if(!resolve_type(&cast->type, RES_ALLOW_VOID, cast->loc, "Cannot cast to type") || 
       !analyze_expr(inner))
        return false;
    CastParams params;
    params.inner     = inner;
    params.cast_loc  = cast->loc;
    params.from      = inner->type;
    params.to        = cast->type;
    params.from_kind = params.from->canonical->kind;
    params.to_kind   = params.to->canonical->kind;
    params.explicit  = true;
    params.silent    = false;

    if(type_equal(params.from, params.to))
    {
        // Essentially delete the cast expression if it does nothing.
        *cast = *inner;
        return true;
    }

    CastRule rule = get_cast_rule(params.from_kind, params.to_kind);
    if(rule.able == NULL)
    {
        sic_error_at(params.cast_loc, "Casting from %s to %s is not allowed.",
                     type_to_string(params.from), type_to_string(params.to));
        return false;
    }

    if(!rule.able(&params))
    {
        cast->kind = EXPR_INVALID;
        return false;
    }

    rule.conversion(cast, inner, params.from, params.to);
    return true;
}

bool implicit_cast(ASTExpr** expr_to_cast, Type* desired)
{
    SIC_ASSERT(desired->status == STATUS_RESOLVED);
    ASTExpr* prev = *expr_to_cast;
    if(!analyze_expr(prev) || type_is_bad(desired))
        return false;
    CastParams params;
    params.inner     = prev;
    params.cast_loc  = prev->loc;
    params.from      = prev->type;
    params.to        = desired;
    params.from_kind = params.from->canonical->kind;
    params.to_kind   = params.to->canonical->kind;
    params.explicit  = false;
    params.silent    = false;
    if(type_equal(params.from, params.to))
        return true;

    CastRule rule = get_cast_rule(params.from_kind, params.to_kind);
    if(rule.able == NULL)
    {
        sic_error_at(params.cast_loc, "Casting from %s to %s is not allowed.",
                     type_to_string(params.from), type_to_string(params.to));
        return false;
    }


    if(!rule.able(&params))
    {
        *expr_to_cast = g_bad_expr;
        return false;
    }

    ASTExpr* new_cast = CALLOC_STRUCT(ASTExpr);
    new_cast->kind = EXPR_CAST;
    new_cast->loc = prev->loc;
    new_cast->expr.cast.inner = prev;
    new_cast->evaluated = true;
    new_cast->type = desired;

    rule.conversion(new_cast, prev, params.from, params.to);

    *expr_to_cast = new_cast;
    return true;
}

static bool can_cast_params(CastParams* params)
{
    if(type_equal(params->from, params->to))
        return true;

    CastRule rule = get_cast_rule(params->from_kind, params->to_kind);
    if(rule.able == NULL)
    {
        CAST_ERROR("Casting from %s to %s is not allowed.",
                   type_to_string(params->from), type_to_string(params->to));
        return false;
    }

    return rule.able(params);
}

bool can_cast(ASTExpr* expr, Type* to)
{
    SIC_ASSERT(to->status == STATUS_RESOLVED && expr->evaluated);

    CastParams params;
    params.inner     = expr;
    params.cast_loc  = expr->loc;
    params.from      = expr->type;
    params.to        = to;
    params.from_kind = params.from->canonical->kind;
    params.to_kind   = params.to->canonical->kind;
    params.explicit  = false;
    params.silent    = false;
    return can_cast_params(&params);
}


void perform_cast(ASTExpr* cast)
{
    ASTExpr* inner = cast->expr.cast.inner;
    CastRule rule = get_cast_rule(inner->type->canonical->kind, cast->type->canonical->kind);
    rule.conversion(cast, inner, inner->type, cast->type);
}

bool implicit_cast_vararg(ASTExpr** arg)
{
    ASTExpr* expr = *arg;
    if(!analyze_expr(expr)) return false;
    switch(expr->type->kind)
    {
    case TYPE_BOOL:
    case TYPE_CHAR:
    case TYPE_BYTE:
    case TYPE_UBYTE:
    case TYPE_SHORT:
    case TYPE_USHORT:
        implicit_cast_ensured(arg, g_type_int);
        return true;
    case TYPE_FLOAT:
        implicit_cast_ensured(arg, g_type_double);
        return true;
    case TYPE_ANON_ARRAY:
        sic_error_at(expr->loc, "Cannot pass untyped array literal as a variadic argument.");
        return false;
    case TYPE_STRING_LIT:
        implicit_cast_ensured(arg, type_pointer_to(g_type_char));
        return true;
    default:
        return true;
    }
}


static bool rule_not_defined(UNUSED CastParams* params) { SIC_UNREACHABLE(); }
static bool rule_always(UNUSED CastParams* params) { return true; }
static bool rule_explicit_only(CastParams* params)
{
    if(params->explicit)
        return true;
    CAST_ERROR("%s cannot be implicitly converted to %s.",
               type_to_string(params->from), type_to_string(params->to));
    return false;
}

static bool rule_size_change(CastParams* params)
{
    if(params->explicit)
        return true;

    if(params->to_kind == TYPE_CHAR && params->from_kind != TYPE_CHAR)
    {
        CAST_ERROR("Casting from %s to char requires explicit cast."
                   "Use byte or ubyte for single byte integer.",
                   type_to_string(params->from));
        return false;
    }

    if(type_size(params->from) <= type_size(params->to))
        return true;

    // TODO: Add bounds checking for warning/error if constant goes out of bounds.
    if(params->inner->kind == EXPR_CONSTANT)
        return true;

    CAST_ERROR("Narrowing integer type %s to %s requires explicit cast.",
               type_to_string(params->from), type_to_string(params->to));
    return false;
}

static bool rule_ptr_to_ptr(CastParams* params)
{
    if(params->explicit)
        return true;
    Type* from_ptr = params->from->pointer_base;
    Type* to_ptr   = params->to->pointer_base;
    if((params->from_kind == TYPE_POINTER && from_ptr->kind == TYPE_VOID) || 
       (params->to_kind == TYPE_POINTER && to_ptr->kind == TYPE_VOID))
       return true;

    // if(params->)

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

    if(from_is_arr && type_equal(from_ptr->array.elem_type, to_ptr))
        return true;

ERR:
    CAST_ERROR("Unable to implicitly cast between pointer types %s and %s.",
               type_to_string(params->from), type_to_string(params->to));
    return false;
}

static bool rule_str_to_ptr(CastParams* params)
{
    TypeKind kind = params->to->pointer_base->kind;

    if(params->from_kind != TYPE_STRING_LIT)
    {
        CAST_ERROR("Casting from %s to %s is not allowed.",
                   type_to_string(params->from), type_to_string(params->to));
        return false;
    }

    if(kind != TYPE_CHAR && kind != TYPE_UBYTE && kind != TYPE_BYTE)
    {
        CAST_ERROR("String literal can only be casted to char*, ubyte*, or byte*.");
        return false;
    }
    return true;
}

static bool rule_anon_to_arr(CastParams* params)
{
    if(params->to_kind == TYPE_RUNTIME_ARRAY)
    {
        // NOTE: This can be improved later on if I add some runtime checks. Don't plan on doing that
        //       atm, but if I do, revisit this.
        CAST_ERROR("Cannot cast to runtime array because its length is not known at compile-time.");
        return false;
    }
    uint64_t arr_size = params->to->array.static_len; 
    Type* elem_type = params->to->array.elem_type;
    if(params->from_kind == TYPE_STRING_LIT)
    {
        SIC_ASSERT(params->inner->kind == EXPR_CONSTANT && params->inner->expr.constant.kind == CONSTANT_STRING);
        if(elem_type->kind != TYPE_CHAR && elem_type->kind != TYPE_UBYTE && elem_type->kind != TYPE_BYTE)
        {
            CAST_ERROR("String literal can only be casted to char[], ubyte[], or byte[].");
            return false;
        }
        uint64_t str_len = params->inner->expr.constant.val.str_len;
        if(arr_size >= str_len)
        {
            CAST_ERROR("Length of array(%lu) is smaller than length of string literal(%lu).",
                       arr_size, str_len);
            return false;
        }
        return true;
    }

    SIC_ASSERT(params->inner->kind == EXPR_ARRAY_INIT_LIST);
    InitList* list = &params->inner->expr.init_list;
    for(uint32_t i = 0; i < list->size; ++i)
    {
        if(list->data[i].const_index >= arr_size)
        {
            if(!params->silent)
            {
                sic_error_at(list->data[i].init_value->loc, "Index of array constant(%lu) exceeds bounds of array(%lu).",
                             list->data[i].const_index, arr_size);
            }
            return false;
        }

        if(!can_cast(list->data[i].init_value, elem_type))
            return false;
    }

    return true;
}

static bool rule_distinct(CastParams* params)
{
    if(!params->explicit)
    {
        CAST_ERROR("Casting from %s to %s requires an explicit cast.",
                   type_to_string(params->from), type_to_string(params->to));
        return false;
    }

    CastParams new_params;
    new_params.inner     = params->inner;
    new_params.from      = params->from;
    new_params.to        = params->to;
    new_params.from_kind = type_reduce(new_params.from)->kind;
    new_params.to_kind   = type_reduce(new_params.to)->kind;
    new_params.explicit  = true;
    new_params.silent    = params->silent;
    return can_cast_params(&new_params);
}

// Casting functions

static void cast_any_to_void(UNUSED ASTExpr* cast, UNUSED ASTExpr* inner,
                             UNUSED Type* from, UNUSED Type* to)
{
    SIC_TODO_MSG("Cast any to void");
}

static void cast_int_to_int(ASTExpr* cast, ASTExpr* inner, Type* from, Type* to)
{
    if(inner->kind == EXPR_CONSTANT)
    {
        cast->kind = EXPR_CONSTANT;
        cast->expr.constant.kind = CONSTANT_INTEGER;
        cast->expr.constant.val.i = inner->expr.constant.val.i;
        cast->const_eval = true;
        uint32_t from_bit_width = from->canonical->builtin.bit_size;
        uint32_t to_bit_width = to->canonical->builtin.bit_size;
        if(to_bit_width == 64)
            return;

        uint64_t val = cast->expr.constant.val.i;
        uint32_t shift = 64 - to_bit_width; 
        bool to_signed = type_is_signed(to);
        if(from_bit_width < to_bit_width) // Extending
        {
            if(to_signed || type_is_unsigned(from))
                return;
            cast->expr.constant.val.i = (val << shift) >> shift;
            return;
        }

        if(to_signed)
            cast->expr.constant.val.i = (int64_t)(val << shift) >> shift;
        else
            cast->expr.constant.val.i = (val << shift) >> shift;
        return;
    }

    if(type_size(from) == type_size(to))
    {
        cast->expr.cast.kind = CAST_REINTERPRET;
        return;
    }
    cast->expr.cast.kind = type_is_signed(from) ? 
                            CAST_SINT_EXT_TRUNC : 
                            CAST_UINT_EXT_TRUNC;
}

static void cast_int_to_bool(ASTExpr* cast, ASTExpr* inner, 
                             UNUSED Type* from, UNUSED Type* to)
{
    if(inner->kind == EXPR_CONSTANT)
    {
        cast->kind = EXPR_CONSTANT;
        cast->expr.constant.val.i = (inner->expr.constant.val.i != 0);
        cast->expr.constant.kind = CONSTANT_INTEGER;
        cast->const_eval = true;
        return;
    }

    cast->expr.cast.kind = CAST_INT_TO_BOOL;
}

static void cast_int_to_float(ASTExpr* cast, ASTExpr* inner, Type* from, UNUSED Type* to)
{
    if(inner->kind == EXPR_CONSTANT)
    {
        cast->kind = EXPR_CONSTANT;
        cast->expr.constant.val.f = (double)inner->expr.constant.val.i;
        cast->expr.constant.kind = CONSTANT_FLOAT;
        cast->const_eval = true;
        return;
    }

    cast->expr.cast.kind = type_is_signed(from) ? 
                            CAST_SINT_TO_FLOAT : 
                            CAST_UINT_TO_FLOAT;
}

static void cast_int_to_ptr(ASTExpr* cast, ASTExpr* inner, 
                            UNUSED Type* from, UNUSED Type* to)
{
    if(inner->kind == EXPR_CONSTANT)
    {
        cast->kind = EXPR_CONSTANT;
        cast->expr.constant.val.i = inner->expr.constant.val.i;
        cast->expr.constant.kind = CONSTANT_INTEGER;
        cast->const_eval = true;
        return;
    }

    cast->expr.cast.kind = CAST_INT_TO_PTR;
}

static void cast_float_to_float(ASTExpr* cast, ASTExpr* inner,
                                UNUSED Type* from, UNUSED Type* to)
{
    if(inner->kind == EXPR_CONSTANT)
    {
        cast->kind = EXPR_CONSTANT;
        cast->expr.constant.val.f = inner->expr.constant.val.f;
        cast->expr.constant.kind = CONSTANT_FLOAT;
        cast->const_eval = true;
        return;
    }

    cast->expr.cast.kind = CAST_FLOAT_EXT_TRUNC;
}

static void cast_float_to_bool(ASTExpr* cast, ASTExpr* inner,
                               UNUSED Type* from, UNUSED Type* to)
{
    if(inner->kind == EXPR_CONSTANT)
    {
        cast->kind = EXPR_CONSTANT;
        cast->expr.constant.val.i = (inner->expr.constant.val.f != 0.0);
        cast->expr.constant.kind = CONSTANT_INTEGER;
        cast->const_eval = true;
        return;
    }

    SIC_TODO();
}

static void cast_float_to_int(ASTExpr* cast, ASTExpr* inner, UNUSED Type* from, Type* to)
{
    if(inner->kind == EXPR_CONSTANT)
    {
        cast->kind = EXPR_CONSTANT;
        cast->expr.constant.val.i = (uint64_t)inner->expr.constant.val.f;
        cast->expr.constant.kind = CONSTANT_INTEGER;
        cast->const_eval = true;
        return;
    }

    cast->expr.cast.kind = type_is_signed(to) ? 
                            CAST_FLOAT_TO_SINT : 
                            CAST_FLOAT_TO_UINT;
}

static void cast_ptr_to_bool(ASTExpr* cast, ASTExpr* inner,
                             UNUSED Type* from, UNUSED Type* to)
{
    cast->const_eval = inner->const_eval;
    if(inner->kind == EXPR_CONSTANT)
    {
        cast->kind = EXPR_CONSTANT;
        cast->expr.constant.val.i = (inner->expr.constant.val.i != 0);
        cast->expr.constant.kind = CONSTANT_INTEGER;
        return;
    }

    cast->expr.cast.kind = CAST_PTR_TO_BOOL;
}

static void cast_ptr_to_int(ASTExpr* cast, ASTExpr* inner,
                            UNUSED Type* from, UNUSED Type* to)
{
    cast->const_eval = inner->const_eval;
    if(inner->kind == EXPR_CONSTANT)
    {
        cast->kind = EXPR_CONSTANT;
        cast->expr.constant.val.i = inner->expr.constant.val.i;
        cast->expr.constant.kind = CONSTANT_INTEGER;
        return;
    }

    cast->expr.cast.kind = CAST_PTR_TO_INT;
}

static void cast_ptr_to_ptr(ASTExpr* cast, ASTExpr* inner,
                            UNUSED Type* from, UNUSED Type* to)
{
    cast->const_eval = inner->const_eval;
    if(inner->kind == EXPR_CONSTANT)
    {
        cast->kind = EXPR_CONSTANT;
        cast->expr.constant.val.i = inner->expr.constant.val.i;
        cast->expr.constant.kind = CONSTANT_POINTER;
        return;
    }
    cast->expr.cast.kind = CAST_REINTERPRET;
}

static void cast_reinterpret(ASTExpr* cast, UNUSED ASTExpr* inner,
                             UNUSED Type* from, UNUSED Type* to)
{
    cast->expr.cast.kind = CAST_REINTERPRET;
}

static void cast_anon_arr(ASTExpr* cast, ASTExpr* inner,
                          UNUSED Type* from, UNUSED Type* to)
{
    // This is different than the parameter 'to'. This is the unaltered
    // type we are casting to, without any reduction.
    Type* to_type = cast->type;
    *cast = *inner;
    cast->type = to_type;
}

static void cast_distinct(ASTExpr* cast, ASTExpr* inner, Type* from, Type* to)
{
    Type* from_reduced = type_reduce(from);
    Type* to_reduced = type_reduce(to);
    CastRule rule = get_cast_rule(from_reduced->kind, to_reduced->kind);
    rule.conversion(cast, inner, from_reduced, to_reduced);
}

#define NOALLW { NULL              , NULL }
#define NOTDEF { rule_not_defined  , NULL }
#define TOVOID { rule_explicit_only, cast_any_to_void }
#define INTINT { rule_size_change  , cast_int_to_int }
#define INTBOO { rule_always       , cast_int_to_bool }
#define INTFLT { rule_always       , cast_int_to_float }
#define INTPTR { rule_explicit_only, cast_int_to_ptr }
#define FLTFLT { rule_size_change  , cast_float_to_float }
#define FLTBOO { rule_explicit_only, cast_float_to_bool }
#define FLTINT { rule_explicit_only, cast_float_to_int }
#define PTRBOO { rule_explicit_only, cast_ptr_to_bool }
#define PTRINT { rule_explicit_only, cast_ptr_to_int }
#define PTRPTR { rule_ptr_to_ptr   , cast_ptr_to_ptr }
#define STRPTR { rule_str_to_ptr   , cast_reinterpret }
#define ANNARR { rule_anon_to_arr  , cast_anon_arr }
#define DISTIN { rule_distinct     , cast_distinct }

static CastRule s_rule_table[__CAST_GROUP_COUNT][__CAST_GROUP_COUNT] = {
    // FROM             TO:   VOID    BOOL    INT     FLOAT   PTR     ARRAY   STRUCT  ANONAR  DIST
    [CAST_GROUP_VOID]     = { NOTDEF, NOALLW, NOALLW, NOALLW, NOALLW, NOALLW, NOALLW, NOTDEF, NOALLW },
    [CAST_GROUP_BOOL]     = { TOVOID, NOTDEF, INTINT, NOALLW, NOALLW, NOALLW, NOALLW, NOTDEF, DISTIN },
    [CAST_GROUP_INT]      = { TOVOID, INTBOO, INTINT, INTFLT, INTPTR, NOALLW, NOALLW, NOTDEF, DISTIN },
    [CAST_GROUP_FLOAT]    = { TOVOID, FLTBOO, FLTINT, FLTFLT, NOALLW, NOALLW, NOALLW, NOTDEF, DISTIN },
    [CAST_GROUP_PTR]      = { TOVOID, PTRBOO, PTRINT, NOALLW, PTRPTR, NOALLW, NOALLW, NOTDEF, DISTIN },
    [CAST_GROUP_ARRAY]    = { TOVOID, NOALLW, NOALLW, NOALLW, NOALLW, NOALLW, NOALLW, NOTDEF, DISTIN },
    [CAST_GROUP_STRUCT]   = { TOVOID, NOALLW, NOALLW, NOALLW, NOALLW, NOALLW, NOALLW, NOTDEF, DISTIN },
    [CAST_GROUP_ANON_ARR] = { TOVOID, NOALLW, NOALLW, NOALLW, STRPTR, ANNARR, NOALLW, NOTDEF, DISTIN },
    [CAST_GROUP_DISTINCT] = { TOVOID, DISTIN, DISTIN, DISTIN, DISTIN, DISTIN, DISTIN, DISTIN, DISTIN },
};

// We add +1 so that by default all the unspecified types get 0 which is the INVALID group.
static CastGroup s_type_to_group[__TYPE_COUNT] = {
    [TYPE_VOID]           = CAST_GROUP_VOID + 1,
    [TYPE_BOOL]           = CAST_GROUP_BOOL + 1,
    [TYPE_CHAR]           = CAST_GROUP_INT + 1,
    [TYPE_CHAR16]         = CAST_GROUP_INT + 1,
    [TYPE_CHAR32]         = CAST_GROUP_INT + 1,
    [TYPE_BYTE]           = CAST_GROUP_INT + 1,
    [TYPE_UBYTE]          = CAST_GROUP_INT + 1,
    [TYPE_SHORT]          = CAST_GROUP_INT + 1,
    [TYPE_USHORT]         = CAST_GROUP_INT + 1,
    [TYPE_INT]            = CAST_GROUP_INT + 1,
    [TYPE_UINT]           = CAST_GROUP_INT + 1,
    [TYPE_LONG]           = CAST_GROUP_INT + 1,
    [TYPE_ULONG]          = CAST_GROUP_INT + 1,
    [TYPE_FLOAT]          = CAST_GROUP_FLOAT + 1,
    [TYPE_DOUBLE]         = CAST_GROUP_FLOAT + 1,
    [TYPE_POINTER]        = CAST_GROUP_PTR + 1,
    [TYPE_FUNC_PTR]       = CAST_GROUP_PTR + 1,
    [TYPE_STATIC_ARRAY]   = CAST_GROUP_ARRAY + 1,
    [TYPE_RUNTIME_ARRAY]  = CAST_GROUP_ARRAY + 1,
    [TYPE_ALIAS_DISTINCT] = CAST_GROUP_DISTINCT + 1,
    [TYPE_ENUM_DISTINCT]  = CAST_GROUP_DISTINCT + 1,
    [TYPE_STRUCT]         = CAST_GROUP_STRUCT + 1,
    [TYPE_UNION]          = CAST_GROUP_STRUCT + 1,
    [TYPE_ANON_ARRAY]     = CAST_GROUP_ANON_ARR + 1,
    [TYPE_STRING_LIT]     = CAST_GROUP_ANON_ARR + 1,
};

static inline CastRule get_cast_rule(TypeKind from_kind, TypeKind to_kind)
{
    CastGroup from = s_type_to_group[from_kind] - 1;
    CastGroup to = s_type_to_group[to_kind] - 1;
    SIC_ASSERT(from != CAST_GROUP_INVALID && to != CAST_GROUP_INVALID);
    return s_rule_table[from][to];
}
