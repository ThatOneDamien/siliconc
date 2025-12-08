#include "semantics.h"

typedef struct CastParams CastParams;
typedef struct CastRule   CastRule;

struct CastParams
{
    ASTExpr*     inner;
    Type*        from;
    Type*        to;
    TypeKind     from_kind;
    TypeKind     to_kind;
    bool         explicit;
    bool         silent;
};

struct CastRule
{
    bool (*able)(CastParams*);
    void (*conversion)(ASTExpr*, ASTExpr*);
};

static CastGroup s_type_to_group[__TYPE_COUNT];
static CastRule s_rule_table[__CAST_GROUP_COUNT][__CAST_GROUP_COUNT];

static inline CastRule get_cast_rule(CastParams* params);

PRINTF_FMT(2, 3)
static inline void cast_error(CastParams* params, const char* message, ...);

bool analyze_cast(ASTExpr* cast)
{
    if(!resolve_type(&cast->type, RES_ALLOW_VOID, cast->loc, "Cannot cast to type") || 
       !analyze_expr(&cast->expr.cast.inner))
        return false;
    ASTExpr* inner = cast->expr.cast.inner;
    CastParams params;
    params.inner     = inner;
    params.from      = inner->type;
    params.to        = cast->type;
    params.from_kind = params.from->canonical->kind;
    params.to_kind   = params.to->canonical->kind;
    params.explicit  = true;
    params.silent    = false;

    if(type_equal(params.from, params.to))
    {
        // Essentially delete the cast expression if it does nothing.
        memcpy(cast, inner, sizeof(ASTExpr));
        return true;
    }

    CastRule rule = get_cast_rule(&params);
    if(rule.able == NULL)
    {
        sic_error_at(cast->loc, "Casting from %s to %s is not allowed.",
                     type_to_string(params.from), type_to_string(params.to));
        return false;
    }

    if(!rule.able(&params))
    {
        cast->kind = EXPR_INVALID;
        return false;
    }

    rule.conversion(cast, inner);

    return true;
}

bool implicit_cast(ASTExpr** expr_to_cast, Type* desired)
{
    SIC_ASSERT(desired->status == STATUS_RESOLVED);
    if(!analyze_expr(expr_to_cast) || type_is_bad(desired))
        return false;
    ASTExpr* prev = *expr_to_cast;
    CastParams params;
    params.inner     = prev;
    params.from      = prev->type;
    params.to        = desired;
    params.from_kind = params.from->canonical->kind;
    params.to_kind   = params.to->canonical->kind;
    params.explicit  = false;
    params.silent    = false;
    if(type_equal(params.from, params.to))
        return true;

    CastRule rule = get_cast_rule(&params);
    if(rule.able == NULL)
    {
        sic_error_at(prev->loc, "Casting from %s to %s is not allowed.",
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

    rule.conversion(new_cast, prev);

    *expr_to_cast = new_cast;
    return true;
}

bool can_cast(ASTExpr* expr, Type* to)
{
    SIC_ASSERT(to->status == STATUS_RESOLVED && expr->evaluated);

    CastParams params;
    params.inner     = expr;
    params.from      = expr->type;
    params.to        = to;
    params.from_kind = params.from->canonical->kind;
    params.to_kind   = params.to->canonical->kind;
    params.explicit  = false;
    params.silent    = false;
    if(type_equal(params.from, params.to))
        return true;

    CastRule rule = get_cast_rule(&params);
    if(rule.able == NULL)
    {
        sic_error_at(expr->loc, "Casting from %s to %s is not allowed.",
                     type_to_string(params.from), type_to_string(params.to));
        return false;
    }


    return rule.able(&params);
}

void perform_cast(ASTExpr* cast)
{
    ASTExpr* inner = cast->expr.cast.inner;
    CastGroup from = s_type_to_group[inner->type->canonical->kind];
    CastGroup to = s_type_to_group[cast->type->canonical->kind];
    SIC_ASSERT(from != CAST_GROUP_INVALID && to != CAST_GROUP_INVALID);
    CastRule rule = s_rule_table[from][to];

    rule.conversion(cast, inner);
}

void implicit_cast_varargs(ASTExpr** expr_to_cast)
{
    switch((*expr_to_cast)->type->kind)
    {
    case TYPE_BOOL:
    case TYPE_CHAR:
    case TYPE_BYTE:
    case TYPE_UBYTE:
    case TYPE_SHORT:
    case TYPE_USHORT:
    case TYPE_INT:
    case TYPE_UINT:
        implicit_cast(expr_to_cast, g_type_ulong);
        break;
    case TYPE_FLOAT:
        implicit_cast(expr_to_cast, g_type_double);
        break;
    default:
        return;
    }
}


static bool rule_not_defined(UNUSED CastParams* params) { SIC_UNREACHABLE(); }
static bool rule_always(UNUSED CastParams* params) { return true; }
static bool rule_explicit_only(CastParams* params)
{
    if(params->explicit)
        return true;
    cast_error(params, "%s cannot be implicitly converted to %s.",
               type_to_string(params->from), type_to_string(params->to));
    return false;
}

static bool rule_size_change(CastParams* params)
{
    if(params->explicit)
        return true;

    if(params->to_kind == TYPE_CHAR && params->from_kind != TYPE_CHAR)
    {
        cast_error(params, "Casting from %s to char requires explicit cast to "
                   "avoid ambiguity. Use byte or ubyte for single byte integer",
                   type_to_string(params->from));
        return false;
    }

    if(type_size(params->from) <= type_size(params->to))
        return true;

    // TODO: Add bounds checking for warning/error if constant goes out of bounds.
    if(params->inner->kind == EXPR_CONSTANT)
        return true;

    cast_error(params, "Narrowing integer type %s to %s requires explicit cast.",
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
    cast_error(params, "Unable to implicitly cast between pointer types %s and %s.",
               type_to_string(params->from), type_to_string(params->to));
    return false;
}

static bool rule_str_to_ptr(CastParams* params)
{
    TypeKind kind = params->to->pointer_base->kind;

    if(kind != TYPE_CHAR && kind != TYPE_UBYTE)
    {
        cast_error(params, "String literal can only be casted to char* or ubyte*.");
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
        cast_error(params, "Array cannot be assigned because its size is unknown at compile time (try memcpy).");
        return false;
    }
    uint64_t arr_size = params->to->array.ss_size; 
    Type* elem_type = params->to->array.elem_type;
    if(params->inner->kind == EXPR_CONSTANT && params->inner->expr.constant.kind == CONSTANT_STRING)
    {
        if(elem_type->kind != TYPE_CHAR)
        {
            cast_error(params, "String literal can only be casted to char[].");
            return false;
        }
        uint64_t str_len = params->inner->expr.constant.val.str_len;
        if(arr_size >= str_len)
        {
            cast_error(params, "Length of array(%lu) is smaller than length of string literal(%lu).",
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
        cast_error(params, "Casting from %s to %s requires an explicit cast.",
                   type_to_string(params->from), type_to_string(params->to));
        return false;
    }

    return true;
}

// Casting functions

static void cast_any_to_void(UNUSED ASTExpr* cast, UNUSED ASTExpr* inner)
{
    SIC_TODO_MSG("Cast any to void");
}

static void cast_int_to_int(ASTExpr* cast, ASTExpr* inner)
{
    if(inner->kind == EXPR_CONSTANT)
    {
        cast->kind = EXPR_CONSTANT;
        cast->expr.constant.kind = CONSTANT_INTEGER;
        cast->expr.constant.val.i = inner->expr.constant.val.i;
        cast->const_eval = true;
        uint32_t from_bit_width = inner->type->builtin.bit_size;
        uint32_t to_bit_width = cast->type->builtin.bit_size;
        if(to_bit_width == 64)
            return;

        uint64_t val = cast->expr.constant.val.i;
        uint32_t shift = 64 - to_bit_width; 
        bool to_signed = type_is_signed(cast->type);
        if(from_bit_width < to_bit_width) // Extending
        {
            if(to_signed || type_is_unsigned(inner->type))
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

    if(type_size(cast->type) == type_size(inner->type))
    {
        cast->expr.cast.kind = CAST_REINTERPRET;
        return;
    }
    cast->expr.cast.kind = type_is_signed(inner->type) ? 
                            CAST_SINT_EXT_TRUNC : 
                            CAST_UINT_EXT_TRUNC;
}

static void cast_int_to_bool(ASTExpr* cast, ASTExpr* inner)
{
    if(inner->kind == EXPR_CONSTANT)
    {
        cast->kind = EXPR_CONSTANT;
        cast->expr.constant.val.i = (inner->expr.constant.val.i != 0);
        cast->expr.constant.kind = CONSTANT_BOOL;
        cast->const_eval = true;
        return;
    }

    cast->expr.cast.kind = CAST_INT_TO_BOOL;
}

static void cast_int_to_float(ASTExpr* cast, ASTExpr* inner)
{
    if(inner->kind == EXPR_CONSTANT)
    {
        cast->kind = EXPR_CONSTANT;
        cast->expr.constant.val.f = (double)inner->expr.constant.val.i;
        cast->expr.constant.kind = CONSTANT_FLOAT;
        cast->const_eval = true;
        return;
    }

    cast->expr.cast.kind = type_is_signed(inner->type) ? 
                            CAST_SINT_TO_FLOAT : 
                            CAST_UINT_TO_FLOAT;
}

static void cast_int_to_ptr(ASTExpr* cast, ASTExpr* inner)
{
    if(inner->kind == EXPR_CONSTANT)
    {
        SIC_TODO();
    }

    cast->expr.cast.kind = CAST_INT_TO_PTR;
}

static void cast_float_to_float(ASTExpr* cast, ASTExpr* inner)
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

static void cast_float_to_bool(ASTExpr* cast, ASTExpr* inner)
{
    if(inner->kind == EXPR_CONSTANT)
    {
        cast->kind = EXPR_CONSTANT;
        cast->expr.constant.val.i = (inner->expr.constant.val.f != 0.0);
        cast->expr.constant.kind = CONSTANT_BOOL;
        cast->const_eval = true;
        return;
    }

    SIC_TODO();
}

static void cast_float_to_int(ASTExpr* cast, ASTExpr* inner)
{
    if(inner->kind == EXPR_CONSTANT)
    {
        cast->kind = EXPR_CONSTANT;
        cast->expr.constant.val.i = (uint64_t)inner->expr.constant.val.f;
        cast->expr.constant.kind = CONSTANT_INTEGER;
        cast->const_eval = true;
        return;
    }

    cast->expr.cast.kind = type_is_signed(cast->type) ? 
                            CAST_FLOAT_TO_SINT : 
                            CAST_FLOAT_TO_UINT;
}

static void cast_ptr_to_bool(ASTExpr* cast, ASTExpr* inner)
{
    cast->const_eval = inner->const_eval;
    if(inner->kind == EXPR_CONSTANT)
    {
        cast->kind = EXPR_CONSTANT;
        cast->expr.constant.val.i = (inner->expr.constant.val.i != 0);
        cast->expr.constant.kind = CONSTANT_BOOL;
        return;
    }

    cast->expr.cast.kind = CAST_PTR_TO_BOOL;
}

static void cast_ptr_to_int(ASTExpr* cast, ASTExpr* inner)
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

static void cast_ptr_to_ptr(ASTExpr* cast, ASTExpr* inner)
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

static void cast_reinterpret(ASTExpr* cast, UNUSED ASTExpr* inner)
{
    cast->expr.cast.kind = CAST_REINTERPRET;
}

static void cast_anon_arr(ASTExpr* cast, ASTExpr* inner)
{
    Type* to_type = cast->type;
    memcpy(cast, inner, sizeof(ASTExpr));
    cast->type = to_type;
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
#define PTRBOO { rule_always       , cast_ptr_to_bool }
#define PTRINT { rule_explicit_only, cast_ptr_to_int }
#define PTRPTR { rule_ptr_to_ptr   , cast_ptr_to_ptr }
#define STRPTR { rule_str_to_ptr   , cast_reinterpret }
#define ANNARR { rule_anon_to_arr  , cast_anon_arr }
#define DISTIN { rule_distinct     , NULL }

static CastRule s_rule_table[__CAST_GROUP_COUNT][__CAST_GROUP_COUNT] = {
    // FROM             TO:   VOID    BOOL    INT     FLOAT   PTR     ARRAY   ENUM    STRUCT  ANONAR  DIST
    [CAST_GROUP_VOID]     = { NOTDEF, NOALLW, NOALLW, NOALLW, NOALLW, NOALLW, NOALLW, NOALLW, NOTDEF, NOALLW },
    [CAST_GROUP_BOOL]     = { TOVOID, NOTDEF, INTINT, NOALLW, NOALLW, NOALLW, NOALLW, NOALLW, NOTDEF, DISTIN },
    [CAST_GROUP_INT]      = { TOVOID, INTBOO, INTINT, INTFLT, INTPTR, NOALLW, NOALLW, NOALLW, NOTDEF, DISTIN },
    [CAST_GROUP_FLOAT]    = { TOVOID, FLTBOO, FLTINT, FLTFLT, NOALLW, NOALLW, NOALLW, NOALLW, NOTDEF, DISTIN },
    [CAST_GROUP_PTR]      = { TOVOID, PTRBOO, PTRINT, NOALLW, PTRPTR, PTRPTR, NOALLW, NOALLW, NOTDEF, DISTIN },
    [CAST_GROUP_ARRAY]    = { TOVOID, NOALLW, NOALLW, NOALLW, NOALLW, NOALLW, NOALLW, NOALLW, NOTDEF, DISTIN },
    [CAST_GROUP_ENUM]     = { TOVOID, INTBOO, INTINT, INTFLT, INTPTR, NOALLW, NOALLW, NOALLW, NOTDEF, DISTIN },
    [CAST_GROUP_STRUCT]   = { TOVOID, NOALLW, NOALLW, NOALLW, NOALLW, NOALLW, NOALLW, NOALLW, NOTDEF, DISTIN },
    [CAST_GROUP_ANON_ARR] = { TOVOID, NOALLW, NOALLW, NOALLW, STRPTR, ANNARR, NOALLW, NOALLW, NOTDEF, DISTIN },
    [CAST_GROUP_DISTINCT] = { TOVOID, DISTIN, DISTIN, DISTIN, DISTIN, DISTIN, DISTIN, DISTIN, DISTIN, DISTIN },
};

static CastGroup s_type_to_group[__TYPE_COUNT] = {
    [TYPE_INVALID]        = CAST_GROUP_INVALID,
    [TYPE_VOID]           = CAST_GROUP_VOID,
    [TYPE_BOOL]           = CAST_GROUP_BOOL,
    [TYPE_CHAR]           = CAST_GROUP_INT,
    [TYPE_BYTE]           = CAST_GROUP_INT,
    [TYPE_UBYTE]          = CAST_GROUP_INT,
    [TYPE_SHORT]          = CAST_GROUP_INT,
    [TYPE_USHORT]         = CAST_GROUP_INT,
    [TYPE_INT]            = CAST_GROUP_INT,
    [TYPE_UINT]           = CAST_GROUP_INT,
    [TYPE_LONG]           = CAST_GROUP_INT,
    [TYPE_ULONG]          = CAST_GROUP_INT,
    [TYPE_FLOAT]          = CAST_GROUP_FLOAT,
    [TYPE_DOUBLE]         = CAST_GROUP_FLOAT,
    [TYPE_POINTER]        = CAST_GROUP_PTR,
    [TYPE_FUNC_PTR]       = CAST_GROUP_PTR,
    [TYPE_STATIC_ARRAY]   = CAST_GROUP_ARRAY,
    [TYPE_RUNTIME_ARRAY]  = CAST_GROUP_ARRAY,
    [TYPE_ALIAS]          = CAST_GROUP_INVALID,
    [TYPE_ALIAS_DISTINCT] = CAST_GROUP_INVALID,
    [TYPE_ENUM]           = CAST_GROUP_ENUM,
    [TYPE_ENUM_DISTINCT]  = CAST_GROUP_ENUM,
    [TYPE_STRUCT]         = CAST_GROUP_STRUCT,
    [TYPE_UNION]          = CAST_GROUP_STRUCT,
    [TYPE_ANON_ARRAY]     = CAST_GROUP_ANON_ARR,
};

static inline CastRule get_cast_rule(CastParams* params)
{
    CastGroup from = s_type_to_group[params->from_kind];
    CastGroup to = s_type_to_group[params->to_kind];
    SIC_ASSERT(from != CAST_GROUP_INVALID && to != CAST_GROUP_INVALID);
    return s_rule_table[from][to];
}

static inline void cast_error(CastParams* params, const char* message, ...)
{
    if(params->silent) return;
    va_list va;
    va_start(va, message);
    sic_diagnostic_atv(params->inner->loc, DIAG_ERROR, message, va);
    va_end(va);
}
