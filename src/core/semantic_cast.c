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
};

struct CastRule
{
    bool (*able)(CastParams*, bool);
    void (*convert)(ASTExpr*, ASTExpr*);
};

static CastGroup s_type_to_group[__TYPE_COUNT];
static CastRule s_rule_table[__CAST_GROUP_COUNT][__CAST_GROUP_COUNT];

static inline CastGroup type_to_group(Type* type);

bool analyze_cast(ASTExpr* cast)
{
    if(!resolve_type(&cast->type, RES_ALLOW_VOID, cast->loc, "Cannot cast to type") || 
       !analyze_expr(&cast->expr.cast.inner))
        return false;
    ASTExpr* inner = cast->expr.cast.inner;
    CastParams params;
    params.inner = inner;
    params.from  = inner->type;
    params.to    = cast->type;
    params.from_kind = params.from->kind;
    params.to_kind   = params.to->kind;
    if(type_equal(params.from, params.to))
    {
        // Essentially delete the cast expression if it does nothing.
        memcpy(cast, inner, sizeof(ASTExpr));
        return true;
    }

    CastRule rule = s_rule_table[type_to_group(params.from)][type_to_group(params.to)];
    if(rule.able == NULL)
    {
        sic_error_at(cast->loc, "Casting from %s to %s is not allowed.",
                     type_to_string(params.from), type_to_string(params.to));
        return false;
    }

    if(!rule.able(&params, true))
    {
        cast->kind = EXPR_INVALID;
        return false;
    }

    if(rule.convert != NULL)
        rule.convert(cast, inner);

    return true;
}

bool implicit_cast(ASTExpr** expr_to_cast, Type* desired)
{
    SIC_ASSERT(desired->status == STATUS_RESOLVED);
    if(!analyze_expr(expr_to_cast) || type_is_bad(desired))
        return false;
    ASTExpr* prev = *expr_to_cast;
    CastParams params;
    params.inner        = prev;
    params.from         = prev->type;
    params.to           = desired;
    params.from_kind    = params.from->kind;
    params.to_kind      = params.to->kind;
    if(type_equal(params.from, params.to))
        return true;

    CastRule rule = s_rule_table[type_to_group(params.from)][type_to_group(params.to)];
    if(rule.able == NULL)
    {
        sic_error_at(prev->loc, "Attempted to implicit cast from %s to %s, but it is not allowed.",
                     type_to_string(params.from), type_to_string(params.to));
        return false;
    }


    if(!rule.able(&params, false))
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

    if(rule.convert != NULL)
        rule.convert(new_cast, prev);

    *expr_to_cast = new_cast;
    return true;
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

static bool rule_not_defined(UNUSED CastParams* params, UNUSED bool explicit) { SIC_UNREACHABLE(); }
static bool rule_always(UNUSED CastParams* params, UNUSED bool explicit) { return true; }
static bool rule_explicit_only(CastParams* params, bool explicit)
{
    if(explicit)
        return true;
    sic_error_at(params->inner->loc, "%s cannot be implicitly converted to %s.",
                 type_to_string(params->from), type_to_string(params->to));
    return false;
}

static bool rule_size_change(CastParams* params, bool explicit)
{
    if(explicit)
        return true;

    if(params->to_kind == TYPE_CHAR && params->from_kind != TYPE_CHAR)
    {
        sic_error_at(params->inner->loc, 
                     "Casting from %s to char requires explicit cast to avoid ambiguity. "
                     "Use byte or ubyte for single byte integer",
                     type_to_string(params->from));
        return false;
    }

    if(type_size(params->from) <= type_size(params->to))
        return true;

    // TODO: Add bounds checking for warning/error if constant goes out of bounds.
    if(params->inner->kind == EXPR_CONSTANT)
        return true;

    sic_error_at(params->inner->loc, 
                 "Narrowing integer type %s to %s requires explicit cast.",
                 type_to_string(params->from), type_to_string(params->to));
    return false;
}

static bool rule_ptr_to_ptr(CastParams* params, bool explicit)
{
    if(explicit)
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
    sic_error_at(params->inner->loc, 
                 "Unable to implicitly cast between pointers of different type.");
    return false;
}

static bool rule_str_to_ptr(CastParams* params, UNUSED bool explicit)
{
    if(params->to->pointer_base->kind != TYPE_UBYTE)
    {
        sic_error_at(params->inner->loc, "String literal can only be casted to ubyte*.");
        return false;
    }
    return true;
}

static bool rule_str_to_arr(CastParams* params, UNUSED bool explicit)
{
    if(params->to_kind == TYPE_RUNTIME_ARRAY)
    {
        sic_error_at(params->inner->loc, "Unable to assign runtime-sized array to a string literal.");
        return false;
    }
    if(params->to->array.elem_type->kind != TYPE_UBYTE)
    {
        sic_error_at(params->inner->loc, "String literal can only be casted to ubyte[].");
        return false;
    }
    uint64_t arr_size = params->to->array.ss_size; 
    uint64_t str_len = params->inner->expr.constant.val.str_len;
    if(arr_size != str_len && arr_size != str_len + 1)
    {
        sic_error_at(params->inner->loc, 
                     "Size of array(%lu) does not match size of string literal(%lu).",
                     arr_size,
                     str_len);
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
        return;
    }

    cast->expr.cast.kind = type_is_signed(cast->type) ? 
                            CAST_FLOAT_TO_SINT : 
                            CAST_FLOAT_TO_UINT;
}

static void cast_ptr_to_bool(ASTExpr* cast, ASTExpr* inner)
{
    if(inner->kind == EXPR_CONSTANT)
    {
        cast->kind = EXPR_CONSTANT;
        cast->expr.constant.val.i = (inner->expr.constant.val.i != 0);
        cast->expr.constant.kind = CONSTANT_BOOL;
        return;
    }

    // TODO: Maybe avoid a malloc here?
    ASTExpr* intermediate = MALLOC_STRUCT(ASTExpr);
    memcpy(intermediate, cast, sizeof(ASTExpr));
    intermediate->expr.cast.kind = CAST_PTR_TO_INT;
    intermediate->type = g_type_ulong;
    cast->expr.cast.inner = intermediate;
    cast->expr.cast.kind = CAST_INT_TO_BOOL;
}

static void cast_ptr_to_int(ASTExpr* cast, ASTExpr* inner)
{
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
#define STRARR { rule_str_to_arr   , cast_reinterpret }

static CastRule s_rule_table[__CAST_GROUP_COUNT][__CAST_GROUP_COUNT] = {
    // FROM            TO:   VOID    BOOL    INT     FLOAT   PTR     ARRAY   ENUM    STRUCT  STRLIT
    [CAST_GROUP_VOID]    = { NOTDEF, NOALLW, NOALLW, NOALLW, NOALLW, NOALLW, NOALLW, NOALLW, NOTDEF },
    [CAST_GROUP_BOOL]    = { TOVOID, NOTDEF, INTINT, NOALLW, NOALLW, NOALLW, NOALLW, NOALLW, NOTDEF },
    [CAST_GROUP_INT]     = { TOVOID, INTBOO, INTINT, INTFLT, INTPTR, NOALLW, NOALLW, NOALLW, NOTDEF },
    [CAST_GROUP_FLOAT]   = { TOVOID, FLTBOO, FLTINT, FLTFLT, NOALLW, NOALLW, NOALLW, NOALLW, NOTDEF },
    [CAST_GROUP_PTR]     = { TOVOID, PTRBOO, PTRINT, NOALLW, PTRPTR, PTRPTR, NOALLW, NOALLW, NOTDEF },
    [CAST_GROUP_ARRAY]   = { TOVOID, NOALLW, NOALLW, NOALLW, NOALLW, NOALLW, NOALLW, NOALLW, NOTDEF },
    [CAST_GROUP_ENUM]    = { TOVOID, INTBOO, INTINT, INTFLT, INTPTR, NOALLW, NOALLW, NOALLW, NOTDEF },
    [CAST_GROUP_STRUCT]  = { TOVOID, NOALLW, NOALLW, NOALLW, NOALLW, NOALLW, NOALLW, NOALLW, NOTDEF },
    [CAST_GROUP_STRLIT]  = { TOVOID, NOALLW, NOALLW, NOALLW, STRPTR, STRARR, NOALLW, NOALLW, NOTDEF },
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
    [TYPE_IPTR]           = CAST_GROUP_INT,
    [TYPE_UPTR]           = CAST_GROUP_INT,
    [TYPE_ISZ]            = CAST_GROUP_INT,
    [TYPE_USZ]            = CAST_GROUP_INT,
    [TYPE_FLOAT]          = CAST_GROUP_FLOAT,
    [TYPE_DOUBLE]         = CAST_GROUP_FLOAT,
    [TYPE_POINTER]        = CAST_GROUP_PTR,
    [TYPE_FUNC_PTR]       = CAST_GROUP_PTR,
    [TYPE_STATIC_ARRAY]   = CAST_GROUP_ARRAY,
    [TYPE_RUNTIME_ARRAY]  = CAST_GROUP_ARRAY,
    [TYPE_ENUM]           = CAST_GROUP_ENUM,
    [TYPE_STRUCT]         = CAST_GROUP_STRUCT,
    [TYPE_TYPEDEF]        = CAST_GROUP_INVALID,
    [TYPE_UNION]          = CAST_GROUP_STRUCT,
    [TYPE_STRING_LITERAL] = CAST_GROUP_STRLIT,
};

static inline CastGroup type_to_group(Type* type)
{
    SIC_ASSERT(s_type_to_group[type->kind] != CAST_GROUP_INVALID);
    return s_type_to_group[type->kind];
}
