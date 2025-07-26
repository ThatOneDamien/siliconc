#include "semantics.h"

typedef struct CastParams CastParams;
typedef struct CastRule   CastRule;

struct CastParams
{
    SemaContext* sema_context;
    ASTExpr*     expr;
    ASTExpr*     inner;
    Type*        from;
    Type*        to;
    CastGroup    from_group;
    CastGroup    to_group;
};

struct CastRule
{
    bool (*able)(CastParams*, bool);
    void (*convert)(ASTExpr*, ASTExpr*);
};

static CastGroup s_type_to_group[__TYPE_COUNT];
static CastRule s_rule_table[__CAST_GROUP_COUNT][__CAST_GROUP_COUNT];

bool analyze_cast(SemaContext* c, ASTExpr* cast)
{
    if(cast->expr.cast.inner->kind == EXPR_INVALID)
        return false;
    CastParams params;
    params.sema_context = c;
    params.expr  = cast;
    params.inner = cast->expr.cast.inner;
    params.from  = cast->expr.cast.inner->type;
    params.to    = cast->type;
    params.from_group = s_type_to_group[params.from->kind];
    params.to_group   = s_type_to_group[params.to->kind];
    if(type_equal(params.from, params.to))
    {
        // Essentially delete the cast expression if it does nothing.
        memcpy(cast, cast->expr.cast.inner, sizeof(ASTExpr));
        return true;
    }

    CastRule rule = s_rule_table[params.from_group][params.to_group];
    if(rule.able == NULL)
    {
        sema_error(c, &cast->loc, "Casting from %s to %s is not allowed.",
                   type_to_string(params.from), type_to_string(params.to));
        return false;
    }

    if(!rule.able(&params, true))
        return false;

    if(rule.convert != NULL)
        rule.convert(cast, cast->expr.cast.inner);

    return true;
}

bool implicit_cast(SemaContext* c, ASTExpr* expr_to_cast, Type* desired)
{
    if(expr_to_cast->kind == EXPR_INVALID)
        return false;
    CastParams params;
    params.sema_context = c;
    params.from = expr_to_cast->type;
    params.to   = desired;
    params.from_group = s_type_to_group[params.from->kind];
    params.to_group   = s_type_to_group[params.to->kind];
    if(type_equal(params.from, params.to))
        return true;

    CastRule rule = s_rule_table[params.from_group][params.to_group];
    if(rule.able == NULL)
    {
        sema_error(c, &expr_to_cast->loc, "Attempted to implicit cast from %s to %s, but it is not allowed.",
                   type_to_string(params.from), type_to_string(params.to));
        return false;
    }

    ASTExpr* new_cast = expr_to_cast;
    expr_to_cast = MALLOC_STRUCT(ASTExpr);
    memcpy(expr_to_cast, new_cast, sizeof(ASTExpr));
    new_cast->kind = EXPR_CAST;
    new_cast->expr.cast.inner = expr_to_cast;
    params.expr = new_cast;
    params.inner = expr_to_cast;

    if(!rule.able(&params, false))
        return false;

    new_cast->type = desired;
    if(rule.convert != NULL)
        rule.convert(new_cast, expr_to_cast);

    return true;
}

void implicit_cast_varargs(SemaContext* c, ASTExpr* expr_to_cast)
{
    bool good = true;
    switch(expr_to_cast->type->kind)
    {
    case TYPE_BOOL:
    case TYPE_UBYTE:
    case TYPE_USHORT:
    case TYPE_UINT:
    case TYPE_BYTE:
    case TYPE_SHORT:
    case TYPE_INT:
        good = implicit_cast(c, expr_to_cast, g_type_ulong);
        break;
    case TYPE_FLOAT:
        good = implicit_cast(c, expr_to_cast, g_type_double);
        break;
    default:
        return;
    }
    if(!good)
        SIC_UNREACHABLE();
}

static bool cast_rule_not_defined(UNUSED CastParams* params, UNUSED bool explicit) { SIC_UNREACHABLE(); }
static bool cast_rule_always(UNUSED CastParams* params, UNUSED bool explicit) { return true; }
static bool cast_rule_explicit_only(CastParams* params, bool explicit)
{
    if(explicit)
        return true;
    sema_error(params->sema_context, &params->expr->loc, "%s cannot be implicitly converted to %s.",
               type_to_string(params->from), type_to_string(params->to));
    return false;
}

static bool cast_rule_size_change(CastParams* params, bool explicit)
{
    if(explicit || params->expr)
        return true;

    TypeKind from_kind = params->from->kind;
    TypeKind to_kind   = params->to->kind;
    if(from_kind < to_kind)
        return true;

    // TODO: Add bounds checking for warning/error if constant goes out of bounds.
    if(params->inner->kind == EXPR_CONSTANT)
        return true;

    sema_error(params->sema_context, &params->expr->loc, 
               "Narrowing integer type %s to %s requires explicit cast.",
               type_to_string(params->from), type_to_string(params->to));
    return false;
}

static bool cast_rule_ptr_to_ptr(CastParams* params, bool explicit)
{
    Type* from_ptr = params->from->pointer_base;
    Type* to_ptr   = params->to->pointer_base;
    if(explicit || from_ptr->kind == TYPE_VOID || to_ptr->kind == TYPE_VOID)
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

    if(from_is_arr && type_equal(from_ptr->array.elem_type, to_ptr))
        return true;

ERR:
    sema_error(params->sema_context, &params->expr->loc, 
               "Unable to implicitly cast between pointers of different type.");
    return false;
}

static bool cast_rule_string_lit(CastParams* params, bool explicit)
{
    (void)explicit;
    if(params->inner->kind == EXPR_CONSTANT && 
       params->inner->expr.constant.kind == CONSTANT_STRING)
        return true;

    sema_error(params->sema_context, &params->expr->loc,
               "Arrays do not implicitly decay to pointers, use the address-of operator '&'.");
    return false;
}


// Casting functions

static void cast_nullptr_to_ptr(ASTExpr* cast, ASTExpr* inner)
{
    memcpy(cast, inner, sizeof(ASTExpr));
}

static void cast_any_to_void(ASTExpr* cast, ASTExpr* inner)
{
    (void)cast;
    (void)inner;
    SIC_TODO_MSG("Cast any to void");
}

static void cast_int_to_int(ASTExpr* cast, ASTExpr* inner)
{
    if(inner->kind == EXPR_CONSTANT)
    {
        cast->kind = EXPR_CONSTANT;
        cast->expr.constant.kind = CONSTANT_INTEGER;
        cast->expr.constant.val.i = inner->expr.constant.val.i;
        uint32_t from_bit_width = type_size(inner->type) * 8;
        uint32_t to_bit_width = type_size(cast->type) * 8;
        if(to_bit_width == 64)
            return;

        uint64_t val = cast->expr.constant.val.i;
        int shift = 64 - to_bit_width; 
        bool to_signed = type_is_signed(cast->type);
        if(from_bit_width < to_bit_width) // Extending
        {
            if(to_signed || type_is_signed(inner->type) == to_signed)
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
        // Handle constants
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
        // Handle constants
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
        // Handle constants
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
        // Handle constants
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
    (void)cast;
    if(inner->kind == EXPR_CONSTANT)
    {
        // Handle constants
    }

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
        // Handle constants
    }

    cast->expr.cast.kind = CAST_PTR_TO_INT;
}

static void cast_ptr_to_ptr(ASTExpr* cast, ASTExpr* inner)
{
    (void)inner;
    cast->expr.cast.kind = CAST_REINTERPRET;
}


#define NOALLW { NULL                   , NULL }
#define NOTDEF { cast_rule_not_defined  , NULL }
#define NULPTR { cast_rule_always       , cast_nullptr_to_ptr }
#define TOVOID { cast_rule_explicit_only, cast_any_to_void }
#define INTINT { cast_rule_size_change  , cast_int_to_int }
#define INTBOO { cast_rule_always       , cast_int_to_bool }
#define INTFLT { cast_rule_always       , cast_int_to_float }
#define INTPTR { cast_rule_explicit_only, cast_int_to_ptr }
#define FLTFLT { cast_rule_size_change  , cast_float_to_float }
#define FLTBOO { cast_rule_explicit_only, cast_float_to_bool }
#define FLTINT { cast_rule_explicit_only, cast_float_to_int }
#define PTRBOO { cast_rule_always       , cast_ptr_to_bool }
#define PTRINT { cast_rule_explicit_only, cast_ptr_to_int }
#define PTRPTR { cast_rule_ptr_to_ptr   , cast_ptr_to_ptr }
#define STRLIT { cast_rule_string_lit   , cast_ptr_to_ptr }

static CastRule s_rule_table[__CAST_GROUP_COUNT][__CAST_GROUP_COUNT] = {
    //                       VOID    NULLPTR BOOL    INT     FLOAT   PTR     ARRAY   STRUCT
    [CAST_GROUP_VOID]    = { NOTDEF, NOTDEF, NOALLW, NOALLW, NOALLW, NOALLW, NOALLW, NOALLW },
    [CAST_GROUP_NULLPTR] = { NOALLW, NOTDEF, NOALLW, NOALLW, NOALLW, NULPTR, NOALLW, NOALLW },
    [CAST_GROUP_BOOL]    = { TOVOID, NOTDEF, NOTDEF, INTINT, NOALLW, NOALLW, NOALLW, NOALLW },
    [CAST_GROUP_INT]     = { TOVOID, NOTDEF, INTBOO, INTINT, INTFLT, INTPTR, NOALLW, NOALLW },
    [CAST_GROUP_FLOAT]   = { TOVOID, NOTDEF, FLTBOO, FLTINT, FLTFLT, NOALLW, NOALLW, NOALLW },
    [CAST_GROUP_PTR]     = { TOVOID, NOTDEF, PTRBOO, PTRINT, NOALLW, PTRPTR, PTRPTR, NOALLW },
    [CAST_GROUP_ARRAY]   = { TOVOID, NOTDEF, NOALLW, NOALLW, NOALLW, STRLIT, NOALLW, NOALLW },
    [CAST_GROUP_STRUCT]  = { TOVOID, NOTDEF, NOALLW, NOALLW, NOALLW, NOALLW, NOALLW, NOALLW },
};

static CastGroup s_type_to_group[__TYPE_COUNT] = {
    [TYPE_INVALID]  = CAST_GROUP_INVALID,
    [TYPE_VOID]     = CAST_GROUP_VOID,
    [TYPE_NULLPTR]  = CAST_GROUP_NULLPTR,
    [TYPE_BOOL]     = CAST_GROUP_BOOL,
    [TYPE_BYTE]     = CAST_GROUP_INT,
    [TYPE_UBYTE]    = CAST_GROUP_INT,
    [TYPE_SHORT]    = CAST_GROUP_INT,
    [TYPE_USHORT]   = CAST_GROUP_INT,
    [TYPE_INT]      = CAST_GROUP_INT,
    [TYPE_UINT]     = CAST_GROUP_INT,
    [TYPE_LONG]     = CAST_GROUP_INT,
    [TYPE_ULONG]    = CAST_GROUP_INT,
    [TYPE_FLOAT]    = CAST_GROUP_FLOAT,
    [TYPE_DOUBLE]   = CAST_GROUP_FLOAT,
    [TYPE_POINTER]  = CAST_GROUP_PTR,
    [TYPE_SS_ARRAY] = CAST_GROUP_ARRAY,
    [TYPE_DS_ARRAY] = CAST_GROUP_ARRAY,
    [TYPE_ENUM]     = CAST_GROUP_INVALID,
    [TYPE_STRUCT]   = CAST_GROUP_STRUCT,
    [TYPE_TYPEDEF]  = CAST_GROUP_INVALID,
    [TYPE_UNION]    = CAST_GROUP_INVALID,
};
