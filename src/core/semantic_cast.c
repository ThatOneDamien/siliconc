#include "semantics.h"

typedef struct CastParams CastParams;
typedef struct CastRule   CastRule;

struct CastParams
{
    SemaContext* sema_context;
    ASTExpr*     expr;
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
    if(cast->expr.cast.expr_to_cast->kind == EXPR_INVALID)
        return false;
    CastParams params;
    params.sema_context = c;
    params.expr = cast;
    params.from = cast->expr.cast.expr_to_cast->type;
    params.to   = cast->expr.cast.cast_type;
    params.from_group = s_type_to_group[params.from->kind];
    params.to_group   = s_type_to_group[params.to->kind];
    if(params.from->kind == params.to->kind && 
       params.from->kind >= TYPE_BUILTIN_START && params.from->kind <= TYPE_BUILTIN_END)
    {
        // Essentially delete the cast expression if it does nothing.
        memcpy(cast, cast->expr.cast.expr_to_cast, sizeof(ASTExpr));
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

    cast->type = params.to;
    if(rule.convert != NULL)
        rule.convert(cast, cast->expr.cast.expr_to_cast);

    return true;
}

void implicit_cast(SemaContext* c, ASTExpr* expr_to_cast, Type* desired)
{
    CastParams params;
    params.sema_context = c;
    params.from = expr_to_cast->type;
    params.to   = desired;
    params.from_group = s_type_to_group[params.from->kind];
    params.to_group   = s_type_to_group[params.to->kind];

    CastRule rule = s_rule_table[params.from_group][params.to_group];
    if(rule.able == NULL)
    {
        sema_error(c, &expr_to_cast->loc, "Attempted to implicit cast from %s to %s, but it is not allowed.",
                   type_to_string(params.from), type_to_string(params.to));
        goto ERR;
    }

    ASTExpr* new_cast = expr_to_cast;
    expr_to_cast = MALLOC_STRUCT(ASTExpr);
    memcpy(expr_to_cast, new_cast, sizeof(ASTExpr));
    new_cast->loc = expr_to_cast->loc;
    new_cast->kind = EXPR_CAST;
    new_cast->expr.cast.expr_to_cast = expr_to_cast;
    params.expr = new_cast;

    if(!rule.able(&params, false))
        goto ERR;

    new_cast->type = desired;
    if(rule.convert != NULL)
        rule.convert(new_cast, expr_to_cast);

    return;
ERR:
    expr_to_cast->kind = EXPR_INVALID;
}

static bool cast_rule_not_defined(UNUSED CastParams* params, UNUSED bool explicit)
{
    SIC_UNREACHABLE();
}

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
    if(explicit)
        return true;

    TypeKind from_kind = params->from->kind;
    TypeKind to_kind   = params->to->kind;
    if(from_kind <= to_kind)
        return true;
    return false;
}

static bool cast_rule_always(UNUSED CastParams* params, UNUSED bool explicit) { return true; }

// Casting functions
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
        // Handle constants
    }

    cast->expr.cast.kind = CAST_WIDEN_OR_NARROW;
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

    cast->expr.cast.kind = type_is_signed(inner->type) ? CAST_SINT_TO_FLOAT : CAST_UINT_TO_FLOAT;
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
        // Handle constants
    }

    cast->expr.cast.kind = CAST_WIDEN_OR_NARROW;
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

    cast->expr.cast.kind = type_is_signed(cast->type) ? CAST_FLOAT_TO_SINT : CAST_FLOAT_TO_UINT;
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
    (void)cast;
    (void)inner;

}


#define NOALLW { NULL                   , NULL }
#define NOTDEF { cast_rule_not_defined  , NULL }
#define TOVOID { cast_rule_explicit_only, cast_any_to_void }
#define INTINT { cast_rule_size_change  , cast_int_to_int }
#define INTFLT { cast_rule_always       , cast_int_to_float }
#define INTPTR { cast_rule_explicit_only, cast_int_to_ptr }
#define FLTFLT { cast_rule_size_change  , cast_float_to_float }
#define FLTINT { cast_rule_explicit_only, cast_float_to_int }
#define PTRINT { cast_rule_explicit_only, cast_ptr_to_int }
#define PTRPTR { cast_rule_explicit_only, cast_ptr_to_ptr }

static CastRule s_rule_table[__CAST_GROUP_COUNT][__CAST_GROUP_COUNT] = {
    //                     VOID    INT     FLOAT   PTR
    [CAST_GROUP_VOID]  = { NOTDEF, NOALLW, NOALLW, NOALLW },
    [CAST_GROUP_INT]   = { TOVOID, INTINT, INTFLT, INTPTR },
    [CAST_GROUP_FLOAT] = { TOVOID, FLTINT, FLTFLT, NOALLW },
    [CAST_GROUP_PTR]   = { TOVOID, PTRINT, NOALLW, PTRPTR },
};

static CastGroup s_type_to_group[__TYPE_COUNT] = {
    [TYPE_INVALID]       = CAST_GROUP_INVALID,
    [TYPE_VOID]          = CAST_GROUP_VOID,
    [TYPE_U8]            = CAST_GROUP_INT,
    [TYPE_U16]           = CAST_GROUP_INT,
    [TYPE_U32]           = CAST_GROUP_INT,
    [TYPE_U64]           = CAST_GROUP_INT,
    [TYPE_S8]            = CAST_GROUP_INT,
    [TYPE_S16]           = CAST_GROUP_INT,
    [TYPE_S32]           = CAST_GROUP_INT,
    [TYPE_S64]           = CAST_GROUP_INT,
    [TYPE_F32]           = CAST_GROUP_FLOAT,
    [TYPE_F64]           = CAST_GROUP_FLOAT,
    [TYPE_POINTER]       = CAST_GROUP_PTR,
};
