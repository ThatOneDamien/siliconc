#include "semantics.h"

typedef struct CastParams CastParams;
typedef bool (*CastRule)(CastParams* params, bool explicit);
struct CastParams
{
    SemaContext* sema_context;
    ASTExpr*     expr;
    Type*        from;
    Type*        to;
    CastGroup    from_group;
    CastGroup    to_group;
};

static CastGroup s_type_to_group[__TYPE_COUNT];
static CastRule s_rule_table[__CAST_GROUP_COUNT][__CAST_GROUP_COUNT];

void analyze_cast(SemaContext* c, ASTExpr* cast)
{
    CastParams params;
    params.sema_context = c;
    params.expr = cast;
    params.from = cast->expr.cast.expr_to_cast->type;
    params.to   = cast->expr.cast.cast_type;
    params.from_group = s_type_to_group[params.from->kind];
    params.to_group   = s_type_to_group[params.to->kind];

    CastRule rule = s_rule_table[params.from_group][params.to_group];
    if(rule == NULL)
    {
        sema_error(c, &cast->loc, "Casting from %s to %s is not allowed.",
                   type_to_string(params.from), type_to_string(params.to));
        goto ERR;
    }

    if(!rule(&params, true))
        goto ERR;

    cast->type = params.to;
    return;
ERR:
    cast->kind = EXPR_INVALID;
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

#define NOALLW NULL
#define NOTDEF cast_rule_not_defined
#define EXPLIC cast_rule_explicit_only
#define SZCHNG cast_rule_size_change
#define ALWAYS cast_rule_always

static CastRule s_rule_table[__CAST_GROUP_COUNT][__CAST_GROUP_COUNT] = {
    //                     VOID    INT     FLOAT   PTR
    [CAST_GROUP_VOID]  = { NOTDEF, NOALLW, NOALLW, NOALLW },
    [CAST_GROUP_INT]   = { EXPLIC, SZCHNG, ALWAYS, EXPLIC },
    [CAST_GROUP_FLOAT] = { EXPLIC, EXPLIC, SZCHNG, NOALLW },
    [CAST_GROUP_PTR]   = { EXPLIC, EXPLIC, NOALLW, ALWAYS },
};

static CastGroup s_type_to_group[__TYPE_COUNT] = {
    [TYPE_INVALID]       = CAST_GROUP_INVALID,
    [TYPE_VOID]          = CAST_GROUP_VOID,
    [TYPE_S8]            = CAST_GROUP_INT,
    [TYPE_U8]            = CAST_GROUP_INT,
    [TYPE_S16]           = CAST_GROUP_INT,
    [TYPE_U16]           = CAST_GROUP_INT,
    [TYPE_S32]           = CAST_GROUP_INT,
    [TYPE_U32]           = CAST_GROUP_INT,
    [TYPE_S64]           = CAST_GROUP_INT,
    [TYPE_U64]           = CAST_GROUP_INT,
    [TYPE_F32]           = CAST_GROUP_FLOAT,
    [TYPE_F64]           = CAST_GROUP_FLOAT,
    [TYPE_POINTER]       = CAST_GROUP_PTR,
};
