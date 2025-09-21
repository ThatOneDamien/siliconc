#pragma once
#include "internal.h"

typedef enum : uint8_t
{
    BLOCK_REGULAR     = 0,
    BLOCK_BREAKABLE   = 1 << 0,
    BLOCK_CONTINUABLE = 1 << 1,
    BLOCK_LOOP        = BLOCK_BREAKABLE | BLOCK_CONTINUABLE,
    BLOCK_SWITCH      = BLOCK_BREAKABLE,
} BlockContext;

typedef enum : uint8_t
{
    IDENT_VAR_ONLY = 0,
    IDENT_FUNC     = 1 << 0,
    IDENT_ENUM     = 1 << 1,
} IdentMask;

typedef struct SemaContext SemaContext;
struct SemaContext
{
    CompilationUnit* unit;
    HashMap*         priv_syms;
    HashMap*         prot_syms;
    Object*          cur_func;

    BlockContext     block_context;
    IdentMask        ident_mask;
    Object*          circular_def;
};

void     analyze_global_var(SemaContext* c, Object* global_var);
bool     analyze_expr_no_set(SemaContext* c, ASTExpr** expr_ref);
bool     analyze_type_obj(SemaContext* c, Object* type_obj, Type** o_type, ResolutionFlags flags);
bool     analyze_cast(SemaContext* c, ASTExpr* cast);
bool     implicit_cast(SemaContext* c, ASTExpr** expr_to_cast, Type* desired);
void     implicit_cast_varargs(SemaContext* c, ASTExpr** expr_to_cast);
bool     resolve_type(SemaContext* c, Type** type_ref, ResolutionFlags flags);
void     push_obj(Object* obj);
Object*  find_obj(SemaContext* c, Symbol symbol);
uint32_t push_scope();
void     pop_scope(uint32_t old);
bool     expr_is_lvalue(ASTExpr* expr);
bool     expr_is_const_eval(ASTExpr* expr);

static inline void const_int_correct(ASTExpr* expr)
{
    SIC_ASSERT(expr->kind == EXPR_CONSTANT &&
               expr->expr.constant.kind == CONSTANT_INTEGER);
    BitSize shift = expr->type->builtin.bit_size;
    uint64_t val = expr->expr.constant.val.i;
    if(type_is_signed(expr->type))
        expr->expr.constant.val.i = (int64_t)(val << shift) >> shift;
    else
        expr->expr.constant.val.i = (val << shift) >> shift;
}

static inline bool expr_ensure_lvalue(ASTExpr* expr)
{
    if(!expr_is_lvalue(expr))
    {
        sic_error_at(expr->loc, "Expression is not assignable.");
        return false;
    }
    return true;
}

static inline bool analyze_expr(SemaContext* c, ASTExpr** expr_ref)
{
    if(!analyze_expr_no_set(c, expr_ref))
    {
        (*expr_ref)->kind = EXPR_INVALID;
        return false;
    }
    return true;
}

static inline bool obj_is_type(Object* obj)
{
    switch(obj->kind)
    {
    case OBJ_ALIAS_EXPR:
    case OBJ_ENUM_VALUE:
    case OBJ_FUNC:
    case OBJ_VAR:
        return false;
    case OBJ_INVALID:
    case OBJ_BITFIELD:
    case OBJ_ENUM:
    case OBJ_STRUCT:
    case OBJ_TYPE_ALIAS:
    case OBJ_TYPE_DISTINCT:
    case OBJ_UNION:
        return true;
    }
    SIC_UNREACHABLE();
}

static inline void set_circular_def(SemaContext* c, Object* obj)
{
    sic_error_at(obj->loc, "Circular type definition.");
    c->circular_def = obj;
    obj->kind = OBJ_INVALID;
    obj->status = STATUS_RESOLVED;
}

static inline void check_circular_def(SemaContext* c, Object* other, SourceLoc loc)
{
    if(c->circular_def == other)
        c->circular_def = NULL;
    else if(c->circular_def != NULL)
        sic_diagnostic_at(loc, DIAG_NOTE, "From declaration here.");
    other->kind = OBJ_INVALID;
}
