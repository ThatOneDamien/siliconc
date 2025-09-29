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
    Object*          cur_func;
    Object*          cyclic_def;

    BlockContext     block_context;
    IdentMask        ident_mask;
    bool             in_ptr : 1;
    bool             in_typedef : 1;
    bool             in_global_init : 1;
};

extern SemaContext g_sema;

void     analyze_global_var(Object* global_var);
bool     analyze_expr_no_set(ASTExpr** expr_ref);
bool     analyze_cast(ASTExpr* cast);
bool     analyze_type_obj(Object* type_obj, Type** o_type, 
                          ResolutionFlags flags, SourceLoc err_loc, const char* err_str);

bool     implicit_cast(ASTExpr** expr_to_cast, Type* desired);
void     implicit_cast_varargs(ASTExpr** expr_to_cast);
bool     resolve_type(Type** type_ref, ResolutionFlags flags, 
                      SourceLoc err_loc, const char* err_str);
void     push_obj(Object* obj);
Object*  find_obj(Symbol symbol);
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

static inline bool analyze_expr(ASTExpr** expr_ref)
{
    if(!analyze_expr_no_set(expr_ref))
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

static inline void set_cyclic_def(Object* obj)
{
    sic_error_at(obj->loc, "Cyclic definition.");
    g_sema.cyclic_def = obj;
    obj->kind = OBJ_INVALID;
    obj->status = STATUS_RESOLVED;
}

static inline void check_cyclic_def(Object* other, SourceLoc loc)
{
    if(g_sema.cyclic_def == other)
        g_sema.cyclic_def = NULL;
    else if(g_sema.cyclic_def != NULL)
        sic_diagnostic_at(loc, DIAG_NOTE, "From declaration here.");
    other->kind = OBJ_INVALID;
    other->status = STATUS_RESOLVED;
}
