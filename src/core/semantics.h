#pragma once
#include "internal.h"

typedef enum : uint8_t
{
    BLOCK_REGULAR     = 0,
    BLOCK_BREAKABLE   = (1 << 0),
    BLOCK_CONTINUABLE = (1 << 2),
    BLOCK_LOOP        = BLOCK_BREAKABLE | BLOCK_CONTINUABLE,
    BLOCK_SWITCH      = BLOCK_BREAKABLE,
} BlockContext;

typedef enum : uint8_t
{
    IDENT_NONE = 0,
    IDENT_VAR  = (1 << 0),
    IDENT_FUNC = (1 << 1),
    IDENT_ENUM = (1 << 2),
} IdentMask;

typedef struct SemaContext SemaContext;
struct SemaContext
{
    CompilationUnit* unit;
    HashMap*         priv_syms;
    HashMap*         prot_syms;
    Object*          cur_func;

    // TODO: Turn this into a real stack that persists
    //       Right now this is very inefficient, as we create
    //       multiple hashmaps per context, and checking is actually slower.
    ScopeDA          scope_stack;
    BlockContext     block_context;
    IdentMask        ident_mask;
    Object*          circular_def;
};

bool    analyze_expr_no_set(SemaContext* c, ASTExpr* expr);
void    analyze_type_obj(SemaContext* c, Object* type_obj, bool is_pointer);
bool    analyze_cast(SemaContext* c, ASTExpr* cast);
bool    implicit_cast(SemaContext* c, ASTExpr** expr_to_cast, Type* desired);
void    implicit_cast_varargs(SemaContext* c, ASTExpr** expr_to_cast);
bool    resolve_type_or_ptr(SemaContext* c, Type* type, bool is_pointer);
void    declare_obj(SemaContext* c, Object* obj);
Object* find_obj(SemaContext* c, Symbol symbol);
bool    expr_is_lvalue(ASTExpr* expr);
static inline bool resolve_type(SemaContext* c, Type* type)
{
    if(!resolve_type_or_ptr(c, type, false))
    {
        type->kind = TYPE_INVALID;
        return false;
    }
    return true;
}
static inline bool analyze_expr(SemaContext* c, ASTExpr* expr)
{
    if(!analyze_expr_no_set(c, expr))
    {
        expr->kind = EXPR_INVALID;
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

static inline void push_scope(SemaContext* c)
{
    da_resize(&c->scope_stack, c->scope_stack.size + 1);
    Scope* s = c->scope_stack.data + (c->scope_stack.size - 1);
    hashmap_clear(&s->objs);
}

static inline void pop_scope(SemaContext* c)
{
    SIC_ASSERT(c->scope_stack.size > 0);
    c->scope_stack.size--;
}

static inline Scope* get_cur_scope(SemaContext* c)
{
    return c->scope_stack.data + (c->scope_stack.size - 1);
}
