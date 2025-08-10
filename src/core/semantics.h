#pragma once
#include "internal.h"

typedef struct SemaContext SemaContext;
struct SemaContext
{
    CompilationUnit* unit;
    HashMap*         priv_syms;
    HashMap*         prot_syms;
    Object*          cur_func;
    ScopeDA          scope_stack;
};

bool    analyze_expr_no_set(SemaContext* c, ASTExpr* expr);
bool    analyze_cast(SemaContext* c, ASTExpr* cast);
bool    implicit_cast(SemaContext* c, ASTExpr* expr_to_cast, Type* desired);
void    implicit_cast_varargs(SemaContext* c, ASTExpr* expr_to_cast);
bool    resolve_default(ASTExpr* expr, Type* type);
bool    resolve_type(SemaContext* c, Type* type, bool is_pointer);
bool    resolve_struct_type(SemaContext* c, Object* obj, bool is_pointer);
void    declare_obj(SemaContext* c, Object* obj);
Object* find_obj(SemaContext* c, Symbol symbol);
bool    expr_is_lvalue(ASTExpr* expr);
static inline bool analyze_expr(SemaContext* c, ASTExpr* expr)
{
    if(!analyze_expr_no_set(c, expr))
    {
        expr->kind = EXPR_INVALID;
        return false;
    }
    return true;
}
static inline Scope* get_cur_scope(SemaContext* c)
{
    return c->scope_stack.data + (c->scope_stack.size - 1);
}
