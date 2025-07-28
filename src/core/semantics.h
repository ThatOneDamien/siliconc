#pragma once
#include "internal.h"

typedef struct SemaContext SemaContext;
struct SemaContext
{
    CompilationUnit* unit;
    HashMap*         priv_syms;
    HashMap*         prot_syms;
    Object*          cur_func;
    Scope            func_scope;
    Scope*           cur_scope;
};

PRINTF_FMT(3, 4)
static inline void sema_error(const SemaContext* c, const SourceLoc* loc, 
                              const char* restrict message, ...)
{
    va_list va;
    va_start(va, message);
    sic_diagnostic_atv(c->unit->file.full_path, loc, DIAG_ERROR, message, va);
    va_end(va);
}


void    analyze_expr(SemaContext* c, ASTExpr* expr);
bool    analyze_cast(SemaContext* c, ASTExpr* cast);
bool    implicit_cast(SemaContext* c, ASTExpr* expr_to_cast, Type* desired);
void    implicit_cast_varargs(SemaContext* c, ASTExpr* expr_to_cast);
bool    resolve_type(SemaContext* c, Type* type, bool is_pointer);
bool    resolve_struct_type(SemaContext* c, Object* obj, bool is_pointer);
void    declare_obj(SemaContext* c, HashMap* map, Object* obj);
Object* find_obj(SemaContext* c, Symbol symbol);
bool    expr_is_lvalue(SemaContext* c, ASTExpr* expr);

static inline void declare_var(SemaContext* c, Object* var)
{
    declare_obj(c, &c->cur_scope->vars, var);
}

static inline void declare_type(SemaContext* c, Object* type)
{
    declare_obj(c, &c->cur_scope->types, type);
}
