#pragma once
#include "internal.h"

typedef struct SemaContext SemaContext;
struct SemaContext
{
    CompilationUnit* unit;
    Object*          cur_func;
    Scope            unit_scope;
    Scope*           cur_scope;
};

PRINTF_FMT(3, 4)
static inline void sema_error(const SemaContext* c, const SourceLoc* loc, 
                              const char* restrict message, ...)
{
    va_list va;
    va_start(va, message);
    sic_error_atv(c->unit->file.full_path, loc, message, va);
    va_end(va);
}

void    analyze_expr(SemaContext* c, ASTExpr* expr);
void    analyze_cast(SemaContext* c, ASTExpr* cast);
void    declare_obj(SemaContext* c, Object* obj);
Object* find_obj(SemaContext* c, SourceLoc* symbol);
