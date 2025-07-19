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
bool    analyze_cast(SemaContext* c, ASTExpr* cast);
bool    implicit_cast(SemaContext* c, ASTExpr* expr_to_cast, Type* desired);
void    implicit_cast_varargs(SemaContext* c, ASTExpr* expr_to_cast);
bool    resolve_type(SemaContext* c, Type* type, bool is_pointer);
bool    resolve_struct_type(SemaContext* c, Object* obj, bool is_pointer);
void    declare_obj(SemaContext* c, Object* obj);
Object* find_obj(SemaContext* c, SourceLoc* symbol);
