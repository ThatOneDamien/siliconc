#pragma once
#include "internal.h"

typedef enum : uint8_t
{
    ANALYZE_VALUE,
    ANALYZE_ADDRESS,
} AnalysisKind;

typedef struct SemaContext SemaContext;
struct SemaContext
{
    ObjModule*   module;
    ObjFunc*     cur_func;
    Object*      cyclic_def;
    
    ASTStmt*     break_target;
    ASTStmt*     continue_target;
    bool         in_ptr : 1;
    bool         in_typedef : 1;
    bool         in_global_init : 1;

};

extern SemaContext* g_sema;

bool       analyze_global_var(ObjVar* var);
bool       analyze_function(ObjFunc* function);
bool       analyze_expr(ASTExpr* expr);
bool       analyze_lvalue(ASTExpr* expr, bool will_write);
bool       analyze_cast(ASTExpr* cast);
bool       analyze_type_obj(Object* type_obj, Type** o_type, ResolutionFlags flags, SourceLoc err_loc, const char* err_str);
bool       analyze_enum(ObjEnum* enum_, Type** o_type);
bool       analyze_struct(ObjStruct* struct_, Type** o_type);
bool       analyze_typedef(ObjTypedef* typedef_, Type** o_type, ResolutionFlags flags, SourceLoc err_loc, const char* err_str);
bool       analyze_union(ObjStruct* union_, Type** o_type);
bool       implicit_cast(ASTExpr** expr_to_cast, Type* desired);
bool       implicit_cast_vararg(ASTExpr** arg);
bool       resolve_import(ObjModule* module, ObjImport* import);
bool       resolve_type(Type** type_ref, ResolutionFlags flags, SourceLoc err_loc, const char* err_str);
void       push_obj(Object* obj);
ObjModule* find_module(ObjModule* start, SymbolLoc symloc, bool allow_private);
Object*    find_obj(ModulePath* path);
uint32_t   push_scope();
void       pop_scope(uint32_t old);
void       push_labeled_stmt(ASTStmt* stmt, SymbolLoc label);
void       pop_labeled_stmt(ASTStmt* stmt, SymbolLoc label);
ASTStmt*   find_labeled_stmt(SymbolLoc label);
void       set_object_link_name(Object* obj);

static inline void implicit_cast_ensured(ASTExpr** expr_to_cast, Type* desired)
{
    DBG_ASSERT(implicit_cast(expr_to_cast, desired));
}

static inline void const_int_correct(ASTExpr* expr)
{
    DBG_ASSERT(expr->kind == EXPR_CONSTANT &&
               expr->expr.constant.kind == CONSTANT_INTEGER);
    Type* ctype = expr->type->canonical;
    BitSize shift = 128 - ctype->builtin.bit_size;
    Int128 val = i128_shl64(expr->expr.constant.i, shift);
    if(type_is_signed(ctype))
        expr->expr.constant.i = i128_ashr64(val, shift);
    else
        expr->expr.constant.i = i128_lshr64(val, shift);
}

static inline void set_cyclic_def(Object* obj)
{
    sic_error_at(obj->loc, "Cyclic definition.");
    g_sema->cyclic_def = obj;
    obj->kind = OBJ_INVALID;
    obj->status = STATUS_RESOLVED;
}

static inline void check_cyclic_def(Object* other, SourceLoc loc)
{
    if(g_sema->cyclic_def == other)
        g_sema->cyclic_def = NULL;
    else if(g_sema->cyclic_def != NULL)
        sic_diagnostic_at(DIAG_NOTE, loc, "From declaration here.");
    other->kind = OBJ_INVALID;
    other->status = STATUS_RESOLVED;
}

static inline Type* flatten_type(Type* type)
{
    while(true)
    {
        type = type->canonical; // Remove aliasing
        switch(type->kind)
        {
        case TYPE_ALIAS_DISTINCT:
            type = type->typedef_->alias.type;
            continue;
        case TYPE_ENUM_DISTINCT:
        default:
            return type;
        }
    }
}
