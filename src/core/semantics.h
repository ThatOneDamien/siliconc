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
    ObjVarDA     locals;
    
    ASTStmt*     break_target;
    ASTStmt*     continue_target;
    bool         type_res_allow_unresolved;
    bool         in_global_init;
    bool         code_is_unreachable;
    bool         has_errored_unreachable;
};

extern SemaContext g_sema;

// Top level
bool analyze_global_var(ObjVar* var);
bool analyze_function_signature(ObjFunc* function);
bool analyze_type_obj(Object* type_obj);
bool resolve_type(Type** type_ref, TypeResFlags flags, SourceLoc error_loc, const char* error_msg);
bool resolve_import(ObjModule* module, ObjImport* import);

// Statements
bool analyze_stmt_block(ASTStmt* stmt);
void analyze_ct_assert(ASTStmt* stmt);
bool analyze_declaration(ObjVar* decl);

// Expressions
bool analyze_expr(ASTExpr* expr);
bool analyze_rvalue(ASTExpr* expr);
bool analyze_rvalue_no_mutate(ASTExpr* expr);
bool analyze_lvalue(ASTExpr* expr, bool will_write);
bool analyze_explicit_cast(ASTExpr* cast);

// Casting
bool can_cast(ASTExpr* expr, Type* to, bool silent);
void perform_cast(ASTExpr* expr, Type* to);
bool implicit_cast(ASTExpr* expr, Type* desired);
bool implicit_cast_vararg(ASTExpr* arg);

// Misc
void       push_obj(Object* obj);
ObjModule* find_module(ObjModule* start, SymbolLoc symloc, bool allow_private);
Object*    find_obj(ModulePath* path);
uint32_t   push_scope();
void       pop_scope(uint32_t old);
void       push_labeled_stmt(ASTStmt* stmt, SymbolLoc label);
void       pop_labeled_stmt(ASTStmt* stmt, SymbolLoc label);
ASTStmt*   find_labeled_stmt(SymbolLoc label);
void       set_object_link_name(Object* obj);

static inline void expr_copy(ASTExpr* dst, const ASTExpr* src)
{
    if(dst == src) return;
    SourceLoc loc = dst->loc;
    *dst = *src;
    dst->loc = loc;
}

static inline void const_int_correct(ASTExpr* expr)
{
    DBG_ASSERT(expr->kind == EXPR_CONSTANT && expr->expr.constant.kind == CONSTANT_INTEGER);
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
    g_sema.cyclic_def = obj;
    obj->kind = OBJ_INVALID;
    obj->status = STATUS_RESOLVED;
}

static inline void check_cyclic_def(Object* other, SourceLoc loc)
{
    if(g_sema.cyclic_def == other)
        g_sema.cyclic_def = NULL;
    else if(g_sema.cyclic_def != NULL)
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

static inline void convert_to_constant(ASTExpr* expr, ConstantKind kind)
{
    expr->kind = EXPR_CONSTANT;
    expr->is_const_eval = true;
    expr->expr.constant.kind = kind;
}

static inline void convert_to_const_bool(ASTExpr* expr, bool value)
{
    DBG_ASSERT(expr->type == g_type_bool);
    convert_to_constant(expr, CONSTANT_BOOL);
    expr->expr.constant.b = value;
}

static inline void convert_to_const_float(ASTExpr* expr, double value)
{
    DBG_ASSERT(type_is_float(expr->type->canonical));
    convert_to_constant(expr, CONSTANT_FLOAT);
    expr->expr.constant.f = value;
}

static inline void convert_to_const_int(ASTExpr* expr, Int128 value)
{
    DBG_ASSERT(type_is_integer(expr->type->canonical));
    convert_to_constant(expr, CONSTANT_INTEGER);
    expr->expr.constant.i = value;
    const_int_correct(expr);
}

static inline void convert_to_const_pointer(ASTExpr* expr, Int128 value)
{
    convert_to_constant(expr, CONSTANT_POINTER);
    expr->expr.constant.i = value;
}
