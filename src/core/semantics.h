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
    ObjModule* module;
    ObjFunc*   cur_func;
    Object*    circular_def;
    ObjVarDA   locals;
    ObjEnum*   cur_enum; // Used when analyzing enums to find members
    Type*      inferred_type;
    
    Stmt*      break_target;
    Stmt*      continue_target;
    Expr*      result_target;
    bool       type_res_allow_unresolved;
    bool       in_global_init;
    bool       code_is_unreachable;
    bool       has_errored_unreachable;
};

extern SemaContext g_sema;

// Top level
bool analyze_global_var(ObjVar* var);
bool analyze_function_signature(ObjFunc* function);
bool analyze_type_obj(Object* type_obj);
bool analyze_enum_underlying(ObjEnum* enum_);
bool analyze_enum_value(ObjEnum* parent, uint32_t index);
bool resolve_type(Type** type_ref, TypeResFlags flags, SourceLoc error_loc, const char* error_msg);
bool resolve_import(ObjModule* module, ObjImport* import);

// Statements
void analyze_stmt(Stmt* stmt);
bool analyze_if(ASTIf* if_);
bool analyze_stmt_list(Stmt* stmt_list);
void analyze_ct_assert(Stmt* stmt);
bool analyze_declaration(ObjVar* decl);

// Expressions
bool analyze_expr_args(Expr* expr, Type* expected_type);
bool analyze_expr_dispatch(Expr* expr, Type* expected_type);
bool analyze_rvalue_dispatch(Expr* expr, bool mutate);
bool analyze_lvalue_dispatch(Expr* expr, bool will_write);
bool analyze_var_rvalue(Expr* expr);

static inline bool analyze_expr(Expr* expr)
{
    return analyze_expr_args(expr, NULL);
}

static inline bool analyze_rvalue_args(Expr* expr, Type* expected_type, bool mutate)
{
    DBG_ASSERT(expr != NULL);
    return analyze_expr_args(expr, expected_type) && analyze_rvalue_dispatch(expr, mutate);
}

static inline bool analyze_rvalue(Expr* expr)
{
    return analyze_rvalue_args(expr, NULL, true);
}

static inline bool analyze_lvalue(Expr* expr, bool will_write)
{
    DBG_ASSERT(expr != NULL);
    return analyze_expr(expr) && analyze_lvalue_dispatch(expr, will_write);
}


// Casting
bool analyze_explicit_cast(Expr* cast);
bool can_cast(Expr* expr, Type* to, bool silent);
void perform_cast(Expr* expr, Type* to);
bool implicit_cast(Expr* expr, Type* desired);
bool implicit_cast_silent(Expr* expr, Type* desired);
bool implicit_cast_vararg(Expr* arg);

// Misc
void       push_obj(Object* obj);
ObjModule* find_module(ObjModule* start, SymbolLoc symloc, bool allow_private);
Object*    find_obj(const ModulePath* path);
Object*    find_struct_member(ObjStruct* struct_, Symbol sym);
uint32_t   push_scope();
void       pop_scope(uint32_t old);
void       push_labeled_stmt(Stmt* stmt, SymbolLoc label);
void       pop_labeled_stmt(Stmt* stmt, SymbolLoc label);
Stmt*   find_labeled_stmt(SymbolLoc label);

static inline void expr_copy(Expr* dst, const Expr* src)
{
    if(dst == src) return;
    SourceLoc loc = dst->loc;
    *dst = *src;
    dst->loc = loc;
}

static inline void const_int_correct(Expr* expr)
{
    Type* ctype = type_reduce(expr->type);
    DBG_ASSERT(type_is_integer(ctype));
    DBG_ASSERT(expr->kind == EXPR_CONSTANT && expr->expr.constant.kind == CONSTANT_INTEGER);
    BitSize shift = 128 - ctype->builtin.bit_size;
    if(shift == 0) return;
    Int128 val = i128_shl64(expr->expr.constant.i, shift);
    if(type_is_signed(ctype))
        expr->expr.constant.i = i128_ashr64(val, shift);
    else
        expr->expr.constant.i = i128_lshr64(val, shift);
}

static inline void set_circular_def(Object* obj)
{
    sic_error_at(obj->loc, "Circular definition.");
    g_sema.circular_def = obj;
    invalidate_obj(obj);
}

static inline void check_circular_def(Object* other, SourceLoc loc)
{
    if(g_sema.circular_def == other)
        g_sema.circular_def = NULL;
    else if(g_sema.circular_def != NULL)
        sic_diagnostic_at(DIAG_NOTE, loc, "From declaration here.");
    invalidate_obj(other);
}

static inline Type* flatten_type(Type* type)
{
    while(true)
    {
        type = type->canonical; // Remove aliasing
        switch(type->kind)
        {
        case TYPE_ALIAS_DISTINCT:
            type = obj_as_typedef(type->user_def)->alias.type;
            continue;
        case TYPE_ENUM_DISTINCT:
        default:
            return type;
        }
    }
}

static inline void convert_to_constant(Expr* expr, Type* to, ConstantKind kind)
{
    expr->kind = EXPR_CONSTANT;
    expr->is_const_eval = true;
    expr->expr.constant.kind = kind;
    expr->type = to;
}

static inline void convert_to_const_bool(Expr* expr, Type* to, bool value)
{
    DBG_ASSERT(to->canonical->kind == TYPE_BOOL);
    convert_to_constant(expr, to, CONSTANT_BOOL);
    expr->expr.constant.b = value;
}

static inline void convert_to_const_float(Expr* expr, Type* to, double value)
{
    DBG_ASSERT(type_is_float(to->canonical));
    convert_to_constant(expr, to, CONSTANT_FLOAT);
    expr->expr.constant.f = value;
}

static inline void convert_to_const_int(Expr* expr, Type* to, Int128 value)
{
    DBG_ASSERT(type_is_integer(type_reduce(to)));
    convert_to_constant(expr, to, CONSTANT_INTEGER);
    expr->expr.constant.i = value;
    const_int_correct(expr);
}

static inline void convert_to_const_pointer(Expr* expr, Type* to, Int128 value)
{
    convert_to_constant(expr, to, CONSTANT_POINTER);
    expr->expr.constant.i = value;
}
