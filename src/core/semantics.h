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
    bool         in_ptr : 1;
    bool         in_typedef : 1;
    bool         in_global_init : 1;

};

extern SemaContext* g_sema;

bool       analyze_global_var(ObjVar* var);
bool       analyze_function(ObjFunc* function);
void       analyze_stmt(ASTStmt* stmt);
bool       analyze_stmt_block(ASTStmt* stmt);
void       analyze_ct_assert(ASTStmt* stmt);
bool       analyze_declaration(ObjVar* decl);
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

static inline void convert_to_constant(ASTExpr* expr, ConstantKind kind)
{
    expr->kind = EXPR_CONSTANT;
    expr->const_eval = true;
    expr->pure = true;
    expr->expr.constant.kind = kind;
}

static inline void convert_to_const_bool(ASTExpr* expr, bool value)
{
    DBG_ASSERT(expr->type->canonical->kind == TYPE_BOOL);
    convert_to_constant(expr, CONSTANT_BOOL);
    expr->expr.constant.b = value;
}

static inline void convert_to_const_char(ASTExpr* expr, uint32_t value)
{
    DBG_ASSERT(type_is_char(expr->type->canonical));
    convert_to_constant(expr, CONSTANT_CHAR);
    expr->expr.constant.c = value;
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

static inline void convert_to_const_pointer(ASTExpr* expr, uint64_t value)
{
    convert_to_constant(expr, CONSTANT_POINTER);
    expr->expr.constant.i = i128_from_u64(value);
}

static inline void convert_to_const_enum(ASTExpr* expr, ObjEnumValue* enum_value)
{
    convert_to_constant(expr, CONSTANT_ENUM);
    expr->type = enum_value->enum_type;
    expr->expr.constant.enum_ = enum_value;
}
