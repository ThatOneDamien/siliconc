#pragma once
#include "internal.h"

// I do minus 1 here to keep the stack_top and stack_bottom members
// on the same page as the end of the data array so that there will
// be less cache misses hopefully.
#define OBJ_STACK_SIZE ((1 << 16) - 1)

typedef enum : uint8_t
{
    BLOCK_REGULAR     = 0,
    BLOCK_BREAKABLE   = 1 << 0,
    BLOCK_CONTINUABLE = 1 << 1,
    BLOCK_LOOP        = BLOCK_BREAKABLE | BLOCK_CONTINUABLE,
    BLOCK_SWITCH      = BLOCK_BREAKABLE,
} BlockContext;

typedef struct SemaContext SemaContext;
struct SemaContext
{
    ObjModule*   module;
    ObjFunc*     cur_func;
    Object*      cyclic_def;
    
    BlockContext block_context;
    bool         in_ptr : 1;
    bool         in_typedef : 1;
    bool         in_global_init : 1;

};

typedef struct ObjStack ObjStack;
struct ObjStack
{
    Object*  data[OBJ_STACK_SIZE];
    uint32_t stack_top;
    uint32_t stack_bottom;
};

extern SemaContext* g_sema;
extern ObjStack     g_obj_stack;

bool       analyze_global_var(ObjVar* var);
bool       analyze_function(ObjFunc* function);
bool       analyze_expr(ASTExpr* expr);
bool       analyze_lvalue(ASTExpr* expr, bool allow_func);
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
bool       expr_is_lvalue(ASTExpr* expr);

static inline void implicit_cast_ensured(ASTExpr** expr_to_cast, Type* desired)
{
    SIC_ASSERT(implicit_cast(expr_to_cast, desired));
}

static inline void const_int_correct(ASTExpr* expr)
{
    SIC_ASSERT(expr->kind == EXPR_CONSTANT &&
               expr->expr.constant.kind == CONSTANT_INTEGER);
    Type* ctype = expr->type->canonical;
    BitSize shift = ctype->builtin.bit_size;
    uint64_t val = expr->expr.constant.val.i;
    if(type_is_signed(ctype))
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

static inline bool obj_is_type(Object* obj)
{
    switch(obj->kind)
    {
    case OBJ_ENUM_VALUE:
    case OBJ_FUNC:
    case OBJ_IMPORT:
    case OBJ_MODULE:
    case OBJ_VAR:
        return false;
    case OBJ_INVALID:
    case OBJ_BITFIELD:
    case OBJ_ENUM:
    case OBJ_STRUCT:
    case OBJ_TYPEDEF:
    case OBJ_UNION:
        return true;
    }
    SIC_UNREACHABLE();
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
