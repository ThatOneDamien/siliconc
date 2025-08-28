#pragma once
#include <assert.h>
#include "structs.h"
#include "utils/da.h"
#include "utils/lib.h"

// Compiler globals
extern Cmdline         g_args;
extern CompilerContext g_compiler;

// Builtin types (defined in type.c)
extern Type* g_type_void;
extern Type* g_type_bool;
extern Type* g_type_ubyte;
extern Type* g_type_byte;
extern Type* g_type_ushort;
extern Type* g_type_short;
extern Type* g_type_uint;
extern Type* g_type_int;
extern Type* g_type_ulong;
extern Type* g_type_long;
extern Type* g_type_float;
extern Type* g_type_double;

void run_subprocess(const char** cmd);

// Command line arguments
void process_cmdln_args(int argc, char* argv[]);

// Token functions
const char*  tok_kind_to_str(TokenKind kind);
BinaryOpKind tok_to_binary_op(TokenKind kind);
UnaryOpKind  tok_to_unary_op(TokenKind kind);

// Lexer functions
void lexer_init_unit(Lexer* l, CompilationUnit* unit);
void lexer_advance(Lexer* l);
static inline Token* lexer_peek(Lexer* l)
{
    return l->la_buf.buf + l->la_buf.cur;
}
static inline Token* lexer_prev(Lexer* l)
{
    return l->la_buf.buf + l->la_buf.head;
}
static inline Token* lexer_next(Lexer* l)
{
    return l->la_buf.buf + ((l->la_buf.cur + 1) & LOOK_AHEAD_MASK);
}
static inline Token* lexer_nextnext(Lexer* l)
{
    return l->la_buf.buf + ((l->la_buf.cur + 2) & LOOK_AHEAD_MASK);
}
static inline bool token_is_typename(TokenKind kind)
{
    return kind >= TOKEN_TYPENAME_START && kind <= TOKEN_TYPENAME_END;
}

static inline bool token_is_keyword(TokenKind kind)
{
    return kind >= TOKEN_KEYWORD_START && kind <= TOKEN_KEYWORD_END;
}

// Parser functions
void     parser_init(void);
void     parse_unit(CompilationUnit* unit);
static inline bool expr_is_bad(ASTExpr* expr)
{
    return expr->kind == EXPR_INVALID;
}

static inline bool stmt_is_bad(ASTStmt* stmt)
{
    return stmt->kind == STMT_INVALID;
}

// Semantic analysis functions
void semantic_declaration(CompilationUnit* unit);
void semantic_analysis(ModulePTRDA* modules);

// Symbol map functions
void   sym_map_init(void);
Symbol sym_map_addn(const char* str, uint32_t len, TokenKind* kind);
Symbol sym_map_getn(const char* str, uint32_t len, TokenKind* kind);
static inline Symbol sym_map_add(const char* str, TokenKind* kind)
{
    return sym_map_addn(str, strlen(str), kind);
}
static inline Symbol sym_map_get(const char* str, TokenKind* kind)
{ 
    return sym_map_getn(str, strlen(str), kind);
}

// Type functions
Type*       type_from_token(TokenKind type_token);
Type*       type_copy(const Type* orig);
Type*       type_pointer_to(Type* base);
Type*       type_func_ptr(FuncSignature* signature);
Type*       type_array_of(Type* elem_ty, ASTExpr* size_expr);
bool        type_equal(Type* t1, Type* t2);
uint32_t    type_size(Type* ty);
uint32_t    type_alignment(Type* ty);
const char* type_to_string(Type* type);
static inline bool type_is_builtin(Type* ty)
{
    return ty->kind >= TYPE_BUILTIN_START && ty->kind <= TYPE_BUILTIN_END;
}

static inline bool type_is_integer(Type* ty)
{
    return ty->kind >= TYPE_INTEGER_START && ty->kind <= TYPE_INTEGER_END;
}

static inline bool type_is_signed(Type* ty)
{
    static_assert((TYPE_BYTE & 1) == 1, "Adjust type methods.");
    return type_is_integer(ty) && (ty->kind & 1);
}

static inline bool type_is_unsigned(Type* ty)
{
    static_assert((TYPE_UBYTE & 1) == 0, "Adjust type methods.");
    return type_is_integer(ty) && !(ty->kind & 1);
}

static inline bool type_is_float(Type* ty)
{
    return ty->kind >= TYPE_FLOAT_START && ty->kind <= TYPE_FLOAT_END;
}

static inline bool type_is_numeric(Type* ty)
{
    return (ty->kind >= TYPE_NUMERIC_START && ty->kind <= TYPE_NUMERIC_END) ||
           ty->kind == TYPE_ENUM;
}

static inline bool type_is_pointer(Type* ty)
{
    return ty->kind == TYPE_POINTER;
}

static inline bool type_is_array(Type* ty)
{
    return ty->kind == TYPE_SS_ARRAY || ty->kind == TYPE_DS_ARRAY;
}

static inline bool type_is_user_def(Type* ty)
{
    return ty->kind >= TYPE_USER_DEF_START && ty->kind <= TYPE_USER_DEF_END;
}

static inline bool type_is_trivially_copyable(Type* ty)
{
    return ty->kind != TYPE_DS_ARRAY && type_size(ty) <= 16;
}

static inline Type* type_pointer_base(Type* ptr_ty)
{
    if(type_is_array(ptr_ty))
        return ptr_ty->array.elem_type;
    if(!type_is_pointer(ptr_ty))
        SIC_UNREACHABLE();
    return ptr_ty->pointer_base;
}


// Debug printing functions
// NOTE: These functions are only enabled when in debug mode, in release these are not compiled.

#ifdef SI_DEBUG

void print_all_tokens(Lexer* lexer);
void print_unit(const CompilationUnit* unit);
void print_func(const Object* func);
void print_stmt(const ASTStmt* stmt);
void print_expr(const ASTExpr* expr);

#endif
