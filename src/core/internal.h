#pragma once
#include "structs.h"
#include "utils/da.h"
#include "utils/file_utils.h"
#include "utils/lib.h"

// Compiler globals
extern Cmdline         g_args;
extern CompilerContext g_compiler;

// Builtin types (defined in type.c)
extern Type* g_type_void;
extern Type* g_type_nullptr;
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

void run_subprocess(char** cmd);

// Command line arguments
void process_cmdln_args(int argc, char* argv[]);

// Token functions
const char*  tok_kind_to_str(TokenKind kind);
BinaryOpKind tok_to_binary_op(TokenKind kind);
UnaryOpKind  tok_to_unary_op(TokenKind kind);

// Lexer functions
void lexer_init_unit(Lexer* lexer, CompilationUnit* unit);
void lexer_set_pos_in_unit(Lexer* lexer, CompilationUnit* unit, SourceLoc* start);
void lexer_advance(Lexer* lexer);
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
void     parse_ambiguous_decl(Lexer* l, ASTStmt* ambiguous);
ASTExpr* parse_ambiguous_expr(Lexer* l); 

// Semantic analysis functions
void semantic_analysis(CompilationUnit* unit);

// Symbol map functions
void      sym_map_init(void);
TokenKind sym_map_get(const char* str);
TokenKind sym_map_getn(const char* str, size_t len);

// Type functions
Type*       type_from_token(TokenKind type_token);
Type*       type_copy(const Type* orig);
Type*       type_pointer_to(Type* base);
Type*       type_array_of(Type* elem_ty, ASTExpr* size_expr);
bool        type_equal(Type* t1, Type* t2);
uint32_t    type_size(Type* ty);
uint32_t    type_alignment(Type* ty);
const char* type_to_string(Type* type);
static inline bool type_is_builtin(Type* ty)
{
    return ty->kind >= TYPE_BUILTIN_START && ty->kind <= TYPE_BUILTIN_END;
}

static inline bool type_is_signed(Type* ty)
{
    return ty->kind >= TYPE_SIGNED_START && ty->kind <= TYPE_SIGNED_END;
}

static inline bool type_is_unsigned(Type* ty)
{
    return ty->kind >= TYPE_UNSIGNED_START && ty->kind <= TYPE_UNSIGNED_END;
}

static inline bool type_is_integer(Type* ty)
{
    return ty->kind >= TYPE_INTEGER_START && ty->kind <= TYPE_INTEGER_END;
}

static inline bool type_is_float(Type* ty)
{
    return ty->kind >= TYPE_FLOAT_START && ty->kind <= TYPE_FLOAT_END;
}

static inline bool type_is_numeric(Type* ty)
{
    return ty->kind >= TYPE_NUMERIC_START && ty->kind <= TYPE_NUMERIC_END;
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

static inline Type* type_pointer_base(Type* ptr_ty)
{
    if(type_is_array(ptr_ty))
        return ptr_ty->array.elem_type;
    if(!type_is_pointer(ptr_ty))
        SIC_UNREACHABLE();
    return ptr_ty->pointer_base;
}
