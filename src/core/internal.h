#pragma once
#include "core.h"
#include "structs.h"
#include "utils/file_utils.h"
#include "utils/lib.h"

// Builtin types (defined in type.c)
extern Type* g_type_void;
extern Type* g_type_u8;
extern Type* g_type_s8;
extern Type* g_type_u16;
extern Type* g_type_s16;
extern Type* g_type_u32;
extern Type* g_type_s32;
extern Type* g_type_u64;
extern Type* g_type_s64;
extern Type* g_type_f32;
extern Type* g_type_f64;
extern Type* g_type_f128;

// Token functions
const char*  tok_kind_to_str(TokenKind kind);
BinaryOpKind tok_to_binary_op(TokenKind kind);
UnaryOpKind  tok_to_unary_op(TokenKind kind);
void sic_error_atv(const char* filepath, const SourceLoc* loc, const char* restrict message, va_list va);

ATTR_PRINTF(3, 4)
static inline void sic_error_at(const char* filepath, const SourceLoc* loc, const char* restrict message, ...)
{
    va_list va;
    va_start(va, message);
    sic_error_atv(filepath, loc, message, va);
    va_end(va);
}

// Lexer functions
void   lexer_init_unit(Lexer* lexer, CompilationUnit* unit);
bool   lexer_advance(Lexer* lexer);
Token* lexer_look_ahead(Lexer* lexer, uint32_t count); // count < LOOK_AHEAD_SIZE

// Parser functions
void parser_init(void);
void parse_unit(CompilationUnit* unit);

// Semantic analysis functions
void semantic_analysis(CompilationUnit* unit);

// Symbol map functions
void      sym_map_init(void);
TokenKind sym_map_get(const char* str);
TokenKind sym_map_getn(const char* str, size_t len);

// Type functions
Type*       builtin_type(TokenKind type_token);
Type*       type_copy(Type* orig);
Type*       pointer_to(Type* base);
const char* type_to_string(Type* type);

static inline bool is_builtin_type(TokenKind kind)
{
    return kind >= TOKEN_TYPENAME_START && kind <= TOKEN_TYPENAME_END;
}

static inline uint32_t type_size(Type* ty)
{
    SIC_ASSERT(ty != NULL);
    if(ty->kind >= TYPE_BUILTIN_START && ty->kind <= TYPE_BUILTIN_END)
        return ty->builtin.size;
    if(ty->kind == TYPE_POINTER)
        return 8;

    SIC_ERROR_DBG("Unimplemented type.");
    return 0;
}
