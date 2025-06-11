#pragma once
#include "core.h"
#include "enums.h"
#include "utils/file_utils.h"
#include "utils/lib.h"

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

typedef struct TranslationUnit  TranslationUnit;
typedef struct Object           Object;
typedef struct ObjFunc          ObjFunc;
typedef struct ObjVar           ObjVar;
typedef struct ASTNode          ASTNode;
typedef struct ASTExpr          ASTExpr;
typedef struct ASTReturn        ASTReturn;
typedef struct Lexer            Lexer;
typedef struct LookAhead        LookAhead;
typedef struct Token            Token;
typedef struct TypeBuiltin      TypeBuiltin;
typedef struct Type             Type;

struct Token
{
    TokenKind   kind;
    const char* loc;
    const char* line_start;
    uint32_t    len;
    uint32_t    line_num;
};

#define LOOK_AHEAD_SIZE 4 // Should be a power of two for fast modulo.

struct LookAhead
{
    Token    buf[LOOK_AHEAD_SIZE];
    uint32_t head;
};

struct Lexer
{
    const char* file_name;
    const char* src_start;
    const char* line_start;
    const char* cur_pos;
    uint32_t    cur_line;
    LookAhead   la_buf;
};

struct ObjFunc
{
    Type*    ret_type;
    Object*  local_objs;
    ASTNode* body;
    Object*  params;
    size_t   param_cnt;
    int      stack_size;
};

struct ObjVar
{
    Type* type;
    int   offset;
};

struct Object
{
    Object*      next;
    Token        symbol;
    ObjKind      kind;
    StorageClass storage;

    union
    {
        ObjFunc  func; // Components of function
        ObjVar   var;  // Components of variable
    };

};

struct ASTNode
{
    NodeKind kind;
    ASTNode* next;
    Token    token;

    union
    {
    Object*  var;
    ASTNode* children;

    };
};

struct ASTExpr
{

};

struct ASTReturn
{

};

struct TypeBuiltin
{
    uint32_t size;
};

struct Type
{
    TypeKind      kind;
    TypeQualifier qualifiers;

    union
    {
        TypeBuiltin builtin;
        Type*       pointer_base;
    };
};


struct TranslationUnit
{
    SIFile  file;
    Object* program;
};

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
const char* tok_kind_to_str(TokenKind kind);
void sic_error_atv(const char* filepath, const Token* t, const char* restrict message, va_list va);

__attribute__((format(printf, 3, 4)))
static inline void sic_error_at(const char* filepath, const Token* t, const char* restrict message, ...)
{
    va_list va;
    va_start(va, message);
    sic_error_atv(filepath, t, message, va);
    va_end(va);
}

// Lexer functions
void   lexer_init_file(Lexer* lexer, const SIFile* input);
bool   lexer_advance(Lexer* lexer);
Token* lexer_look_ahead(Lexer* lexer, uint32_t count); // count < LOOK_AHEAD_SIZE

// Parser functions
void    parser_init(void);
Object* parse_unit(Lexer* lexer);

// Symbol map functions
void      sym_map_init(void);
TokenKind sym_map_get(const char* str);
TokenKind sym_map_getn(const char* str, size_t len);

// Type functions
Type* builtin_type(TokenKind type_token);
Type* type_copy(Type* orig);
Type* pointer_to(Type* base);
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
