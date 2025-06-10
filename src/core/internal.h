#pragma once
#include "core.h"
#include "enums.h"
#include "utils/file_utils.h"
#include "utils/lib.h"

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

typedef struct Token           Token;
typedef struct LookAhead       LookAhead;
typedef struct Lexer           Lexer;
typedef struct FuncComps       FuncComps;
typedef struct VarComps        VarComps;
typedef struct Object          Object;
typedef struct ASTNode         ASTNode;
typedef struct TypeBuiltin     TypeBuiltin;
typedef struct Type            Type;
typedef struct Scope           Scope;
typedef struct TranslationUnit TranslationUnit;

struct Token
{
    TokenKind   kind;
    Type*       type;
    const char* loc;
    const char* line_start;
    uint32_t    len;
    uint32_t    line_num;

    union
    {
        int64_t     i;
        double      f;
        const char* s;
    } val;
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

struct FuncComps
{
    Type*    ret_type;
    Object*  local_objs;
    ASTNode* body;
    Object*  params;
    size_t   param_cnt;
    int      stack_size;
};

struct VarComps
{
    Type* type;
    int   offset;
};

struct Object
{
    Object*      next;
    Token        symbol;
    StorageClass storage;
    bool         is_function;
    bool         is_declaration;

    union
    {
        FuncComps func; // Components of function
        VarComps  var;  // Components of variable
    } comps;

};

struct ASTNode
{
    NodeKind kind;
    Token    token;
    Object*  var;
    ASTNode* children;
    ASTNode* next;
};

struct TypeBuiltin
{
    uint32_t size;
};

struct Type
{
    TypeKind kind;
    Token    symbol;
    bool     is_const;

    union
    {
        TypeBuiltin builtin;
        Type*       pointer_base;
    } v;
};

struct Scope
{
    Scope*  parent;

    HashMap vars;
    HashMap types;
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
static inline uint32_t type_size(Type* ty)
{
    SIC_ASSERT(ty != NULL);
    if(ty->kind >= TYPE_BUILTIN_START && ty->kind <= TYPE_BUILTIN_END)
        return ty->v.builtin.size;
    if(ty->kind == TYPE_POINTER)
        return 8;

    SIC_ERROR_DBG("Unimplemented type.");
    return 0;
}
