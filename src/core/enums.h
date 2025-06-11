#pragma once

typedef enum
{
    TOKEN_INVALID = 0,

    TOKEN_IDENT,            // Identifier
    TOKEN_STR,              // String Literal
    TOKEN_NUM,              // Numeric Literal (including chars)

    TOKEN_AMP,              // &
    TOKEN_ASTERISK,         // *
    TOKEN_LOG_NOT,          // !
    TOKEN_BIT_NOT,          // ~
    TOKEN_BIT_OR,           // |
    TOKEN_BIT_XOR,          // ^
    TOKEN_COLON,            // :
    TOKEN_SEMI,             // ;
    TOKEN_ASSIGN,           // =
    TOKEN_LT,               // <
    TOKEN_GT,               // >
    TOKEN_DIV,              // /
    TOKEN_PERIOD,           // .
    TOKEN_COMMA,            // ,
    TOKEN_LBRACE,           // {
    TOKEN_LBRACKET,         // [
    TOKEN_LPAREN,           // (
    TOKEN_RPAREN,           // )
    TOKEN_RBRACKET,         // ]
    TOKEN_RBRACE,           // }
    TOKEN_ADD,              // +
    TOKEN_SUB,              // -
    TOKEN_MOD,              // %
    TOKEN_QUESTION,         // ?

    TOKEN_SHR,              // >>
    TOKEN_SHL,              // <<
    TOKEN_LOG_AND,          // &&
    TOKEN_LOG_OR,           // ||
    TOKEN_LOG_EQUIV,        // ==
    TOKEN_LOG_NOT_EQUIV,    // !=
    TOKEN_LE,               // <=
    TOKEN_GE,               // >=
    TOKEN_BIT_AND_ASSIGN,   // &=
    TOKEN_BIT_OR_ASSIGN,    // |=
    TOKEN_BIT_XOR_ASSIGN,   // ^=
    TOKEN_ADD_ASSIGN,       // +=
    TOKEN_SUB_ASSIGN,       // -=
    TOKEN_MUL_ASSIGN,       // *=
    TOKEN_DIV_ASSIGN,       // /=
    TOKEN_MOD_ASSIGN,       // %=
    TOKEN_INCREM,           // ++
    TOKEN_DECREM,           // --
    
    TOKEN_SHR_ASSIGN,       // >>=
    TOKEN_SHL_ASSIGN,       // <<=

    // Keywords
    TOKEN_CONST,
    TOKEN_KEYWORD_START = TOKEN_CONST,
    TOKEN_EXTERN,
    TOKEN_RETURN,

    // Built-in/Primitive type names (Still part of keywords)
    TOKEN_VOID,
    TOKEN_TYPENAME_START = TOKEN_VOID,
    TOKEN_U8,
    TOKEN_S8,
    TOKEN_U16,
    TOKEN_S16,
    TOKEN_U32,
    TOKEN_S32,
    TOKEN_U64,
    TOKEN_S64,
    TOKEN_F32,
    TOKEN_F64,
    TOKEN_TYPENAME_END = TOKEN_F64,
    TOKEN_KEYWORD_END = TOKEN_F64,

    TOKEN_EOF               // End Of File Token
} TokenKind;

typedef enum
{
    NODE_INVALID = 0,
    NODE_NOP,
    NODE_BLOCK,
    NODE_VAR,
    NODE_NUM,
    NODE_RETURN,

    NODE_ASSIGN,
    NODE_TERNARY,
    NODE_LOG_OR,
    NODE_LOG_AND,
    NODE_BIT_OR,
    NODE_BIT_XOR,
    NODE_BIT_AND,
    NODE_EQ,
    NODE_NE,
    NODE_LT,
    NODE_LE,
    NODE_SHL,
    NODE_SHR,
    NODE_ADD,
    NODE_SUB,
    NODE_MUL,
    NODE_DIV,
    NODE_MOD,
    NODE_CAST,
    NODE_INC,
    NODE_DEC,
    NODE_NEG,
    NODE_LOG_NOT,
    NODE_BIT_NOT,
    NODE_ADDR_OF,
    NODE_FUNC_CALL,
    NODE_DEREF,
} NodeKind;

typedef enum
{
    OBJ_INVALID = 0,
    OBJ_VAR,
    OBJ_FUNC,
    OBJ_TYPEDEF
} ObjKind;

typedef enum
{
    TYPE_INVALID = 0,
    TYPE_VOID,
    TYPE_BUILTIN_START = TYPE_VOID,
    TYPE_U8,
    TYPE_UNSIGNED_START = TYPE_U8,
    TYPE_U16,
    TYPE_U32,
    TYPE_U64,
    TYPE_UNSIGNED_END = TYPE_U64,
    TYPE_S8,
    TYPE_SIGNED_START = TYPE_S8,
    TYPE_S16,
    TYPE_S32,
    TYPE_S64,
    TYPE_SIGNED_END = TYPE_S64,
    TYPE_F32,
    TYPE_FLOAT_START = TYPE_F32,
    TYPE_F64,
    TYPE_FLOAT_END = TYPE_F64,
    TYPE_BUILTIN_END = TYPE_F64,

    TYPE_POINTER,
    // TODO: Add structs, unions, arrays, pointers, etc

} TypeKind;

typedef enum
{
    STORAGE_DEFAULT = 0, // Default storage (Different depending on context)
    STORAGE_GLOBAL,      // Global vars
    STORAGE_EXTERN,      // Externally defined vars
    STORAGE_SCOPE,       // Vars with scoped lifetime
} StorageClass;

typedef enum
{
    QUALIFIER_NONE = 0,
    QUALIFIER_CONST = (1 << 0),
    // QUALIFIER_VOLATILE = (1 << 1),
    // QUALIFIER_RESTRICT = (1 << 2),
} TypeQualifier;


