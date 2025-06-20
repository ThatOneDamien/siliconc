#pragma once

typedef enum
{
    TOKEN_INVALID = 0,

    TOKEN_IDENT,            // Identifier
    TOKEN_INT_LITERAL,
    TOKEN_CHAR_LITERAL,
    TOKEN_FLOAT_LITERAL,
    TOKEN_STRING_LITERAL,


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
    TOKEN_EQ,               // ==
    TOKEN_NE,               // !=
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
    TOKEN_AS,
    TOKEN_KEYWORD_START = TOKEN_AS,
    TOKEN_CONST,
    TOKEN_EXTERN,
    TOKEN_PRIV,
    TOKEN_PROT,
    TOKEN_PUB,
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

    TOKEN_EOF,               // End Of File Token
    __TOKEN_COUNT
} TokenKind;

typedef enum
{
    CONST_INVALID = 0,
    CONSTANT_INTEGER,
    CONSTANT_FLOAT,
    CONSTANT_STRING,
} ConstantKind;

typedef enum
{
    EXPR_INVALID = 0,
    
    EXPR_NOP,
    EXPR_BINARY,
    EXPR_CAST,
    EXPR_CONSTANT,
    EXPR_FUNC_CALL,
    EXPR_PRE_SEMANTIC_IDENT,
    EXPR_IDENT,
    EXPR_TERNARY,
    EXPR_UNARY,
} ExprKind;

typedef enum
{
    BINARY_INVALID = 0,
    BINARY_ADD,
    BINARY_SUB,
    BINARY_MUL,
    BINARY_DIV,
    BINARY_MOD,
    BINARY_LOG_OR,
    BINARY_LOG_AND,
    BINARY_EQ,
    BINARY_NE,
    BINARY_LT,
    BINARY_LE,
    BINARY_GT,
    BINARY_GE,
    BINARY_SHL,
    BINARY_SHR,
    BINARY_BIT_OR,
    BINARY_BIT_XOR,
    BINARY_BIT_AND,
    BINARY_ASSIGN,
    BINARY_ADD_ASSIGN,
    BINARY_SUB_ASSIGN,
    BINARY_MUL_ASSIGN,
    BINARY_DIV_ASSIGN,
    BINARY_MOD_ASSIGN,
    BINARY_BIT_OR_ASSIGN,
    BINARY_BIT_XOR_ASSIGN,
    BINARY_BIT_AND_ASSIGN,
    BINARY_SHL_ASSIGN,
    BINARY_SHR_ASSIGN,
} BinaryOpKind;

typedef enum
{
    UNARY_INVALID = 0,

    UNARY_ADDR_OF,
    UNARY_DEREF,
    UNARY_NEG,
} UnaryOpKind;

typedef enum
{
    NODE_INVALID = 0,

    NODE_BLOCK,
    NODE_SINGLE_DECL,
    NODE_MULTI_DECL,
    NODE_EXPR_STMT,
    NODE_RETURN,
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

    TYPE_S8,
    TYPE_INTEGER_START = TYPE_S8,
    TYPE_NUMERIC_START = TYPE_S8,
    TYPE_U8,
    TYPE_S16,
    TYPE_U16,
    TYPE_S32,
    TYPE_U32,
    TYPE_S64,
    TYPE_U64,
    TYPE_INTEGER_END = TYPE_U64,

    TYPE_F32,
    TYPE_FLOAT_START = TYPE_F32,
    TYPE_F64,
    TYPE_FLOAT_END = TYPE_F64,
    TYPE_NUMERIC_END = TYPE_F64,
    TYPE_BUILTIN_END = TYPE_F64,

    TYPE_POINTER,
    // TODO: Add structs, unions, arrays, pointers, etc

} TypeKind;

typedef enum
{
    ACCESS_PRIVATE,     // Accessible only to compilation unit
    ACCESS_PROTECTED,   // Accessible only to module (default behavior)
    ACCESS_PUBLIC,      // Accessible to all modules that import this module

    ACCESS_DEFAULT = ACCESS_PROTECTED,
} ObjAccess;

typedef enum
{
    ATTR_NONE   = 0,
    ATTR_EXTERN = (1 << 0),
} ObjAttr;

typedef enum
{
    QUALIFIER_NONE = 0,
    QUALIFIER_CONST = (1 << 0),
    // QUALIFIER_VOLATILE = (1 << 1),
    // QUALIFIER_RESTRICT = (1 << 2),
} TypeQualifier;

typedef enum
{
    PREC_NONE = 0,
    PREC_ASSIGN,
    PREC_TERNARY,
    PREC_LOG_OR,
    PREC_LOG_AND,
    PREC_BIT_OR,
    PREC_BIT_XOR,
    PREC_BIT_AND,
    PREC_RELATIONAL,
    PREC_ADD_SUB,
    PREC_SHIFTS,
    PREC_MUL_DIV_MOD,
    PREC_UNARY_PREFIX,
    PREC_PRIMARY_POSTFIX
} OpPrecedence;
