#pragma once
#include <stdint.h>

typedef enum : uint8_t
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
    BINARY_LSHR,
    BINARY_ASHR,
    BINARY_BIT_OR,
    BINARY_BIT_XOR,
    BINARY_BIT_AND,
    BINARY_ASSIGN,
    BINARY_ADD_ASSIGN,
    BINARY_OP_ASSIGN_START = BINARY_ADD_ASSIGN,
    BINARY_SUB_ASSIGN,
    BINARY_MUL_ASSIGN,
    BINARY_DIV_ASSIGN,
    BINARY_MOD_ASSIGN,
    BINARY_BIT_OR_ASSIGN,
    BINARY_BIT_XOR_ASSIGN,
    BINARY_BIT_AND_ASSIGN,
    BINARY_SHL_ASSIGN,
    BINARY_LSHR_ASSIGN,
    BINARY_ASHR_ASSIGN,
    BINARY_OP_ASSIGN_END = BINARY_ASHR_ASSIGN,
} BinaryOpKind;

typedef enum : int8_t
{
    CAST_GROUP_INVALID = -1,
    CAST_GROUP_VOID = 0,
    CAST_GROUP_BOOL,
    CAST_GROUP_INT,
    CAST_GROUP_FLOAT,
    CAST_GROUP_PTR,
    CAST_GROUP_ARRAY,
    CAST_GROUP_STRUCT,
    CAST_GROUP_ANON_ARR,
    CAST_GROUP_DISTINCT,
    __CAST_GROUP_COUNT,
} CastGroup;

typedef enum : uint8_t
{
    CAST_INVALID = 0,
    CAST_FLOAT_TO_SINT,
    CAST_FLOAT_TO_UINT,
    CAST_SINT_TO_FLOAT,
    CAST_UINT_TO_FLOAT,
    CAST_INT_TO_BOOL,
    CAST_PTR_TO_BOOL,
    CAST_PTR_TO_INT,
    CAST_INT_TO_PTR,
    CAST_FLOAT_EXT_TRUNC,
    CAST_SINT_EXT_TRUNC,
    CAST_UINT_EXT_TRUNC,
    CAST_REINTERPRET,
} CastKind;

typedef enum : uint8_t
{
    MODE_NONE = 0,   // NULL Mode
    MODE_COMPILE,    // Compile
    MODE_ASSEMBLE,   // Compile + Assemble
    MODE_LINK        // Compile + Assemble + Link
} CompileMode;

// Could be expanded later to support multiple architectures.
typedef enum : uint8_t
{
    TARGET_NONE = 0,
    TARGET_x86_64,
} CompileTarget;

typedef enum : uint8_t
{
    CONSTANT_INVALID = 0,
    CONSTANT_INTEGER,
    CONSTANT_FLOAT,
    CONSTANT_STRING,
    CONSTANT_POINTER,
} ConstantKind;

typedef enum : uint8_t
{
    DEBUG_NONE    = 0,
    DEBUG_LEXER   = (1 << 0),
    DEBUG_PARSER  = (1 << 1),
    DEBUG_SEMA    = (1 << 2),
    DEBUG_CODEGEN = (1 << 3),
    DEBUG_MEMORY  = (1 << 4),
} DebugOutput;

typedef enum : uint8_t
{
    DIAG_NOTE = 0,
    DIAG_WARNING,
    DIAG_ERROR,
    DIAG_FATAL,
} DiagnosticType;

typedef enum : uint8_t
{
    EXPR_INVALID = 0,
    
    EXPR_ARRAY_ACCESS,
    EXPR_ARRAY_INIT_LIST,
    EXPR_BINARY,
    EXPR_CAST,
    EXPR_CONSTANT,
    EXPR_DEFAULT,
    EXPR_FUNC_CALL,
    EXPR_IDENT,
    EXPR_MEMBER_ACCESS,
    EXPR_POSTFIX,
    EXPR_PS_IDENT,
    EXPR_STRUCT_INIT_LIST,
    EXPR_TERNARY,
    EXPR_TYPE_IDENT,
    EXPR_UNARY,
    EXPR_UNRESOLVED_ARR,
    EXPR_UNRESOLVED_DOT,
    EXPR_ZEROED_OUT,

    EXPR_CT_SIZEOF,
} ExprKind;

typedef enum : uint8_t
{
    FT_UNKNOWN = 0, // Unknown
    FT_SI,          // Silicon source file
    FT_LLVM_IR,     // LLVM IR File (.ll)
    FT_ASM,         // Assembly file
    FT_OBJ,         // Object file
    FT_STATIC,      // Static library (.a)
    FT_SHARED,      // Shared object/library (.so)
} FileType;

typedef enum : uint8_t
{
    IR_NONE = 0,
    IR_LLVM,
    // IR_CUSTOM // Maybe in the future I will do this.
} IRTarget;

typedef enum : uint8_t
{
    VIS_PRIVATE,     // Accessible only within the module
    VIS_PUBLIC,      // Accessible to all modules that import this module

    VIS_DEFAULT = VIS_PRIVATE,
} Visibility;

typedef enum : uint8_t
{
    OBJ_INVALID = 0,
    OBJ_ALIAS_EXPR,
    OBJ_BITFIELD,
    OBJ_ENUM,
    OBJ_ENUM_VALUE,
    OBJ_FUNC,
    OBJ_STRUCT,
    OBJ_TYPE_ALIAS,
    OBJ_UNION,
    OBJ_VAR,
} ObjKind;

typedef enum : uint8_t
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
    PREC_PRIMARY_POSTFIX
} OpPrecedence;

typedef enum
{
    RES_NORMAL           = 0,
    RES_ALLOW_VOID       = 1 << 0,
    RES_ALLOW_AUTO       = 1 << 1,
    RES_ALLOW_AUTO_ARRAY = 1 << 2,
} ResolutionFlags;

typedef enum : uint8_t
{
    STATUS_UNRESOLVED,
    STATUS_RESOLVING,
    STATUS_RESOLVED
} ResolveStatus;

typedef enum : uint8_t
{
    STMT_INVALID = 0,

    STMT_BLOCK,
    STMT_BREAK,
    STMT_CONTINUE,
    STMT_EXPR_STMT,
    STMT_FOR,
    STMT_GOTO,
    STMT_IF,
    STMT_LABEL,
    STMT_MULTI_DECL,
    STMT_NOP,
    STMT_RETURN,
    STMT_SINGLE_DECL,
    STMT_SWAP,
    STMT_SWITCH,
    STMT_TYPE_DECL,
    STMT_WHILE,

    // STMT_CT_IF,
} StmtKind;

typedef enum : uint8_t
{
    TOKEN_INVALID = 0,

    TOKEN_IDENT,            // Identifier
    TOKEN_BIN_INT_LITERAL,
    TOKEN_OCT_INT_LITERAL,
    TOKEN_DEC_INT_LITERAL,
    TOKEN_HEX_INT_LITERAL,
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
    TOKEN_DOT,              // .
    TOKEN_COMMA,            // ,
    TOKEN_LBRACE,           // {
    TOKEN_LBRACKET,         // [
    TOKEN_LPAREN,           // (
    TOKEN_RPAREN,           // )
    TOKEN_RBRACKET,         // ]
    TOKEN_RBRACE,           // }
    TOKEN_ADD,              // +
    TOKEN_SUB,              // -
    TOKEN_MODULO,           // %
    TOKEN_QUESTION,         // ?
    TOKEN_UNDERSCORE,       // _

    TOKEN_ARROW,            // ->
    TOKEN_LSHR,             // >>
    TOKEN_ASHR,             // >>>
    TOKEN_SHL,              // <<
    TOKEN_LOG_AND,          // &&
    TOKEN_LOG_OR,           // ||
    TOKEN_EQ,               // ==
    TOKEN_NE,               // !=
    TOKEN_LE,               // <=
    TOKEN_GE,               // >=
    TOKEN_NAMESPACE,        // ::
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
    TOKEN_RANGE,            // ..
    TOKEN_ELLIPSIS,         // ...
    
    TOKEN_SWAP,             // <->
    TOKEN_LSHR_ASSIGN,      // >>=
    TOKEN_ASHR_ASSIGN,      // >>>=
    TOKEN_SHL_ASSIGN,       // <<=

    // Keywords
    TOKEN_ALIAS,
    TOKEN_KEYWORD_START = TOKEN_ALIAS,
    TOKEN_AS,
    TOKEN_AUTO,
    TOKEN_BITFIELD,
    TOKEN_BREAK,
    TOKEN_CASE,
    TOKEN_CONST,
    TOKEN_CONTINUE,
    TOKEN_DEFAULT,
    TOKEN_DISTINCT,
    TOKEN_ELSE,
    TOKEN_ENUM,
    TOKEN_EXTERN,
    TOKEN_FALSE,
    TOKEN_FN,
    TOKEN_FOR,
    TOKEN_GOTO,
    TOKEN_IF,
    TOKEN_IMPORT,
    TOKEN_LABEL,
    TOKEN_MODULE,
    TOKEN_NULLPTR,
    TOKEN_PRIV,
    TOKEN_PUB,
    TOKEN_RETURN,
    TOKEN_STRUCT,
    TOKEN_SWITCH,
    TOKEN_TRUE,
    TOKEN_TYPEDEF,
    TOKEN_UNION,
    TOKEN_WHILE,

    // Built-in/Primitive type names (Still part of keywords)
    TOKEN_VOID,
    TOKEN_TYPENAME_START = TOKEN_VOID,
    TOKEN_BOOL,
    TOKEN_CHAR,
    TOKEN_BYTE,
    TOKEN_UBYTE,
    TOKEN_SHORT,
    TOKEN_USHORT,
    TOKEN_INT,
    TOKEN_UINT,
    TOKEN_LONG,
    TOKEN_ULONG,
    TOKEN_FLOAT,
    TOKEN_DOUBLE,

    // These have to be AFTER float and double because I use a trick
    // in type_to_string() to get the names of the primitives.
    TOKEN_IPTR,
    TOKEN_UPTR,
    TOKEN_ISZ,
    TOKEN_USZ,
    TOKEN_TYPENAME_END = TOKEN_USZ,

    // Compile time tokens (start with #)
	TOKEN_CT_ALIGNOF,
	TOKEN_CT_ASSERT,
	TOKEN_CT_ENDIF,
    TOKEN_CT_IF,
	TOKEN_CT_OFFSETOF,
	TOKEN_CT_SIZEOF,
	TOKEN_CT_TYPEOF,
    TOKEN_KEYWORD_END = TOKEN_CT_TYPEOF,

    TOKEN_EOF,               // End Of File Token
    __TOKEN_COUNT
} TokenKind;

typedef enum : uint8_t
{
    TYPE_INVALID = 0,
    TYPE_VOID,
    TYPE_BUILTIN_START  = TYPE_VOID,
    TYPE_BOOL,
    TYPE_NUMERIC_START  = TYPE_BOOL,

    TYPE_CHAR,
    TYPE_INTEGER_START  = TYPE_CHAR,
    TYPE_BYTE,
    TYPE_UBYTE,
    TYPE_SHORT,
    TYPE_USHORT,
    TYPE_INT,
    TYPE_UINT,
    TYPE_LONG,
    TYPE_ULONG,
    TYPE_INTEGER_END    = TYPE_ULONG,

    TYPE_FLOAT,
    TYPE_FLOAT_START    = TYPE_FLOAT,
    TYPE_DOUBLE,
    TYPE_FLOAT_END      = TYPE_DOUBLE,
    TYPE_NUMERIC_END    = TYPE_DOUBLE,
    TYPE_BUILTIN_END    = TYPE_DOUBLE,

    TYPE_POINTER,
    TYPE_FUNC_PTR,
    TYPE_STATIC_ARRAY,
    TYPE_RUNTIME_ARRAY,

    TYPE_ALIAS,
    TYPE_USER_DEF_START = TYPE_ALIAS,
    TYPE_ALIAS_DISTINCT,
    TYPE_ENUM,
    TYPE_ENUM_DISTINCT,
    TYPE_STRUCT,
    TYPE_UNION,
    TYPE_USER_DEF_END   = TYPE_UNION,

    // Pre-semantic types. After analyzing the type these should never appear
    TYPE_ANON_ARRAY, // Anonymous array literals before being casted (i.e. [4, 3])
    TYPE_AUTO,
    TYPE_PS_ARRAY,
    TYPE_PS_USER,
    TYPE_TYPEOF,
    __TYPE_COUNT,
} TypeKind;

typedef enum : uint8_t
{
    UNARY_INVALID = 0,

    UNARY_ADDR_OF,
    UNARY_BIT_NOT,
    UNARY_DEC,
    UNARY_DEREF,
    UNARY_INC,
    UNARY_LOG_NOT,
    UNARY_NEG,
} UnaryOpKind;

typedef enum : uint8_t
{
    VAR_INVALID = 0,
    VAR_GLOBAL,
    VAR_LOCAL,
    VAR_CONST,
    VAR_PARAM,
    VAR_MEMBER,
} VarKind;

#define INT_TYPES             \
            TYPE_BOOL:        \
            case TYPE_CHAR:   \
            case TYPE_BYTE:   \
            case TYPE_UBYTE:  \
            case TYPE_SHORT:  \
            case TYPE_USHORT: \
            case TYPE_INT:    \
            case TYPE_UINT:   \
            case TYPE_LONG:   \
            case TYPE_ULONG
                
#define FLOAT_TYPES           \
            TYPE_FLOAT:       \
            case TYPE_DOUBLE
                
