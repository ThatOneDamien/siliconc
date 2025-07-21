#include "internal.h"

static const char* s_tok_strs[] = {
    [TOKEN_IDENT]           = "identifier",
    [TOKEN_AMP]             = "&",
    [TOKEN_ASTERISK]        = "*",
    [TOKEN_LOG_NOT]         = "!",
    [TOKEN_BIT_NOT]         = "~",
    [TOKEN_BIT_OR]          = "|",
    [TOKEN_BIT_XOR]         = "^",
    [TOKEN_COLON]           = ":",
    [TOKEN_SEMI]            = ";",
    [TOKEN_ASSIGN]          = "=",
    [TOKEN_LT]              = "<",
    [TOKEN_GT]              = ">",
    [TOKEN_DIV]             = "/",
    [TOKEN_DOT]             = ".",
    [TOKEN_COMMA]           = ",",
    [TOKEN_LBRACE]          = "{",
    [TOKEN_LBRACKET]        = "[",
    [TOKEN_LPAREN]          = "(",
    [TOKEN_RPAREN]          = ")",
    [TOKEN_RBRACKET]        = "]",
    [TOKEN_RBRACE]          = "}",
    [TOKEN_ADD]             = "+",
    [TOKEN_SUB]             = "-",
    [TOKEN_MODULO]          = "%",
    [TOKEN_QUESTION]        = "?",
    [TOKEN_ARROW]           = "->",
    [TOKEN_LSHR]            = ">>",
    [TOKEN_ASHR]            = ">>>",
    [TOKEN_SHL]             = "<<",
    [TOKEN_LOG_AND]         = "&&",
    [TOKEN_LOG_OR]          = "||",
    [TOKEN_EQ]              = "==",
    [TOKEN_NE]              = "!=",
    [TOKEN_LE]              = "<=",
    [TOKEN_GE]              = ">=",
    [TOKEN_SCOPE_RES]       = "::",
    [TOKEN_BIT_AND_ASSIGN]  = "&=",
    [TOKEN_BIT_OR_ASSIGN]   = "|=",
    [TOKEN_BIT_XOR_ASSIGN]  = "^=",
    [TOKEN_ADD_ASSIGN]      = "+=",
    [TOKEN_SUB_ASSIGN]      = "-=",
    [TOKEN_MUL_ASSIGN]      = "*=",
    [TOKEN_DIV_ASSIGN]      = "/=",
    [TOKEN_MOD_ASSIGN]      = "%=",
    [TOKEN_INCREM]          = "++",
    [TOKEN_DECREM]          = "--",
    [TOKEN_ELLIPSIS]        = "...", 
    [TOKEN_LSHR_ASSIGN]     = ">>=",
    [TOKEN_ASHR_ASSIGN]     = ">>>=",
    [TOKEN_SHL_ASSIGN]      = "<<=",
    [TOKEN_AS]              = "as",
    [TOKEN_BITFIELD]        = "bitfield",
    [TOKEN_CONST]           = "const",
    [TOKEN_ELSE]            = "else",
    [TOKEN_ENUM]            = "enum",
    [TOKEN_EXTERN]          = "extern",
    [TOKEN_FALSE]           = "false",
    [TOKEN_IF]              = "if",
    [TOKEN_MODULE]          = "mod",
    [TOKEN_NULLPTR]         = "nullptr",
    [TOKEN_PRIV]            = "priv",
    [TOKEN_PROT]            = "prot",
    [TOKEN_PUB]             = "pub",
    [TOKEN_RETURN]          = "return",
    [TOKEN_STRUCT]          = "struct",
    [TOKEN_TRUE]            = "true",
    [TOKEN_TYPEDEF]         = "typedef",
    [TOKEN_UNION]           = "union",
    [TOKEN_WHILE]           = "while",
    [TOKEN_VOID]            = "void",
    [TOKEN_BOOL]            = "bool",
    [TOKEN_UBYTE]           = "ubyte",
    [TOKEN_BYTE]            = "byte",
    [TOKEN_USHORT]          = "ushort",
    [TOKEN_SHORT]           = "short",
    [TOKEN_UINT]            = "uint",
    [TOKEN_INT]             = "int",
    [TOKEN_ULONG]           = "ulong",
    [TOKEN_LONG]            = "long",
    [TOKEN_FLOAT]           = "float",
    [TOKEN_DOUBLE]          = "double",
};

static BinaryOpKind s_tok_to_bin_op[] = {
    [TOKEN_AMP]             = BINARY_BIT_AND,
    [TOKEN_ASTERISK]        = BINARY_MUL,
    [TOKEN_BIT_OR]          = BINARY_BIT_OR,
    [TOKEN_BIT_XOR]         = BINARY_BIT_XOR,
    [TOKEN_ASSIGN]          = BINARY_ASSIGN,
    [TOKEN_LT]              = BINARY_LT,
    [TOKEN_GT]              = BINARY_GT,
    [TOKEN_DIV]             = BINARY_DIV,
    [TOKEN_ADD]             = BINARY_ADD,
    [TOKEN_SUB]             = BINARY_SUB,
    [TOKEN_MODULO]          = BINARY_MOD,
    [TOKEN_LSHR]            = BINARY_LSHR,
    [TOKEN_ASHR]            = BINARY_ASHR,
    [TOKEN_SHL]             = BINARY_SHL,
    [TOKEN_LOG_AND]         = BINARY_LOG_AND,
    [TOKEN_LOG_OR]          = BINARY_LOG_OR,
    [TOKEN_EQ]              = BINARY_EQ,
    [TOKEN_NE]              = BINARY_NE,
    [TOKEN_LE]              = BINARY_LE,
    [TOKEN_GE]              = BINARY_GE,
    [TOKEN_BIT_AND_ASSIGN]  = BINARY_BIT_AND_ASSIGN,
    [TOKEN_BIT_OR_ASSIGN]   = BINARY_BIT_OR_ASSIGN,
    [TOKEN_BIT_XOR_ASSIGN]  = BINARY_BIT_XOR_ASSIGN,
    [TOKEN_ADD_ASSIGN]      = BINARY_ADD_ASSIGN,
    [TOKEN_SUB_ASSIGN]      = BINARY_SUB_ASSIGN,
    [TOKEN_MUL_ASSIGN]      = BINARY_MUL_ASSIGN,
    [TOKEN_DIV_ASSIGN]      = BINARY_DIV_ASSIGN,
    [TOKEN_MOD_ASSIGN]      = BINARY_MOD_ASSIGN,
    [TOKEN_LSHR_ASSIGN]     = BINARY_LSHR_ASSIGN,
    [TOKEN_ASHR_ASSIGN]     = BINARY_ASHR_ASSIGN,
    [TOKEN_SHL_ASSIGN]      = BINARY_SHL_ASSIGN,
};

static UnaryOpKind s_tok_to_unary_op[] = {
    [TOKEN_AMP]      = UNARY_ADDR_OF,
    [TOKEN_BIT_NOT]  = UNARY_BIT_NOT,
    [TOKEN_DECREM]   = UNARY_DEC,
    [TOKEN_ASTERISK] = UNARY_DEREF,
    [TOKEN_INCREM]   = UNARY_INC,
    [TOKEN_LOG_NOT]  = UNARY_LOG_NOT,
    [TOKEN_SUB]      = UNARY_NEG,
};


const char* tok_kind_to_str(TokenKind kind)
{
    SIC_ASSERT(s_tok_strs[kind] != NULL);
    return s_tok_strs[kind];
}

BinaryOpKind tok_to_binary_op(TokenKind kind)
{
    SIC_ASSERT(kind < sizeof(s_tok_to_bin_op));
    SIC_ASSERT(s_tok_to_bin_op[kind] != BINARY_INVALID);
    return s_tok_to_bin_op[kind];
}

UnaryOpKind tok_to_unary_op(TokenKind kind)
{
    SIC_ASSERT(kind < sizeof(s_tok_to_unary_op));
    SIC_ASSERT(s_tok_to_unary_op[kind] != UNARY_INVALID);
    return s_tok_to_unary_op[kind];
}
