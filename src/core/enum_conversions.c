#include "internal.h"

static const char* s_tok_strs[] = {
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
    [TOKEN_PERIOD]          = ".",
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
    [TOKEN_SHR]             = ">>",
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
    [TOKEN_SHR_ASSIGN]      = ">>=",
    [TOKEN_SHL_ASSIGN]      = "<<=",
    [TOKEN_INCREM]          = "++",
    [TOKEN_DECREM]          = "--",
    [TOKEN_AS]              = "as",
    [TOKEN_CONST]           = "const",
    [TOKEN_EXTERN]          = "extern",
    [TOKEN_PRIV]            = "priv",
    [TOKEN_PROT]            = "prot",
    [TOKEN_PUB]             = "pub",
    [TOKEN_RETURN]          = "return",
    [TOKEN_MODULE]          = "mod",
    [TOKEN_VOID]            = "void",
    [TOKEN_U8]              = "u8",
    [TOKEN_S8]              = "s8",
    [TOKEN_U16]             = "u16",
    [TOKEN_S16]             = "s16",
    [TOKEN_U32]             = "u32",
    [TOKEN_S32]             = "s32",
    [TOKEN_U64]             = "u64",
    [TOKEN_S64]             = "s64",
    [TOKEN_F32]             = "f32",
    [TOKEN_F64]             = "f64",
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
    [TOKEN_SHR]             = BINARY_SHR,
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
    [TOKEN_SHR_ASSIGN]      = BINARY_SHR_ASSIGN,
    [TOKEN_SHL_ASSIGN]      = BINARY_SHL_ASSIGN,
};

static UnaryOpKind s_tok_to_unary_op[] = {
    [TOKEN_AMP]      = UNARY_ADDR_OF,
    [TOKEN_ASTERISK] = UNARY_DEREF,
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
