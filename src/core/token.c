#include "internal.h"

static const char* s_tok_strs[] = {
    [TOKEN_INVALID]         = NULL,
    [TOKEN_IDENT]           = NULL,
    [TOKEN_STR]             = NULL,
    [TOKEN_NUM]             = NULL,
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
    [TOKEN_MOD]             = "%",
    [TOKEN_QUESTION]        = "?",
    [TOKEN_SHR]             = ">>",
    [TOKEN_SHL]             = "<<",
    [TOKEN_LOG_AND]         = "&&",
    [TOKEN_LOG_OR]          = "||",
    [TOKEN_LOG_EQUIV]       = "==",
    [TOKEN_LOG_NOT_EQUIV]   = "!=",
    [TOKEN_LE]              = "<=",
    [TOKEN_GE]              = ">=",
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
    [TOKEN_RETURN]          = "return",
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
    [TOKEN_EOF]             = NULL,
};

const char* tok_kind_to_str(TokenKind kind)
{
    return s_tok_strs[kind];
}
