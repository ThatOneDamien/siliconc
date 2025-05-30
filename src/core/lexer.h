#pragma once
#include "type.h"

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

typedef enum
{
    TOKEN_NONE = 0,
    TOKEN_IDNT,
    TOKEN_SEP,
    TOKEN_KEYWORD,
    TOKEN_STR,
    TOKEN_NUM,
    TOKEN_EOF
} TokenType;

typedef struct Token Token;
struct Token
{
    TokenType type;
    Token*    next;
    char*     ref;
    size_t    len;
    Type*     data_type;

    union
    {
        uint64_t i;
        double   f;
        char*    s;
    } val;
};

Token* lex_file(const char* path);
Token* lex_source(char* source, const char* path);
bool   tok_equal(Token* token, const char* str);
