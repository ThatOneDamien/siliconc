#include "lexer.h"
#include "core.h"
#include "file_utils.h"

#include <ctype.h>

static Token* create_token(TokenType type, char* start, size_t len);
static size_t extract_num_literal(char* source);
static size_t extract_str_literal(char* source);

Token* lex_file(const char* path)
{
    char* src = read_entire_file(path);
    return src == NULL ? NULL : lex_source(src);
}

Token* lex_source(char* source)
{
    SIC_ASSERT(source != NULL);

    Token sentinel;
    Token* cur = &sentinel;

    while(*source)
    {
        size_t len;
        if(*source == '\n' || isspace(*source))
            source++;
        else if((len = extract_num_literal(source)) > 0)
        {
            cur->next = create_token(TOKEN_NUM, source, len);
            cur = cur->next;
            source += len;
        }
        else if((len = extract_str_literal(source)) > 0)
        {
            cur->next = create_token(TOKEN_STR, source, len);
            cur = cur->next;
            source += len;
        }
        else
            source++;
    }

    cur->next = create_token(TOKEN_EOF, source, 0);
    return sentinel.next;
}

static Token* create_token(TokenType type, char* start, size_t len)
{
    Token* res = malloc(sizeof(Token));
    if(res == NULL)
        return NULL;
    res->type = type;
    res->next = NULL;
    res->ref  = start;
    res->len  = len;
    return res;
}

static size_t extract_num_literal(char* source)
{
    size_t count = 0;
    while(isdigit(*(source++)))
        count++;

    return count;
}

static size_t extract_str_literal(char* source)
{
    if(*source != '\"')
        return 0;
    char* orig = source;
    printf("%c\n", *source);
    source++;
    while(*source != '\"')
    {
        if(*source == '\0' || *source == '\n')
            SIC_ERROR_DBG("Bad syntax");
        if(*source == '\\')
            source++;
        source++;
    }
    return source - orig + 1;
}
