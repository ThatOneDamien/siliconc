#include "lexer.h"
#include "core.h"
#include "utils/error.h"
#include "utils/file_utils.h"

#include <ctype.h>
#include <string.h>

static Token* create_token(TokenType type, char* start, size_t len);
static size_t extract_num_literal(char* source);
static size_t extract_identifier(char* source);
static size_t extract_separator(char* source);

static inline void lexing_error(char* location, const char* message);

static const char* s_CurrentPath;
static const char* s_CurrentSource;
static size_t      s_CurrentLine;



Token* lex_file(const char* path)
{
    SIC_ASSERT(path != NULL);
    char* src = read_entire_file(path);
    if(src == NULL)
        sic_error_fatal("File '%s' was unable to be opened.", path);
    return lex_source(src, path);
}

Token* lex_source(char* source, const char* path)
{
    SIC_ASSERT(source != NULL);

    Token head;
    Token* cur = &head;
    s_CurrentPath = path;
    s_CurrentSource = source;
    s_CurrentLine = 1;

    while(*source)
    {
        size_t len = 0;
        TokenType type;
        if(*source == '\n')
        {
            s_CurrentLine++;
            source++;
            continue;
        }
        else if(isspace(*source))
        {
            source++;
            continue;
        }
        else if((len = extract_num_literal(source)) > 0)
            type = TOKEN_NUM;
        else if((len = extract_identifier(source)) > 0)
            type = TOKEN_IDNT;
        else if((len = extract_separator(source)) > 0)
            type = TOKEN_SEP;
        else
        {
            lexing_error(source, "Unknown token.");
            exit(EXIT_FAILURE);
        }

        cur->next = create_token(type, source, len);
        cur = cur->next;
        source += len;
    }

    cur->next = create_token(TOKEN_EOF, source, 0);
    return head.next;
}

bool tok_equal(Token* token, const char* str)
{
    SIC_ASSERT(token != NULL);
    SIC_ASSERT(str != NULL);
    return token->type != TOKEN_EOF && memcmp(token->ref, str, token->len) == 0;
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

static size_t extract_identifier(char* source)
{
    char* p = source;
    if(*p != '_' && !isalpha(*p))
        return 0;

    while(true)
    {
        if(*p != '_' && !isalnum(*p))
            return p - source;
        p++;
    }
}

static size_t extract_separator(char* source)
{
    static const char* seps[] = {
        "==", "!=", ">=", "<=", "&&", "||",
        "+=", "-=", "*=", "/=", "%=", "&=", "|=", "^=", "<<=", ">>=", "++", "--",
        ">>", "<<"
    };
    for(size_t i = 0; i < sizeof(seps) / sizeof(seps[0]); ++i)
    {
        size_t len = strlen(seps[i]);
        if(strncmp(source, seps[i], len) == 0)
            return len;
    }
    return ispunct(*source) ? 1 : 0;
}

static inline void lexing_error(char* location, const char* message)
{
    sic_error(s_CurrentPath, s_CurrentLine, s_CurrentSource, location, message);
}

