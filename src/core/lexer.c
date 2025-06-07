#include "internal.h"
#include "utils/error.h"
#include "utils/file_utils.h"

#include <string.h>

#define at_eof(lex)    ((lex)->cur_pos[0] == '\0')
#define peek(lex)      ((lex)->cur_pos[0])
#define peek_next(lex) ((lex)->cur_pos[1])
#define next(lex)      (++(lex)->cur_pos)
#define nextr(lex)     (*(++(lex)->cur_pos))
#define backtrack(lex) (--(lex)->cur_pos)

// static Token* create_token(TokenKind type, char* start, size_t len);
// static size_t extract_num_literal(char* source);
// static size_t extract_identifier(char* source);
// static size_t extract_separator(char* source);
static void skip_invisible(Lexer* lexer);

static inline bool   consume(Lexer* lexer, char c);

// The 'nl' versions of next, backtrack, and consume do an additional check
// for if the character is a newline, if it is, we update the line data accordingly
static inline char   next_nl(Lexer* lexer);
static inline void   backtrack_nl(Lexer* lexer);
static inline bool   consume_nl(Lexer* lexer);
static inline Token* next_token_loc(Lexer* lexer);
static inline bool   extract_identifier(Lexer* lexer, Token* t);
static inline bool   extract_num_literal(Lexer* lexer, Token* t);

void lexer_init_file(Lexer* lexer, const char* path)
{
    SIC_ASSERT(lexer != NULL);
    SIC_ASSERT(path != NULL);
    lexer->file_name  = path;
    lexer->src_start  = read_entire_file(path);
    lexer->cur_pos    = lexer->src_start;
    lexer->cur_line   = 1;
    lexer->line_start = lexer->src_start;
    memset(&lexer->la_buf, 0, sizeof(LookAhead));
    for(size_t i = 0; i < LOOK_AHEAD_SIZE; ++i)
        lexer_advance(lexer);
}

bool lexer_advance(Lexer* lexer)
{
    SIC_ASSERT(lexer != NULL);

    skip_invisible(lexer);

    Token* t = next_token_loc(lexer);
    if(t->kind == TOKEN_EOF)
        return false;
    t->line_start = lexer->line_start;
    t->line_num   = lexer->cur_line;
    t->loc        = lexer->cur_pos;
    t->kind       = TOKEN_INVALID;

    if(at_eof(lexer))
    {
        t->kind = TOKEN_EOF;
        t->len = 0;
        return true;
    }

    char c = peek(lexer);
    next(lexer);
    t->len = 1;
    switch(c)
    {
    case '~':
        t->kind = TOKEN_BIT_NOT;
        return true;
    case ':':
        t->kind = TOKEN_COLON;
        return true;
    case ';':
        t->kind = TOKEN_SEMI;
        return true;
    case '.':
        t->kind = TOKEN_PERIOD;
        return true;
    case ',':
        t->kind = TOKEN_COMMA;
        return true;
    case '{':
        t->kind = TOKEN_LBRACE;
        return true;
    case '[':
        t->kind = TOKEN_LBRACKET;
        return true;
    case '(':
        t->kind = TOKEN_LPAREN;
        return true;
    case ')':
        t->kind = TOKEN_RPAREN;
        return true;
    case ']':
        t->kind = TOKEN_RBRACKET;
        return true;
    case '}':
        t->kind = TOKEN_RBRACE;
        return true;
    case '?':
        t->kind = TOKEN_QUESTION;
        return true;
    case '&':
        if(consume(lexer, '&'))
            t->kind = TOKEN_LOG_AND;
        else if(consume(lexer, '='))
            t->kind = TOKEN_BIT_AND_ASSIGN;
        else
            t->kind = TOKEN_AMP;
        return true;
    case '*':
        if(consume(lexer, '='))
            t->kind = TOKEN_MUL_ASSIGN;
        else
            t->kind = TOKEN_ASTERISK;
        return true;
    case '!':
        if(consume(lexer, '='))
            t->kind = TOKEN_LOG_NOT_EQUIV;
        else
            t->kind = TOKEN_LOG_NOT;
        return true;
    case '|':
        if(consume(lexer, '|'))
            t->kind = TOKEN_LOG_OR;
        else if(consume(lexer, '='))
            t->kind = TOKEN_BIT_OR_ASSIGN;
        else
            t->kind = TOKEN_BIT_OR;
        return true;
    case '^':
        if(consume(lexer, '='))
            t->kind = TOKEN_BIT_XOR_ASSIGN;
        else
            t->kind = TOKEN_BIT_XOR;
        return true;
    case '=':
        if(consume(lexer, '='))
            t->kind = TOKEN_LOG_EQUIV;
        else
            t->kind = TOKEN_ASSIGN;
        return true;
    case '<':
        if(consume(lexer, '<'))
            t->kind = consume(lexer, '=') ? TOKEN_SHL_ASSIGN : TOKEN_SHL;
        else if(consume(lexer, '='))
            t->kind = TOKEN_LE;
        else
            t->kind = TOKEN_LT;
        return true;
    case '>':
        if(consume(lexer, '>'))
            t->kind = consume(lexer, '=') ? TOKEN_SHR_ASSIGN : TOKEN_SHR;
        else if(consume(lexer, '='))
            t->kind = TOKEN_GE;
        else
            t->kind = TOKEN_GT;
        return true;
    case '/':
        if(consume(lexer, '='))
            t->kind = TOKEN_DIV_ASSIGN;
        else
            t->kind = TOKEN_DIV;
        return true;
    case '+':
        if(consume(lexer, '='))
            t->kind = TOKEN_ADD_ASSIGN;
        else if(consume(lexer, '+'))
            t->kind = TOKEN_INCREM;
        else
            t->kind = TOKEN_ADD;
        return true;
    case '-':
        if(consume(lexer, '='))
            t->kind = TOKEN_SUB_ASSIGN;
        else if(consume(lexer, '-'))
            t->kind = TOKEN_DECREM;
        else
            t->kind = TOKEN_SUB;
        return true;
    case '%':
        if(consume(lexer, '='))
            t->kind = TOKEN_MOD_ASSIGN;
        else
            t->kind = TOKEN_MOD;
        return true;
    case '\'':
        SIC_ERROR_DBG("Unimplemented char literal.");
        return false;
    case '\"':
        SIC_ERROR_DBG("Unimplemented string literal.");
        return false;
    case '_':
    CASE_IDENT:
        return extract_identifier(lexer, t);
    default:
        if(c_is_num(c))
        {
            backtrack(lexer);
            return extract_num_literal(lexer, t);
        }
        if(c_is_alpha(c))
            goto CASE_IDENT;

        SIC_ERROR_DBG_ARGS("Encountered unknown character. %d", c);
        return false;
    }
}

Token* lexer_look_ahead(Lexer* lexer, uint32_t count)
{
    SIC_ASSERT(lexer != NULL);
    SIC_ASSERT(count < LOOK_AHEAD_SIZE);
    return lexer->la_buf.buf + ((lexer->la_buf.head + count) % LOOK_AHEAD_SIZE);
}

static void skip_invisible(Lexer* lexer)
{
    while(true)
    {
        switch(peek(lexer))
        {
        case ' ':
        case '\t':
            next(lexer);
            continue;
        case '\n':
            next(lexer);
            lexer->line_start = lexer->cur_pos;
            lexer->cur_line++;
            continue;
        case '/':
            if(peek_next(lexer) == '/')
            {
                lexer->cur_pos += 2;
                while(peek(lexer) != '\n' && !at_eof(lexer))
                    next(lexer);
                continue;
            }
            if(peek_next(lexer) == '*')
            {
                lexer->cur_pos += 2;
                while(true)
                {
                    if(at_eof(lexer))
                        return;
                    if(peek(lexer) == '*' && peek_next(lexer) == '/')
                    {
                        lexer->cur_pos += 2;
                        break;
                    }
                    next_nl(lexer);
                }
                continue;
            }
            return;
        default:
            return;
        }
    }
}

static inline bool consume(Lexer* lexer, char c)
{
    if(peek(lexer) == c)
    {
        next(lexer);
        return true;
    }
    return false;
}

static inline char next_nl(Lexer* lexer)
{
    if(peek(lexer) == '\n')
    {
        lexer->cur_line++;
        lexer->line_start = lexer->cur_pos + 1;
    }
    return nextr(lexer);
}

static inline void UNUSED backtrack_nl(Lexer* lexer)
{
    if(*(--lexer->cur_pos) == '\n')
        lexer->cur_line--;
}

static inline bool UNUSED consume_nl(Lexer* lexer)
{
    if(peek(lexer) == '\n')
    {
        lexer->cur_line++;
        lexer->line_start = next(lexer);
        return true;
    }
    return false;
}

static inline Token* next_token_loc(Lexer* lexer)
{
    Token* res = lexer->la_buf.buf + lexer->la_buf.head;
    lexer->la_buf.head = (lexer->la_buf.head + 1) % LOOK_AHEAD_SIZE;
    return res;
}

static inline bool extract_identifier(Lexer* lexer, Token* t)
{
    while(c_is_undalphanum(peek(lexer)))
        next(lexer);

    t->len = (uintptr_t)lexer->cur_pos - (uintptr_t)t->loc;
    t->kind = sym_map_getn(t->loc, t->len);
    if(t->kind == TOKEN_INVALID)
        t->kind = TOKEN_IDENT;
    return true;
}

static inline bool extract_num_literal(Lexer* lexer, Token* t)
{
    // Scan prefix
    if(peek(lexer) == '0')
    {
        switch(peek_next(lexer))
        {
        case 'x': // Hex literal
            SIC_ERROR_DBG("Unimplemented hex literal.");
            return false;
        case 'o':
            SIC_ERROR_DBG("Unimplemented octal literal.");
            return false;
        case 'b':
            SIC_ERROR_DBG("Unimplemented binary literal.");
            return false;
        }
    }

    while(c_is_num(peek(lexer)))
        next(lexer);
    
    t->kind = TOKEN_NUM;
    t->len = (uintptr_t)lexer->cur_pos - (uintptr_t)t->loc;
    return true;
}
