#include "internal.h"
#include "utils/error.h"
#include "utils/file_utils.h"

#include <string.h>

#define at_eof(lex)         ((lex)->cur_pos[0] == '\0')
#define peek(lex)           ((lex)->cur_pos[0])
#define peek_next(lex)      ((lex)->cur_pos[1])
#define advance(lex, count) ((lex)->cur_pos += count)
#define next(lex)           (++(lex)->cur_pos)
#define nextr(lex)          (*(++(lex)->cur_pos))
#define backtrack(lex)      (--(lex)->cur_pos)

static void        skip_invisible(Lexer* lexer);
static inline bool consume(Lexer* lexer, char c);

// The 'nl' versions of next, backtrack, and consume do an additional check
// for if the character is a newline, if it is, we update the line data accordingly
static inline char   next_nl(Lexer* lexer);
static inline void   backtrack_nl(Lexer* lexer);
static inline bool   consume_nl(Lexer* lexer);
static inline Token* next_token_loc(Lexer* lexer);
static inline void   extract_identifier(Lexer* lexer, Token* t);
static inline void   extract_char_literal(Lexer* lexer, Token* t);
static inline void   extract_string_literal(Lexer* lexer, Token* t);
static inline void   extract_num_literal(Lexer* lexer, Token* t);
static inline bool   extract_num_suffix(Lexer* lexer, bool* is_float);
static inline int    escaped_char(const char** pos, uint64_t* real);

void lexer_init_unit(Lexer* lexer, CompilationUnit* unit)
{
    SIC_ASSERT(lexer != NULL);
    SIC_ASSERT(unit != NULL);
    lexer->unit       = unit;
    lexer->src_start  = sifile_read(&unit->file);
    lexer->cur_pos    = lexer->src_start;
    lexer->cur_line   = 1;
    lexer->line_start = lexer->src_start;
    memset(&lexer->la_buf, 0, sizeof(LookAhead));
    for(size_t i = 0; i < LOOK_AHEAD_SIZE; ++i)
        lexer_advance(lexer);
}

void lexer_set_pos_in_unit(Lexer* lexer, CompilationUnit* unit, SourceLoc* start)
{
    SIC_ASSERT(lexer != NULL);
    SIC_ASSERT(start != NULL);
    lexer->unit       = unit;
    lexer->src_start  = start->start;
    lexer->cur_pos    = start->start;
    lexer->cur_line   = start->line_num;
    lexer->line_start = start->line_start;
    memset(&lexer->la_buf, 0, sizeof(LookAhead));
    for(size_t i = 0; i < LOOK_AHEAD_SIZE; ++i)
        lexer_advance(lexer);
}

void lexer_advance(Lexer* lexer)
{
    SIC_ASSERT(lexer != NULL);

    skip_invisible(lexer);

    Token* t = next_token_loc(lexer);
    if(t->kind == TOKEN_EOF)
        return;

    if(at_eof(lexer))
    {
        t->kind = TOKEN_EOF;
        t->loc.len = 0;
        return;
    }

    t->loc.line_start = lexer->line_start;
    t->loc.line_num   = lexer->cur_line;
    t->loc.start      = lexer->cur_pos;
    t->kind           = TOKEN_INVALID;

    char c = peek(lexer);
    next(lexer);
    t->loc.len = 1;
    switch(c)
    {
    case '~':
        t->kind = TOKEN_BIT_NOT;
        return;
    case ';':
        t->kind = TOKEN_SEMI;
        return;
    case '.':
        t->kind = (consume(lexer, '.') && consume(lexer, '.')) ? 
                    TOKEN_ELLIPSIS : TOKEN_DOT;
        return;
    case ',':
        t->kind = TOKEN_COMMA;
        return;
    case '{':
        t->kind = TOKEN_LBRACE;
        return;
    case '[':
        t->kind = TOKEN_LBRACKET;
        return;
    case '(':
        t->kind = TOKEN_LPAREN;
        return;
    case ')':
        t->kind = TOKEN_RPAREN;
        return;
    case ']':
        t->kind = TOKEN_RBRACKET;
        return;
    case '}':
        t->kind = TOKEN_RBRACE;
        return;
    case '?':
        t->kind = TOKEN_QUESTION;
        return;
    case ':':
        if(consume(lexer, ':'))
            t->kind = TOKEN_SCOPE_RES;
        else
            t->kind = TOKEN_COLON;
        return;
    case '&':
        if(consume(lexer, '&'))
            t->kind = TOKEN_LOG_AND;
        else if(consume(lexer, '='))
            t->kind = TOKEN_BIT_AND_ASSIGN;
        else
            t->kind = TOKEN_AMP;
        return;
    case '*':
        if(consume(lexer, '='))
            t->kind = TOKEN_MUL_ASSIGN;
        else
            t->kind = TOKEN_ASTERISK;
        return;
    case '!':
        if(consume(lexer, '='))
            t->kind = TOKEN_NE;
        else
            t->kind = TOKEN_LOG_NOT;
        return;
    case '|':
        if(consume(lexer, '|'))
            t->kind = TOKEN_LOG_OR;
        else if(consume(lexer, '='))
            t->kind = TOKEN_BIT_OR_ASSIGN;
        else
            t->kind = TOKEN_BIT_OR;
        return;
    case '^':
        if(consume(lexer, '='))
            t->kind = TOKEN_BIT_XOR_ASSIGN;
        else
            t->kind = TOKEN_BIT_XOR;
        return;
    case '=':
        if(consume(lexer, '='))
            t->kind = TOKEN_EQ;
        else
            t->kind = TOKEN_ASSIGN;
        return;
    case '<':
        if(consume(lexer, '<'))
            t->kind = consume(lexer, '=') ? TOKEN_SHL_ASSIGN : TOKEN_SHL;
        else if(consume(lexer, '='))
            t->kind = TOKEN_LE;
        else
            t->kind = TOKEN_LT;
        return;
    case '>':
        if(consume(lexer, '>'))
        {
            if(consume(lexer, '>'))
                t->kind = consume(lexer, '=') ? TOKEN_ASHR_ASSIGN : TOKEN_ASHR;
            else
                t->kind = consume(lexer, '=') ? TOKEN_LSHR_ASSIGN : TOKEN_LSHR;
        }
        else if(consume(lexer, '='))
            t->kind = TOKEN_GE;
        else
            t->kind = TOKEN_GT;
        return;
    case '/':
        if(consume(lexer, '='))
            t->kind = TOKEN_DIV_ASSIGN;
        else
            t->kind = TOKEN_DIV;
        return;
    case '+':
        if(consume(lexer, '='))
            t->kind = TOKEN_ADD_ASSIGN;
        else if(consume(lexer, '+'))
            t->kind = TOKEN_INCREM;
        else
            t->kind = TOKEN_ADD;
        return;
    case '-':
        if(consume(lexer, '='))
            t->kind = TOKEN_SUB_ASSIGN;
        else if(consume(lexer, '>'))
            t->kind = TOKEN_ARROW;
        else if(consume(lexer, '-'))
            t->kind = TOKEN_DECREM;
        else
            t->kind = TOKEN_SUB;
        return;
    case '%':
        if(consume(lexer, '='))
            t->kind = TOKEN_MOD_ASSIGN;
        else
            t->kind = TOKEN_MODULO;
        return;
    case '\'':
        extract_char_literal(lexer, t);
        return;
    case '\"':
        extract_string_literal(lexer, t);
        return;
    case '_':
    CASE_IDENT:
        extract_identifier(lexer, t);
        return;
    default:
        if(c_is_num(c))
        {
            backtrack(lexer);
            extract_num_literal(lexer, t);
            return;
        }
        if(c_is_alpha(c))
            goto CASE_IDENT;

        SIC_ERROR_DBG_ARGS("Encountered unknown character. %d", c);
        return;
    }
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

static inline void extract_identifier(Lexer* lexer, Token* t)
{
    while(c_is_undalphanum(peek(lexer)))
        next(lexer);

    t->loc.len = (uintptr_t)lexer->cur_pos - (uintptr_t)t->loc.start;
    t->kind = sym_map_getn(t->loc.start, t->loc.len);
    if(t->kind == TOKEN_INVALID)
        t->kind = TOKEN_IDENT;
}

static inline void extract_char_literal(Lexer* lexer, Token* t)
{
    t->loc.start++;
    if(peek(lexer) == '\\')
    {
        next(lexer);
        int escape_len = escaped_char(&lexer->cur_pos, &t->chr.val);
        if(escape_len < 0)
        {
            t->loc.len = 2;
            sic_error_at(lexer->unit->file.full_path, &t->loc, 
                         "Invalid escape sequence.");
            return;
        }
        t->chr.width = escape_len;
    }
    else
    {
        t->chr.val = peek(lexer);
        t->chr.width = 1;
        next(lexer);
    }

    if(peek(lexer) != '\'')
    {
        t->loc.start--;
        sic_error_at(lexer->unit->file.full_path, &t->loc,
                     "Multi-character char literal, or just missing \'.");
        return;
    }
    t->loc.len = lexer->cur_pos - t->loc.start;
    t->kind = TOKEN_CHAR_LITERAL;
    next(lexer);
}

static inline void extract_string_literal(Lexer* lexer, Token* t)
{
    char c;
    const char* orig = lexer->cur_pos;
    while((c = peek(lexer)) != '\"')
    {
        if(c == '\\')
            c = *(++lexer->cur_pos);
        if(c == '\n')
        {
            sic_error_at(lexer->unit->file.full_path, &t->loc, 
                         "Encountered newline character while lexing string literal. Did you forget a '\"'?");
            return;
        }
        if(c == '\0')
        {
            sic_error_at(lexer->unit->file.full_path, &t->loc, 
                         "Encountered end of file while lexing string literal. Did you forget a '\"'?");
            return;
        }
        next(lexer);
    }

    t->loc.start++;
    t->loc.len = (uintptr_t)lexer->cur_pos - (uintptr_t)t->loc.start;
    t->kind = TOKEN_STRING_LITERAL;

    char*  real_string = MALLOC((size_t)(lexer->cur_pos - orig + 1));
    size_t len = 0;

    while(orig < lexer->cur_pos)
    {
        SourceLoc escape_loc;
        escape_loc.start = orig;
        escape_loc.len = 2;
        escape_loc.line_start = lexer->line_start;
        escape_loc.line_num = lexer->cur_line;
        c = *orig;
        orig++;
        if(c == '\\')
        {
            uint64_t value;
            int escape_len = escaped_char(&orig, &value);
            if(escape_len < 0)
            {
                sic_error_at(lexer->unit->file.full_path, &escape_loc, 
                             "Invalid escape sequence.");
                next(lexer);
                t->kind = TOKEN_INVALID;
                return;
            }
            memcpy(real_string + len, &value, escape_len);
            len += escape_len;
            continue;
        }
        real_string[len] = c;
        len++;
    }

    t->str.val = real_string;
    t->str.len = len;

    next(lexer);
}

static inline void extract_num_literal(Lexer* lexer, Token* t)
{
    // Scan prefix
    if(peek(lexer) == '0')
    {
        switch(peek_next(lexer))
        {
        case 'x': // Hex literal
            SIC_TODO_MSG("Hex literal.");
        case 'o':
            SIC_TODO_MSG("Octal literal.");
        case 'b':
            SIC_TODO_MSG("Binary literal.");
        }
    }


    while(c_is_num(peek(lexer)))
        next(lexer);
    
    bool is_float = false;

    if(peek(lexer) == '.')
    {
        is_float = true;
        next(lexer);
        while(c_is_num(peek(lexer)))
            next(lexer);
    }

    if(!extract_num_suffix(lexer, &is_float))
    {
        next(lexer);
        t->kind = TOKEN_INVALID;
        t->loc.len = (uintptr_t)lexer->cur_pos - (uintptr_t)t->loc.start;
        sic_error_at(lexer->unit->file.full_path, &t->loc, "Invalid numeric literal.");
        return;
    }
    
    t->kind = is_float ? TOKEN_FLOAT_LITERAL : TOKEN_INT_LITERAL;
    t->loc.len = (uintptr_t)lexer->cur_pos - (uintptr_t)t->loc.start;
}

static inline bool extract_num_suffix(Lexer* lexer, bool* is_float)
{
    (void)is_float;
    // No suffix
    if(!c_is_alpha(peek(lexer)))
        return true;
    SIC_TODO_MSG("Unimplemented numeric suffix.");
}

static inline int escaped_char(const char** pos, uint64_t* real)
{
    char c = **pos;
    (*pos)++;
    // uint64_t char_val = 0;
    switch(c)
    {
    case 'a':
        *real = '\a';
        return 1;
    case 'b':
        *real = '\b';
        return 1;
    case 'e':
        *real = 0x1B;
        return 1; // ANSI Escape starting byte
    case 'f':
        *real = '\f';
        return 1;
    case 'n':
        *real = '\n';
        return 1;
    case 'r':
        *real = '\r';
        return 1;
    case 't':
        *real = '\t';
        return 1;
    case 'u':
    case 'U':
        SIC_TODO_MSG("Unicode escape sequences.");
    case 'v':
        *real = '\v';
        return 1;
    case 'x':
        SIC_TODO_MSG("Hex escape sequences.");
        break;
    case '\"':
        *real = '\"';
        return 1;
    case '\'':
        *real = '\'';
        return 1;
    case '0':
        *real = '\0';
        return 1;
    case '\\':
        *real = '\\';
        return 1;
    default:
        return -1;
    }
}
