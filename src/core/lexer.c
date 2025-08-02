#include "internal.h"
#include "utils/file_utils.h"

#include <string.h>

#define at_eof(lex)         ((lex)->cur_pos[0] == '\0')
#define peek(lex)           ((lex)->cur_pos[0])
#define peek_next(lex)      ((lex)->cur_pos[1])
#define advance(lex, count) ((lex)->cur_pos += count)
#define next(lex)           (++(lex)->cur_pos)
#define nextr(lex)          (*(++(lex)->cur_pos))
#define backtrack(lex)      (--(lex)->cur_pos)

static void        skip_invisible(Lexer* l);
static inline bool consume(Lexer* l, char c);

static inline char     next_nl(Lexer* l);
static inline Token*   next_token_loc(Lexer* l);
static inline void     extract_identifier(Lexer* l, Token* t);
static inline void     extract_char_literal(Lexer* l, Token* t);
static inline void     extract_string_literal(Lexer* l, Token* t);
static inline void     extract_num_literal(Lexer* l, Token* t);
static inline bool     extract_num_suffix(Lexer* l, bool* is_float);
static inline int      escaped_char(const char** pos, uint64_t* real);
static inline uint32_t get_col(Lexer* l);

void lexer_init_unit(Lexer* l, CompilationUnit* unit)
{
    SIC_ASSERT(l != NULL);
    SIC_ASSERT(unit != NULL);
    InputFile* file = file_from_id(unit->file);
    input_file_read(file);
    l->unit       = unit;
    l->cur_pos    = file->src;
    l->cur_line   = 1;
    l->line_start = l->cur_pos;
    memset(&l->la_buf, 0, sizeof(LookAhead));
    l->la_buf.cur = 1;
    for(size_t i = 0; i < LOOK_AHEAD_SIZE - 1; ++i)
    {
        lexer_advance(l);
        l->la_buf.buf[i].loc.file = unit->file;
    }
}

void lexer_advance(Lexer* l)
{
    SIC_ASSERT(l != NULL);

    skip_invisible(l);

    Token* t = next_token_loc(l);
    if(t->kind == TOKEN_EOF)
        return;

    if(at_eof(l))
    {
        t->kind = TOKEN_EOF;
        t->loc.len = 0;
        return;
    }

    t->loc.col_num  = get_col(l);
    t->loc.line_num = l->cur_line;
    t->kind         = TOKEN_INVALID;
    t->start        = l->cur_pos;

    char c = peek(l);
    next(l);
    t->loc.len = 0;
    switch(c)
    {
    case '~':
        t->kind = TOKEN_BIT_NOT;
        break;
    case ';':
        t->kind = TOKEN_SEMI;
        break;
    case ',':
        t->kind = TOKEN_COMMA;
        break;
    case '{':
        t->kind = TOKEN_LBRACE;
        break;
    case '[':
        t->kind = TOKEN_LBRACKET;
        break;
    case '(':
        t->kind = TOKEN_LPAREN;
        break;
    case ')':
        t->kind = TOKEN_RPAREN;
        break;
    case ']':
        t->kind = TOKEN_RBRACKET;
        break;
    case '}':
        t->kind = TOKEN_RBRACE;
        break;
    case '?':
        t->kind = TOKEN_QUESTION;
        break;
    case '.':
        t->kind = (consume(l, '.') && consume(l, '.')) ? 
                    TOKEN_ELLIPSIS : TOKEN_DOT;
        break;
    case ':':
        t->kind = consume(l, ':') ? TOKEN_SCOPE_RES : TOKEN_COLON;
        break;
    case '*':
        t->kind = consume(l, '=') ? TOKEN_MUL_ASSIGN : TOKEN_ASTERISK;
        break;
    case '!':
        t->kind = consume(l, '=') ? TOKEN_NE : TOKEN_LOG_NOT;
        break;
    case '^':
        t->kind = consume(l, '=') ? TOKEN_BIT_XOR_ASSIGN : TOKEN_BIT_XOR;
        break;
    case '=':
        t->kind = consume(l, '=') ? TOKEN_EQ : TOKEN_ASSIGN;
        break;
    case '%':
        t->kind = consume(l, '=') ? TOKEN_MOD_ASSIGN : TOKEN_MODULO;
        break;
    case '/':
        if(consume(l, '='))
            t->kind = TOKEN_DIV_ASSIGN;
        else
            t->kind = TOKEN_DIV;
        break;
    case '&':
        if(consume(l, '&'))
            t->kind = TOKEN_LOG_AND;
        else if(consume(l, '='))
            t->kind = TOKEN_BIT_AND_ASSIGN;
        else
            t->kind = TOKEN_AMP;
        break;
    case '|':
        if(consume(l, '|'))
            t->kind = TOKEN_LOG_OR;
        else if(consume(l, '='))
            t->kind = TOKEN_BIT_OR_ASSIGN;
        else
            t->kind = TOKEN_BIT_OR;
        break;
    case '<':
        if(consume(l, '<'))
            t->kind = consume(l, '=') ? TOKEN_SHL_ASSIGN : TOKEN_SHL;
        else if(consume(l, '='))
            t->kind = TOKEN_LE;
        else if(peek(l) == '-' && peek_next(l) == '>')
        {
            l->cur_pos += 2;
            t->kind = TOKEN_SWAP;
        }
        else
            t->kind = TOKEN_LT;
        break;
    case '>':
        if(consume(l, '>'))
        {
            if(consume(l, '>'))
                t->kind = consume(l, '=') ? TOKEN_ASHR_ASSIGN : TOKEN_ASHR;
            else
                t->kind = consume(l, '=') ? TOKEN_LSHR_ASSIGN : TOKEN_LSHR;
        }
        else if(consume(l, '='))
            t->kind = TOKEN_GE;
        else
            t->kind = TOKEN_GT;
        break;
    case '+':
        if(consume(l, '='))
            t->kind = TOKEN_ADD_ASSIGN;
        else if(consume(l, '+'))
            t->kind = TOKEN_INCREM;
        else
            t->kind = TOKEN_ADD;
        break;
    case '-':
        if(consume(l, '='))
            t->kind = TOKEN_SUB_ASSIGN;
        else if(consume(l, '>'))
            t->kind = TOKEN_ARROW;
        else if(consume(l, '-'))
            t->kind = TOKEN_DECREM;
        else
            t->kind = TOKEN_SUB;
        break;
    case '\'':
        extract_char_literal(l, t);
        return;
    case '\"':
        extract_string_literal(l, t);
        break;
    case '_':
    CASE_IDENT:
        extract_identifier(l, t);
        return;
    default:
        if(c_is_num(c))
        {
            backtrack(l);
            extract_num_literal(l, t);
            break;
        }
        if(c_is_alpha(c))
            goto CASE_IDENT;

        t->loc.len = 1;
        sic_error_at(t->loc, "Unknown character.");
        return;
    }
    t->loc.len = get_col(l) - t->loc.col_num;
}

static void skip_invisible(Lexer* l)
{
    while(true)
    {
        switch(peek(l))
        {
        case ' ':
        case '\t':
            next(l);
            continue;
        case '\n':
            next(l);
            l->line_start = l->cur_pos;
            l->cur_line++;
            continue;
        case '/':
            if(peek_next(l) == '/')
            {
                l->cur_pos += 2;
                while(peek(l) != '\n' && !at_eof(l))
                    next(l);
                continue;
            }
            if(peek_next(l) == '*')
            {
                l->cur_pos += 2;
                while(true)
                {
                    if(at_eof(l))
                        return;
                    if(peek(l) == '*' && peek_next(l) == '/')
                    {
                        l->cur_pos += 2;
                        break;
                    }
                    next_nl(l);
                }
                continue;
            }
            return;
        default:
            return;
        }
    }
}

static inline bool consume(Lexer* l, char c)
{
    if(peek(l) == c)
    {
        next(l);
        return true;
    }
    return false;
}

static inline char next_nl(Lexer* l)
{
    if(peek(l) == '\n')
    {
        l->cur_line++;
        l->line_start = l->cur_pos + 1;
    }
    return nextr(l);
}

static inline Token* next_token_loc(Lexer* l)
{
    Token* res = l->la_buf.buf + l->la_buf.head;
    l->la_buf.head = l->la_buf.cur;
    l->la_buf.cur = (l->la_buf.cur + 1) & LOOK_AHEAD_MASK;
    return res;
}

static inline void extract_identifier(Lexer* l, Token* t)
{
    while(c_is_undalphanum(peek(l)))
        next(l);

    t->loc.len = get_col(l) - t->loc.col_num;
    t->kind = TOKEN_IDENT;
    t->sym = sym_map_addn(t->start, t->loc.len, &t->kind);
}

static inline void extract_char_literal(Lexer* l, Token* t)
{
    if(peek(l) == '\\')
    {
        next(l);
        int escape_len = escaped_char(&l->cur_pos, &t->chr.val);
        if(escape_len < 0)
        {
            t->loc.len = 2;
            sic_error_at(t->loc, "Invalid escape sequence.");
            return;
        }
        t->chr.width = escape_len;
    }
    else
    {
        t->chr.val = peek(l);
        t->chr.width = 1;
        next(l);
    }

    if(peek(l) != '\'')
    {
        t->loc.len = 1;
        sic_error_at(t->loc, "Multi-character char literal, or just missing \'.");
        return;
    }
    t->kind = TOKEN_CHAR_LITERAL;
    next(l);
}

static inline void extract_string_literal(Lexer* l, Token* t)
{
    char c;
    const char* orig = l->cur_pos;
    while((c = peek(l)) != '\"')
    {
        if(c == '\\')
            c = *(++l->cur_pos);
        if(c == '\n')
        {
            sic_error_at(t->loc, "Encountered newline character while lexing "
                                 "string literal. Did you forget a '\"'?");
            return;
        }
        if(c == '\0')
        {
            sic_error_at(t->loc, "Encountered end of file while lexing "
                                 "string literal. Did you forget a '\"'?");
            return;
        }
        next(l);
    }

    t->kind = TOKEN_STRING_LITERAL;

    char*  real_string = MALLOC((size_t)(l->cur_pos - orig + 1), 1);
    size_t len = 0;
    SourceLoc escape_loc;
    escape_loc.file = t->loc.file;
    escape_loc.len = 2;
    escape_loc.col_num = t->loc.col_num + 1;
    escape_loc.line_num = t->loc.line_num;

    while(orig < l->cur_pos)
    {
        c = *orig;
        orig++;
        if(c == '\\')
        {
            uint64_t value;
            int escape_len = escaped_char(&orig, &value);
            if(escape_len < 0)
            {
                sic_error_at(escape_loc, "Invalid escape sequence.");
                next(l);
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

    next(l);
}

static inline void extract_num_literal(Lexer* l, Token* t)
{
    const char* err_str = "Invalid underscore placement.";
    // Scan prefix
    if(peek(l) == '0')
    {
        switch(peek_next(l))
        {
        case 'x': // Hex literal
            SIC_TODO_MSG("Hex literal.");
        case 'o':
            SIC_TODO_MSG("Octal literal.");
        case 'b':
            SIC_TODO_MSG("Binary literal.");
        }
        if(peek(l) == '_')
        {
            next(l);
            goto ERR;
        }
    }

    while(c_is_undnum(peek(l)))
        next(l);
    
    bool is_float = false;

    if(peek(l) == '.')
    {
        if(l->cur_pos[-1] == '_' || peek_next(l) == '_')
            goto ERR;
        is_float = true;
        next(l);
        while(c_is_undnum(peek(l)))
            next(l);
    }

    if(l->cur_pos[-1] == '_')
        goto ERR;

    if(!extract_num_suffix(l, &is_float))
    {
        next(l);
        err_str = "Invalid numeric literal suffix.";
        goto ERR;
    }
    
    t->kind = is_float ? TOKEN_FLOAT_LITERAL : TOKEN_INT_LITERAL;
    return;
ERR:
    t->kind = TOKEN_INVALID;
    sic_error_at(t->loc, "%s", err_str);
}

static inline bool extract_num_suffix(Lexer* l, bool* is_float)
{
    switch(peek(l))
    {
    case 'u':
    case 'U':
    case 'i':
    case 'I':
        SIC_TODO_MSG("Integer literal suffixes.");
    case 'f':
    case 'F':
        *is_float = true;
        // TODO: Add sizes like f32 and f64
        next(l);
        return true;
    default:
        return true;
    }
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

static inline uint32_t get_col(Lexer* l)
{
    return (uintptr_t)l->cur_pos - (uintptr_t)l->line_start + 1;
}
