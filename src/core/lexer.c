#include "internal.h"
#include "utils/file_utils.h"

#include <string.h>

#define at_eof(lex)         ((lex)->cur_pos[0] == '\0')
#define peek(lex)           ((lex)->cur_pos[0])
#define peek_next(lex)      ((lex)->cur_pos[1])
#define peek_prev(lex)      ((lex)->cur_pos[-1])
#define advance(lex, count) ((lex)->cur_pos += count)
#define next(lex)           (++(lex)->cur_pos)
#define nextr(lex)          (*(++(lex)->cur_pos))
#define backtrack(lex)      (--(lex)->cur_pos)

static inline void     skip_invisible(Lexer* l);
static inline void     extract_identifier(Lexer* l, Token* t);
static inline void     extract_char_literal(Lexer* l, Token* t);
static inline void     extract_string_literal(Lexer* l, Token* t);
static inline void     extract_base_2(Lexer* l, Token* t);
static inline void     extract_base_8(Lexer* l, Token* t);
static inline void     extract_base_10(Lexer* l, Token* t);
static inline void     extract_base_16(Lexer* l, Token* t);
static inline bool     extract_num_suffix(Lexer* l, bool* is_float);
static inline int      escaped_char(const char** pos, uint64_t* real);
static inline bool     consume(Lexer* l, char c);
static inline char     next_nl(Lexer* l);
static inline Token*   next_token_loc(Lexer* l);
static inline uint32_t get_col(Lexer* l);
static inline bool     c_is_hex(char c);

PRINTF_FMT(3, 4)
static inline void lexer_error_at_current(Lexer* l, Token* t, const char* msg, ...);
PRINTF_FMT(3, 4)
static inline void lexer_error(Lexer* l, Token* t, const char* msg, ...);

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
    l->la_buf.buf[LOOK_AHEAD_SIZE - 1].loc.file = unit->file;
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
            advance(l, 2);
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
    case '0':
        switch(peek(l))
        {
        case 'b':
            next(l);
            extract_base_2(l, t);
            break;
        case 'o':
            next(l);
            extract_base_8(l, t);
            break;
        default:
            extract_base_10(l, t);
            break;
        case 'x':
            next(l);
            extract_base_16(l, t);
            break;
        }
        break;
    case '1':
    case '2':
    case '3':
    case '4':
    case '5':
    case '6':
    case '7':
    case '8':
    case '9':
        extract_base_10(l, t);
        break;
    case 'A':
    case 'B':
    case 'C':
    case 'D':
    case 'E':
    case 'F':
    case 'G':
    case 'H':
    case 'I':
    case 'J':
    case 'K':
    case 'L':
    case 'M':
    case 'N':
    case 'O':
    case 'P':
    case 'Q':
    case 'R':
    case 'S':
    case 'T':
    case 'U':
    case 'V':
    case 'W':
    case 'X':
    case 'Y':
    case 'Z':
    case 'a':
    case 'b':
    case 'c':
    case 'd':
    case 'e':
    case 'f':
    case 'g':
    case 'h':
    case 'i':
    case 'j':
    case 'k':
    case 'l':
    case 'm':
    case 'n':
    case 'o':
    case 'p':
    case 'q':
    case 'r':
    case 's':
    case 't':
    case 'u':
    case 'v':
    case 'w':
    case 'x':
    case 'y':
    case 'z':
    case '_':
        extract_identifier(l, t);
        return;
    default:
        t->loc.len = 1;
        sic_error_at(t->loc, "Unknown character.");
        t->kind = TOKEN_INVALID;
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
                advance(l, 2);
                while(peek(l) != '\n' && !at_eof(l))
                    next(l);
                continue;
            }
            if(peek_next(l) == '*')
            {
                advance(l, 2);
                while(true)
                {
                    if(at_eof(l))
                        return;
                    if(peek(l) == '*' && peek_next(l) == '/')
                    {
                        advance(l, 2);
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
            lexer_error_at_current(l, t, "Invalid escape sequence.");
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
        lexer_error_at_current(l, t, "Multi-character char literal, or just missing \'.");
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
            lexer_error(l, t, "Encountered newline character while "
                              "lexing literal. Did you forget a '\"'?");
            return;
        }
        if(c == '\0')
        {
            lexer_error(l, t, "Encountered end of file while lexing "
                              "string literal. Did you forget a '\"'?");
            return;
        }
        next(l);
    }

    t->kind = TOKEN_STRING_LITERAL;

    char*  real_string = MALLOC((size_t)(l->cur_pos - orig + 1), sizeof(char));
    size_t len = 0;
    SourceLoc escape_loc;
    escape_loc.file = t->loc.file;
    escape_loc.len = 2;
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
                escape_loc.col_num = (uintptr_t)orig - (uintptr_t)l->line_start + 1;
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

static inline void extract_base_2(Lexer* l, Token* t)
{
    if(!c_is_binary(peek(l)))
    {
        lexer_error_at_current(l, t, "0b should be followed by at least 1 binary digit (0 or 1).");
        return;
    }
    next(l);
    while(c_is_binary(peek(l)) || peek(l) == '_')
        next(l);

    if(c_is_num(peek(l)))
    {
        lexer_error_at_current(l, t, "0b should be followed by only binary digits (0 or 1).");
        return;
    }

    if(peek_prev(l) == '_')
    {
        backtrack(l);
        lexer_error_at_current(l, t, "Numeric literal cannot finish with an underscore.");
        return;
    }

    bool is_float = peek(l) == '.';
    if(!extract_num_suffix(l, &is_float))
        return;
    if(is_float)
    {
        lexer_error(l, t, "Binary literals cannot have a fractional component or float suffix.");
        return;
    }
    t->kind = TOKEN_BIN_INT_LITERAL;
}

static inline void extract_base_8(Lexer* l, Token* t)
{
    if(!c_is_octal(peek(l)))
    {
        lexer_error_at_current(l, t, "0o should be followed by at least 1 octal digit (0-7).");
        return;
    }
    next(l);
    while(c_is_octal(peek(l)) || peek(l) == '_')
        next(l);

    if(c_is_num(peek(l)))
    {
        lexer_error_at_current(l, t, "0o should be followed by only octal digits (0-7).");
        return;
    }

    if(peek_prev(l) == '_')
    {
        backtrack(l);
        lexer_error_at_current(l, t, "Numeric literal cannot finish with an underscore.");
        return;
    }

    bool is_float = peek(l) == '.';
    if(!extract_num_suffix(l, &is_float))
        return;
    if(is_float)
    {
        lexer_error(l, t, "Octal literals cannot have a fractional component or float suffix.");
        return;
    }
    t->kind = TOKEN_OCT_INT_LITERAL;
}

static inline void extract_base_10(Lexer* l, Token* t)
{
    while(c_is_undnum(peek(l)))
        next(l);
    
    bool is_float = false;

    if(peek(l) == '.')
    {
        if(peek_prev(l) == '_')
            goto END_UND;

        next(l);
        if(peek(l) == '_')
        {
            lexer_error_at_current(l, t, "An underscore cannot follow the decimal point in a numeric literal.");
            return;
        }
        is_float = true;
        while(c_is_undnum(peek(l)))
            next(l);
    }

    if(peek_prev(l) == '_')
    {
END_UND:
        backtrack(l);
        lexer_error_at_current(l, t, "Numeric literal cannot finish with an underscore.");
        return;
    }

    if(!extract_num_suffix(l, &is_float))
        return;
    
    t->kind = is_float ? TOKEN_FLOAT_LITERAL : TOKEN_DEC_INT_LITERAL;
    return;
}

static inline void extract_base_16(Lexer* l, Token* t)
{
    if(!c_is_hex(peek(l)))
    {
        lexer_error_at_current(l, t, "0x should be followed by at least 1 octal digit (0-9 or A-F).");
        return;
    }
    next(l);
    while(c_is_hex(peek(l)) || peek(l) == '_')
        next(l);

    if(peek_prev(l) == '_')
    {
        backtrack(l);
        lexer_error_at_current(l, t, "Numeric literal cannot finish with an underscore.");
        return;
    }

    bool is_float = peek(l) == '.';
    if(!extract_num_suffix(l, &is_float))
        return;
    if(is_float)
    {
        lexer_error(l, t, "Hex literals cannot have a fractional component or float suffix.");
        return;
    }
    t->kind = TOKEN_HEX_INT_LITERAL;
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

static inline uint32_t get_col(Lexer* l)
{
    return (uintptr_t)l->cur_pos - (uintptr_t)l->line_start + 1;
}

static inline bool c_is_hex(char c)
{
    static bool is_hex[256] = {
        ['0'] = true,
        ['1'] = true,
        ['2'] = true,
        ['3'] = true,
        ['4'] = true,
        ['5'] = true,
        ['6'] = true,
        ['7'] = true,
        ['8'] = true,
        ['9'] = true,
        ['A'] = true,
        ['B'] = true,
        ['C'] = true,
        ['D'] = true,
        ['E'] = true,
        ['F'] = true,
        ['a'] = true,
        ['b'] = true,
        ['c'] = true,
        ['d'] = true,
        ['e'] = true,
        ['f'] = true,
    };
    return is_hex[(size_t)c];
}

static inline void lexer_error_at_current(Lexer* l, Token* t, const char* msg, ...)
{
    va_list va;
    va_start(va, msg);
    SourceLoc loc;
    loc.file = l->unit->file;
    loc.col_num  = get_col(l);
    loc.line_num = l->cur_line;
    loc.len = 1;
    sic_diagnostic_atv(loc, DIAG_ERROR, msg, va);
    va_end(va);
    t->kind = TOKEN_INVALID;
}

static inline void lexer_error(Lexer* l, Token* t, const char* msg, ...)
{
    va_list va;
    va_start(va, msg);
    t->loc.len = get_col(l) - t->loc.col_num;
    sic_diagnostic_atv(t->loc, DIAG_ERROR, msg, va);
    va_end(va);
    t->kind = TOKEN_INVALID;
}
