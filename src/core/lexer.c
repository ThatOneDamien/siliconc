#include "internal.h"
#include "utils/file_utils.h"

#include <string.h>

#define at_eof(lex)         ((lex)->cur_pos[0] == '\0')
#define peek(lex)           ((lex)->cur_pos[0])
#define peek_next(lex)      ((lex)->cur_pos[1])
#define peek_prev(lex)      ((lex)->cur_pos[-1])
#define advance(lex, count) ((lex)->cur_pos += count)
#define next(lex)           (++(lex)->cur_pos)
#define backtrack(lex)      (--(lex)->cur_pos)

typedef struct
{
    char*    data;
    uint32_t size;
    uint32_t capacity;
} StringBuilder;

static inline void     skip_invisible(Lexer* l);
static inline void     extract_identifier(Lexer* l, Token* t);
static inline void     extract_ct_identifier(Lexer* l, Token* t);
static inline void     extract_attr_identifier(Lexer* l, Token* t);
static inline void     extract_char_literal(Lexer* l, Token* t, CharEncoding encoding);
static inline void     extract_string_literal(Lexer* l, Token* t, CharEncoding encoding);
static inline void     extract_raw_string_literal(Lexer* l, Token* t);
static inline void     extract_base_2(Lexer* l, Token* t);
static inline void     extract_base_8(Lexer* l, Token* t);
static inline void     extract_base_10(Lexer* l, Token* t);
static inline void     extract_base_16(Lexer* l, Token* t);
static inline bool     extract_num_suffix(Lexer* l, bool* is_float);
static ByteSize        next_char_in_literal(Lexer* l, uint32_t* value, CharEncoding encoding);
static inline ByteSize next_utf8(Lexer* l, uint32_t* codepoint);
static inline ByteSize unicode_to_utf8(uint32_t* value);
static inline bool     consume(Lexer* l, char c);
static inline char     next_nl(Lexer* l);
static inline Token*   next_token_loc(Lexer* l);
static inline uint32_t get_col(Lexer* l);
static inline FileId   lexer_file_id(Lexer* l);
static inline bool     c_is_hex(char c);

PRINTF_FMT(3, 4)
static inline void lexer_error_at_current(Lexer* l, Token* t, const char* msg, ...);
PRINTF_FMT(3, 4)
static inline void lexer_error(Lexer* l, Token* t, const char* msg, ...);

const uint8_t g_hex_char_to_val[256] = {
    ['0'] = 1,
    ['1'] = 2,
    ['2'] = 3,
    ['3'] = 4,
    ['4'] = 5,
    ['5'] = 6,
    ['6'] = 7,
    ['7'] = 8,
    ['8'] = 9,
    ['9'] = 10,
    ['A'] = 11, ['a'] = 11,
    ['B'] = 12, ['b'] = 12,
    ['C'] = 13, ['c'] = 13,
    ['D'] = 14, ['d'] = 14,
    ['E'] = 15, ['e'] = 15,
    ['F'] = 16, ['f'] = 16,
};

Lexer lexer_from_source(FileId fileid)
{
    SourceFile* file = file_from_id(fileid);
    Lexer l;
    l.module     = file->module;
    l.cur_pos    = file->src;
    l.cur_line   = 1;
    l.line_start = l.cur_pos;
    memset(&l.la_buf, 0, sizeof(LookAhead));
    l.la_buf.cur = 1;
    for(size_t i = 0; i < LOOK_AHEAD_SIZE - 1; ++i)
    {
        lexer_advance(&l);
        l.la_buf.buf[i].loc.file = fileid;
    }
    l.la_buf.buf[LOOK_AHEAD_SIZE - 1].loc.file = fileid;
    return l;
}

void lexer_advance(Lexer* l)
{
    SIC_ASSERT(l != NULL);

    skip_invisible(l);

    Token* t = next_token_loc(l);
    if(t->kind == TOKEN_EOF)
        return;

    t->loc.col_num  = get_col(l);
    t->loc.line_num = l->cur_line;
    t->start        = l->cur_pos;

    if(at_eof(l))
    {
        t->kind = TOKEN_EOF;
        t->loc.len = 1;
        return;
    }

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
        if(consume(l, '.'))
            t->kind = consume(l, '.') ? TOKEN_ELLIPSIS : TOKEN_RANGE;
        else
            t->kind = TOKEN_DOT;
        break;
    case ':':
        t->kind = consume(l, ':') ? TOKEN_NAMESPACE : TOKEN_COLON;
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
        extract_char_literal(l, t, CHAR_ENCODING_UTF8);
        break;
    case '\"':
        extract_string_literal(l, t, CHAR_ENCODING_UTF8);
        break;
    case 'u':
        switch(peek(l))
        {
        case '\'': 
            extract_char_literal(l, t, CHAR_ENCODING_UTF16);
            break;
        case '\"':
            extract_string_literal(l, t, CHAR_ENCODING_UTF16);
            break;
        default:
            goto IDENT;
        }
        break;
    case 'U':
        switch(peek(l))
        {
        case '\'': 
            extract_char_literal(l, t, CHAR_ENCODING_UTF32);
            break;
        case '\"':
            extract_string_literal(l, t, CHAR_ENCODING_UTF32);
            break;
        default:
            goto IDENT;
        }
        break;
    case '`':
        extract_raw_string_literal(l, t);
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
    case '_':
        if(!c_is_undalphanum(peek(l)))
        {
            t->kind = TOKEN_UNDERSCORE;
            break;
        }
        FALLTHROUGH;
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
    case 'v':
    case 'w':
    case 'x':
    case 'y':
    case 'z':
    IDENT:
        extract_identifier(l, t);
        return;
    case '#':
        extract_ct_identifier(l, t);
        return;
    case '@':
        extract_attr_identifier(l, t);
        return;
    default:
        t->loc.len = 1;
        sic_error_at(t->loc, "Invalid/unknown character. For now, siliconc does not support "
                             "unicode charcters anywhere except in char/string literals.");
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

static inline void extract_ct_identifier(Lexer* l, Token* t)
{
    while(c_is_undalphanum(peek(l)))
        next(l);

    t->loc.len = get_col(l) - t->loc.col_num;
    t->sym = sym_map_getn(t->start, t->loc.len, &t->kind);
    if(t->sym == NULL)
    {
        sic_error_at(t->loc, "Unknown compile-time identifier.");
        t->kind = TOKEN_INVALID;
    }
}

static inline void extract_attr_identifier(Lexer* l, Token* t)
{
    while(c_is_undalphanum(peek(l)))
        next(l);

    t->loc.len = get_col(l) - t->loc.col_num;
    t->kind = TOKEN_ATTRIBUTE_IDENT;
    t->sym = sym_map_addn(t->start, t->loc.len, &t->kind);
}

static inline void extract_char_literal(Lexer* l, Token* t, CharEncoding encoding)
{
    ByteSize len = next_char_in_literal(l, &t->chr.val, CHAR_ENCODING_UTF8);
    if(len == 0)
        t->kind = TOKEN_INVALID;

    char c;
    bool valid = peek(l) == '\'';
    while((c = peek(l)) != '\'')
    {
        if(c == '\n' || c == '\0')
        {
            lexer_error(l, t, "Encountered end of the line while lexing "
                              "char literal. Did you forget a '?");
            return;
        }
        next(l);
    }
    next(l);

    if(!valid)
    {
        lexer_error(l, t, "Multi-character char literals are not allowed.");
        return;
    }
    if(len > (ByteSize)encoding)
    {
        lexer_error(l, t, "Value of char literal cannot fit in its containing "
                          "type. For char16 and char32 add prefix u/U respectively.");
        return;
    }
    t->kind = TOKEN_CHAR_LITERAL;
}

static inline void extract_string_literal(Lexer* l, Token* t, CharEncoding encoding)
{
    StringBuilder sb;
    da_init(&sb, 256);
    bool errored = false;
    char c;
    while((c = peek(l)) != '\"')
    {
        if(c == '\n' || c == '\0')
        {
            lexer_error(l, t, "Encountered end of the line while lexing "
                              "string literal. Did you forget a '\"'?");
            return;
        }

        uint32_t value;
        ByteSize len = next_char_in_literal(l, &value, encoding);
        if(!errored && len == 0)
        {
            errored = true;
            t->kind = TOKEN_INVALID;
            continue;
        }


        if(!errored)
            da_append_arr(&sb, (char*)&value, len);
    }

    next(l);
    if(errored)
        return;

    t->str.len = sb.size;
    da_append(&sb, '\0');

    da_compact(&sb);
    t->str.val = sb.data; 
    t->kind = TOKEN_STRING_LITERAL;
}

static inline void extract_raw_string_literal(Lexer* l, Token* t)
{
    StringBuilder sb;
    da_init(&sb, 256);
    char c;
    while((c = peek(l)) != '`')
    {
        if(c == '\0')
        {
            lexer_error(l, t, "Encountered end of file while lexing raw "
                              "string literal. Did you forget a '`'?");
            return;
        }
        da_append(&sb, c);
        next_nl(l);
    }

    next(l);
    t->str.len = sb.size;
    da_append(&sb, '\0');

    da_compact(&sb);
    t->str.val = sb.data; 
    t->kind = TOKEN_STRING_LITERAL;
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

    if(peek(l) == '.' && peek_next(l) != '.')
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

static ByteSize next_char_in_literal(Lexer* l, uint32_t* value, CharEncoding encoding)
{
    if(peek(l) == '\\')
    {
        const char* msg;
        SourceLoc loc;
        loc.file = lexer_file_id(l);
        loc.col_num = get_col(l);
        loc.line_num = l->cur_line;

        next(l);
        switch(*(l->cur_pos++))
        {
        case 'a': // Audible Bell
            *value = 0x07;
            return 1;
        case 'b': // Backspace
            *value = 0x08;
            return 1;
        case 'e': // ANSI Escape Starting Byte
            *value = 0x1B;
            return 1;
        case 'f': // Form Feed
            *value = 0x0C;
            return 1;
        case 'n': // Line Feed/New Line
            *value = 0x0A;
            return 1;
        case 'o': { // Arbitrary Octal Value
            msg = "'\\o' must be followed by exactly 3 octal digits(0-7).";
            uint8_t d1 = peek(l) - '0';
            if(d1 > 7) break;
            next(l);
            uint8_t d2 = peek(l) - '0';
            if(d2 > 7) break;
            next(l);
            uint8_t d3 = peek(l) - '0';
            if(d3 > 7) break;
            next(l);
            if(d1 > 3)
            {
                msg = "Value exceeds the maximum for one byte(0o377).";
                break;
            }

            *value = (d1 << 6) | (d2 << 3) | d3;
            return 1;
        }
        case 'r': // Carriage Return
            *value = 0x0D;
            return 1;
        case 't': // Horizontal Tab
            *value = 0x09;
            return 1;
        case 'u': {
            uint32_t val = 0;
            for(int i = 0; i < 4; ++i)
            {
                uint8_t v = g_hex_char_to_val[(size_t)peek(l)];
                if(v == 0)
                {
                    msg = "'\\u' must be followed by exactly 4 hexadecimal digits(0-F).";
                    break;
                }
                next(l);
                val = (val << 4) | (v - 1);
            }

            *value = val;
            goto UNICODE;
        }
        case 'U': {
            uint32_t val = 0;
            for(int i = 0; i < 8; ++i)
            {
                uint8_t v = g_hex_char_to_val[(size_t)peek(l)];
                if(v == 0)
                {
                    msg = "'\\U' must be followed by exactly 8 hexadecimal digits(0-F).";
                    break;
                }
                next(l);
                val = (val << 4) | (v - 1);
            }
            if(val > 0x10FFFF)
            {
                msg = "Invalid universal character code.";
                break;
            }

            *value = val;
            goto UNICODE;
        }
        case 'v': // Vertical Tab
            *value = 0x0B;
            return 1;
        case 'x': { // Arbitrary Hex Value
            msg = "'\\x' must be followed by exactly 2 hexadecimal digits(0-F).";
            uint8_t d1 = g_hex_char_to_val[(size_t)peek(l)];
            if(d1 == 0) break;
            next(l);
            uint8_t d2 = g_hex_char_to_val[(size_t)peek(l)];
            if(d1 == 0) break;
            next(l);

            *value = ((d1 - 1) << 4) | (d2 - 1);
            return 1;
        }
        case '\"': // Double quote
            *value = 0x22;
            return 1;
        case '\'': // Single quote
            *value = 0x27;
            return 1;
        case '0': // Null character
            *value = 0x00;
            return 1;
        case '\\':
            *value = 0x5C;
            return 1;
        case '?':
            *value = 0x3F;
            return 1;
        default:
            msg = "Invalid escape sequence.";
            break;
        }

        loc.len = get_col(l) - loc.col_num;
        sic_error_at(loc, "%s", msg);
        return 0;
    }
    else if(next_utf8(l, value) == 0)
        return 0;

UNICODE:
    // If we are here, value is a unicode codepoint that
    // now needs to be converted to the encoding specified
    // by the literal width
    switch(encoding)
    {
    case CHAR_ENCODING_UTF8:
        return unicode_to_utf8(value);
    case CHAR_ENCODING_UTF16:
    case CHAR_ENCODING_UTF32:
    default:
        SIC_UNREACHABLE();
    }
    
}

static inline ByteSize next_utf8(Lexer* l, uint32_t* codepoint)
{
    SourceLoc loc;
    uint32_t point;
    ByteSize len;
    const uint8_t* const p = (uint8_t*)l->cur_pos;

    if (p[0] < 0x80) 
    {
        point = p[0];
        len = 1;
    }
    else if ((p[0] & 0xE0) == 0xC0) 
    {
        if ((p[1] & 0xC0) != 0x80) 
            goto ERR;

        point = ((p[0] & 0x1F) << 6) | 
                (p[1] & 0x3F);

        if (point < 0x80) goto ERR;
        len = 2;
    }
    else if ((p[0] & 0xF0) == 0xE0) 
    {
        if ((p[1] & 0xC0) != 0x80 || (p[2] & 0xC0) != 0x80) 
            goto ERR;

        point = ((p[0] & 0x0F) << 12) |
                ((p[1] & 0x3F) << 6) |
                (p[2] & 0x3F);

        if (point < 0x800) goto ERR;
        if (point >= 0xD800 && point <= 0xDFFF) goto ERR;
        len = 3;
    }
    else if ((p[0] & 0xF8) == 0xF0) 
    {
        if ((p[1] & 0xC0) != 0x80 ||
            (p[2] & 0xC0) != 0x80 ||
            (p[3] & 0xC0) != 0x80) goto ERR;

        point = ((p[0] & 0x07) << 18) |
                ((p[1] & 0x3F) << 12) |
                ((p[2] & 0x3F) << 6) |
                (p[3] & 0x3F);
        if (point < 0x10000 || point > 0x10FFFF) goto ERR;
        len = 4;
    }
    else
        goto ERR;

    *codepoint = point;
    advance(l, len);
    return len;

ERR:
    loc.file = lexer_file_id(l);
    loc.col_num  = get_col(l);
    loc.line_num = l->cur_line;
    loc.len = 1;
    next(l);
    sic_error_at(loc, "Invalid utf-8 encoded character.");
    return 0;
}

static inline ByteSize unicode_to_utf8(uint32_t* value)
{
    uint32_t codepoint = *value;
    char* utf8_buf = (char*)value;
    if(codepoint <= 0x7F)
        return 1; // Do nothing
    if(codepoint <= 0x7FF)
    {
        utf8_buf[0] = 0xC0 | (codepoint >> 6);
        utf8_buf[1] = 0x80 | (codepoint & 0x3F);
        return 2;
    }
    if(codepoint <= 0xFFFF)
    {
        utf8_buf[0] = 0xE0 | (codepoint >> 12);
        utf8_buf[1] = 0x80 | ((codepoint >> 6) & 0x3F);
        utf8_buf[2] = 0x80 | (codepoint & 0x3F);
        return 3;
    }

    SIC_ASSERT(codepoint <= 0x10FFFF);
    utf8_buf[0] = 0xF0 | (codepoint >> 18);
    utf8_buf[1] = 0x80 | ((codepoint >> 12) & 0x3F);
    utf8_buf[2] = 0x80 | ((codepoint >> 6) & 0x3F);
    utf8_buf[3] = 0x80 | (codepoint & 0x3F);
    return 4;
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
    char c = peek(l); 
    if(c == '\n')
    {
        l->cur_line++;
        l->line_start = l->cur_pos + 1;
    }
    next(l);
    return c;
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

static inline FileId lexer_file_id(Lexer* l)
{
    return l->la_buf.buf[0].loc.file;
}

static inline bool c_is_hex(char c)
{
    return g_hex_char_to_val[(uint8_t)c] != 0;
}

static inline void lexer_error_at_current(Lexer* l, Token* t, const char* msg, ...)
{
    va_list va;
    va_start(va, msg);
    SourceLoc loc;
    loc.file = lexer_file_id(l);
    loc.col_num  = get_col(l);
    loc.line_num = l->cur_line;
    loc.len = 1;
    sic_diagnostic_atv(DIAG_ERROR, loc, msg, va);
    va_end(va);
    t->kind = TOKEN_INVALID;
}

static inline void lexer_error(Lexer* l, Token* t, const char* msg, ...)
{
    va_list va;
    va_start(va, msg);
    t->loc.len = get_col(l) - t->loc.col_num;
    sic_diagnostic_atv(DIAG_ERROR, t->loc, msg, va);
    va_end(va);
    t->kind = TOKEN_INVALID;
}
