#include "utils/error.h"
#include "core/core.h"
#include "core/internal.h"

#include <ctype.h>
#include <stdarg.h>
#include <string.h>

static int s_error_cnt = 0;

void sic_error_fatal(const char* restrict message, ...)
{
    SIC_ASSERT(message != NULL);
    va_list va;
    va_start(va, message);
    fprintf(stderr, "sic: \033[31mfatal error:\033[0m "); 
    vfprintf(stderr, message, va);
    putc('\n', stderr);
    va_end(va);
    SIC_ERROR_DBG("Fatal Error Triggered Debug Break.");
    exit(EXIT_FAILURE);
}

void sic_error(const char* restrict message, ...)
{
    SIC_ASSERT(message != NULL);
    va_list va;
    va_start(va, message);
    fprintf(stderr, "sic: \033[31merror:\033[0m "); 
    vfprintf(stderr, message, va);
    putc('\n', stderr);
    va_end(va);
    s_error_cnt++;
}

void sic_error_weak(const char* restrict message, ...)
{
    SIC_ASSERT(message != NULL);
    va_list va;
    va_start(va, message);
    fprintf(stderr, "sic: \033[31merror:\033[0m "); 
    vfprintf(stderr, message, va);
    putc('\n', stderr);
    va_end(va);
}

void sic_error_atv(const char* filepath, const SourceLoc* loc, 
                   const char* restrict message, va_list va)
{
    SIC_ASSERT(filepath != NULL);
    SIC_ASSERT(loc != NULL);
    SIC_ASSERT(loc->start != NULL);
    SIC_ASSERT(message != NULL);
    char* next_line = strchr(loc->start, '\n');
    const char* loc_end = loc->start + loc->len;
    int before_size = (uintptr_t)loc->start - (uintptr_t)loc->line_start;
    int after_size = next_line == NULL ? strlen(loc_end) : (uintptr_t)next_line - (uintptr_t)loc_end;

    fprintf(stderr, "%s:%u: \033[31merror:\033[0m ", 
            filepath, 
            loc->line_num);
    
    vfprintf(stderr, message, va);

    fprintf(stderr, "\n%4u | %.*s\033[31m%.*s\033[0m%.*s\n     | ", 
            loc->line_num, 
            before_size, loc->line_start,
            (int)loc->len, loc->start,
            after_size,  loc_end);
    for(const char* s = loc->line_start; s < loc->start; ++s)
    {
        if(isspace(*s))
            putc(*s, stderr);
        else
            putc(' ', stderr);
    }
    fprintf(stderr, "\033[31m^");
    for(uint32_t i = 1; i < loc->len; ++i)
        putc('~', stderr);
    fprintf(stderr, "\033[0m\n");
    s_error_cnt++;
}

int sic_error_cnt(void)
{
    return s_error_cnt;
}
