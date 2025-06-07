#include "utils/error.h"
#include "core/core.h"

#include <ctype.h>
#include <stdarg.h>
#include <string.h>

static bool s_has_error = false;

void sic_error_fatal(const char* restrict message, ...)
{
    SIC_ASSERT(message != NULL);
    va_list va;
    va_start(va, message);
    fprintf(stderr, "sic: \033[31mfatal error:\033[0m "); 
    vfprintf(stderr, message, va);
    putc('\n', stderr);
    va_end(va);
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
    s_has_error = true;
}

void sic_error_in_src(const char* filepath, size_t line,
                      const char* line_start, const char* err_loc, 
                      const char* restrict message, ...)
{
    SIC_ASSERT(filepath != NULL);
    SIC_ASSERT(line_start != NULL);
    SIC_ASSERT(err_loc != NULL);
    SIC_ASSERT(message != NULL);
    char* next_line = strchr(line_start, '\n');
    int line_size = next_line == NULL ? 
                        strlen(line_start) :
                        (uintptr_t)next_line - (uintptr_t)line_start;

    fprintf(stderr, "%s:%lu: \033[31merror:\033[0m ", 
            filepath, 
            line);
    
    va_list va;
    va_start(va, message);
    vfprintf(stderr, message, va);
    va_end(va);

    fprintf(stderr, "\n%4lu | %.*s\n     | ", 
            line, 
            line_size,
            line_start);
    for(; line_start < err_loc; ++line_start)
    {
        if(isspace(*line_start))
            putc(*line_start, stderr);
        else
            putc(' ', stderr);
    }
    fprintf(stderr, "^\n");
}

bool sic_has_error(void)
{
    bool err = s_has_error;
    s_has_error = false;
    return err;
}
