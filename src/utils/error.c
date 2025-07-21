#include "core/internal.h"

#include <ctype.h>
#include <stdarg.h>
#include <string.h>

int g_error_cnt = 0;
int g_warning_cnt = 0;

static const char* DIAG_COLOR[] = {
    [DIAG_NOTE]    = "34",
    [DIAG_WARNING] = "95",
    [DIAG_ERROR]   = "91",
    [DIAG_FATAL]   = "31",
};

static const char* DIAG_NAME[] = {
    [DIAG_NOTE]    = "note",
    [DIAG_WARNING] = "warning",
    [DIAG_ERROR]   = "error",
    [DIAG_FATAL]   = "fatal error",
};

void sic_diagnostic(DiagnosticType diag, const char* restrict message, ...)
{
    va_list va;
    va_start(va, message);
    sic_diagnosticv(diag, message, va);
    va_end(va);
}

void sic_diagnosticv(DiagnosticType diag, const char* restrict message, va_list va)
{
    SIC_ASSERT(message != NULL);
    fprintf(stderr, "sic: \033[%sm%s:\033[0m ", DIAG_COLOR[diag], DIAG_NAME[diag]); 
    vfprintf(stderr, message, va);
    putc('\n', stderr);
    if(diag == DIAG_ERROR)
        g_error_cnt++;
    else if(diag == DIAG_WARNING)
        g_warning_cnt++;
}

void sic_diagnostic_at(const char* filepath, const SourceLoc* loc, DiagnosticType diag,
                       const char* restrict message, ...)
{
    va_list va;
    va_start(va, message);
    sic_diagnostic_atv(filepath, loc, diag, message, va);
    va_end(va);
}

void sic_diagnostic_atv(const char* filepath, const SourceLoc* loc, DiagnosticType diag,
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

    fprintf(stderr, "%s:%u: \033[%sm%s:\033[0m ",
            filepath, loc->line_num, DIAG_COLOR[diag], DIAG_NAME[diag]);
    
    vfprintf(stderr, message, va);

    fprintf(stderr, "\n%4u | %.*s\033[%sm%.*s\033[0m%.*s\n     | ", 
            loc->line_num, 
            before_size, loc->line_start,
            DIAG_COLOR[diag],
            (int)loc->len, loc->start,
            after_size,  loc_end);
    for(const char* s = loc->line_start; s < loc->start; ++s)
    {
        if(isspace(*s))
            putc(*s, stderr);
        else
            putc(' ', stderr);
    }
    fprintf(stderr, "\033[%sm^", DIAG_COLOR[diag]);
    for(uint32_t i = 1; i < loc->len; ++i)
        putc('~', stderr);
    fprintf(stderr, "\033[0m\n");
    if(diag == DIAG_ERROR)
        g_error_cnt++;
    else if(diag == DIAG_WARNING)
        g_warning_cnt++;
}
