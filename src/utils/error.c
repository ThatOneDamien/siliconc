#include "core/internal.h"
#include "file_utils.h"

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

void sic_diagnostic(DiagnosticType diag, const char* message, ...)
{
    va_list va;
    va_start(va, message);
    sic_diagnosticv(diag, message, va);
    va_end(va);
}

void sic_diagnosticv(DiagnosticType diag, const char* message, va_list va)
{
    SIC_ASSERT(message != NULL);
    if(g_args.werror && diag == DIAG_WARNING)
        diag = DIAG_ERROR;
    fprintf(stderr, "sic: \033[%sm%s:\033[0m ", DIAG_COLOR[diag], DIAG_NAME[diag]); 
    vfprintf(stderr, message, va);
    putc('\n', stderr);
    if(diag == DIAG_ERROR)
        g_error_cnt++;
    else if(diag == DIAG_WARNING)
        g_warning_cnt++;
}

void sic_diagnostic_at(SourceLoc loc, DiagnosticType diag, const char* message, ...)
{
    va_list va;
    va_start(va, message);
    sic_diagnostic_atv(loc, diag, message, va);
    va_end(va);
}

void sic_diagnostic_atv(SourceLoc loc, DiagnosticType diag, const char* message, va_list va)
{
    SIC_ASSERT(message != NULL);
    if(g_args.werror && diag == DIAG_WARNING)
        diag = DIAG_ERROR;
    SourceFile* file = file_from_id(loc.file);
    const char* line_start = file->src;
    const char* loc_start;
    const char* after_loc;
    uint32_t cnt = 1;
    uint32_t after_len = 0;

    while(cnt < loc.line_num)
    {
        if(*line_start == '\n')
            ++cnt;
        line_start++;
    }

    loc_start = line_start + loc.col_num - 1;
    after_loc = loc_start + loc.len;

    while(after_loc[after_len] != '\0' &&
          after_loc[after_len] != '\n')
        after_len++;


    fprintf(stderr, "%s:%u:%hhu \033[%sm%s:\033[0m ",
            file->rel_path, loc.line_num, loc.col_num, DIAG_COLOR[diag], DIAG_NAME[diag]);
    
    vfprintf(stderr, message, va);

    fprintf(stderr, "\n%4u | %.*s\033[%sm%.*s\033[0m%.*s\n     | ", 
            loc.line_num, 
            loc.col_num - 1, line_start,
            DIAG_COLOR[diag],
            loc.len, loc_start,
            after_len, after_loc);
    for(int i = 0; i < loc.col_num - 1; ++i)
        putc(line_start[i] == '\t' ? '\t' : ' ', stderr);

    fprintf(stderr, "\033[%sm^", DIAG_COLOR[diag]);
    for(uint32_t i = 1; i < loc.len; ++i)
        putc('~', stderr);
    fprintf(stderr, "\033[0m\n");
    if(diag == DIAG_ERROR)
        g_error_cnt++;
    else if(diag == DIAG_WARNING)
        g_warning_cnt++;
}

void sic_error_redef(Object* redef, Object* orig)
{
    sic_diagnostic_at(redef->loc, DIAG_ERROR, "Redefinition of symbol \'%s\'.", redef->symbol);
    sic_diagnostic_at(orig->loc, DIAG_NOTE, "Previous definition here.");
}
