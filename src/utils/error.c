#include "core/internal.h"
#include "file_utils.h"

#include <ctype.h>
#include <stdarg.h>
#include <string.h>

int g_error_cnt = 0;
int g_warning_cnt = 0;

static const char* const DIAG_COLOR[] = {
    [DIAG_NOTE]    = "34",
    [DIAG_WARNING] = "95",
    [DIAG_ERROR]   = "91",
    [DIAG_FATAL]   = "31",
};

static const char* const DIAG_NAME[] = {
    [DIAG_NOTE]    = "note",
    [DIAG_WARNING] = "warning",
    [DIAG_ERROR]   = "error",
    [DIAG_FATAL]   = "fatal error",
};

LineCol loc_get_col_line(const SourceLoc loc)
{
    SourceFile* file = file_from_id(loc.file);
    uint32_t lo = 0;
    uint32_t hi = file->line_starts.size - 1;

    while (lo + 1 < hi) {
        uint32_t mid = lo + (hi - lo) / 2;

        if (file->line_starts.data[mid] <= loc.start)
            lo = mid;
        else
            hi = mid;
    }

    LineCol result;
    result.line = lo + 1;
    result.col = loc.start - file->line_starts.data[lo] + 1;
    return result;
}

void sic_diagnosticv(DiagnosticType diag, const char* message, va_list va)
{
    DBG_ASSERT(message != NULL);
    if(g_compiler.werror && diag == DIAG_WARNING)
        diag = DIAG_ERROR;
    fprintf(stderr, "sic: \033[%sm%s:\033[0m ", DIAG_COLOR[diag], DIAG_NAME[diag]); 
    vfprintf(stderr, message, va);
    putc('\n', stderr);
    if(diag == DIAG_ERROR)
        g_error_cnt++;
    else if(diag == DIAG_WARNING)
        g_warning_cnt++;
}


void sic_diagnostic_atv(DiagnosticType diag, SourceLoc loc, const char* message, va_list va)
{
    DBG_ASSERT(message != NULL);
    if(g_compiler.werror && diag == DIAG_WARNING)
        diag = DIAG_ERROR;
    SourceFile* file = file_from_id(loc.file);
    LineCol source_lc = loc_get_col_line(loc);

    fprintf(stderr, "%s:%u:%u: \033[%sm%s:\033[0m ",
            file->rel_path, source_lc.line, source_lc.col, DIAG_COLOR[diag], DIAG_NAME[diag]);
    
    vfprintf(stderr, message, va);

    const char* line_start = file->src + file->line_starts.data[source_lc.line - 1];

    // FIXME: Rewrite this to support multi-line.
    fprintf(stderr, "\n%4u | %.*s\033[%sm%.*s\033[0m%.*s\n     | ", 
            source_lc.line, 
            source_lc.col - 1, line_start,
            DIAG_COLOR[diag],
            loc.len, file->src + loc.start,
            file->line_starts.data[source_lc.line] - (loc.start + loc.len) - 1, file->src + loc.start + loc.len);
    for(uint32_t i = 0; i < source_lc.col - 1; ++i)
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

void sic_diagnostic_afterv(DiagnosticType diag, SourceLoc loc, const char* under, const char* message, va_list va)
{
    DBG_ASSERT(message != NULL);
    if(g_compiler.werror && diag == DIAG_WARNING)
        diag = DIAG_ERROR;
    SourceFile* file = file_from_id(loc.file);
    loc.start += loc.len - 1;
    loc.len = 1;
    LineCol source_lc = loc_get_col_line(loc);

    fprintf(stderr, "%s:%u:%u: \033[%sm%s:\033[0m ",
            file->rel_path, source_lc.line, source_lc.col, DIAG_COLOR[diag], DIAG_NAME[diag]);
    
    vfprintf(stderr, message, va);

    const char* line_start = file->src + file->line_starts.data[source_lc.line - 1];
    fprintf(stderr, "\n%4u | %.*s %.*s\n     | ", 
            source_lc.line, 
            source_lc.col, line_start,
            file->line_starts.data[source_lc.line] - (loc.start + loc.len) - 1, file->src + loc.start + loc.len);

    for(uint32_t i = 0; i < source_lc.col; ++i)
        putc(line_start[i] == '\t' ? '\t' : ' ', stderr);
    fprintf(stderr, "\033[34m^\033[0m\n");

    if(under != NULL)
    {
        fprintf(stderr, "     | ");
        for(uint32_t i = 0; i < source_lc.col; ++i)
            putc(line_start[i] == '\t' ? '\t' : ' ', stderr);
        fprintf(stderr, "\033[34m%s\033[0m\n", under);
    }
    if(diag == DIAG_ERROR)
        g_error_cnt++;
    else if(diag == DIAG_WARNING)
        g_warning_cnt++;
}

