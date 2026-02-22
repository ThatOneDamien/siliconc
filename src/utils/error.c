#include "core/internal.h"
#include "file_utils.h"

#include <ctype.h>
#include <stdarg.h>
#include <string.h>

int g_error_cnt = 0;
int g_warning_cnt = 0;

static const char* const DIAG_COLOR[] = {
    [DIAG_NOTE]    = "\033[34m",
    [DIAG_WARNING] = "\033[95m",
    [DIAG_ERROR]   = "\033[91m",
    [DIAG_FATAL]   = "\033[31m",
};

static const char* const DIAG_NAME[] = {
    [DIAG_NOTE]    = "Note",
    [DIAG_WARNING] = "Warning",
    [DIAG_ERROR]   = "Error",
    [DIAG_FATAL]   = "Fatal Error",
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

static inline const char* color_str(DiagnosticType diag)
{
    return g_compiler.stderr_is_tty ? DIAG_COLOR[diag] : "";
}

static inline const char* reset_color_str()
{
    return g_compiler.stderr_is_tty ? "\033[0m" : "";
}

void sic_diagnosticv(DiagnosticType diag, const char* message, va_list va)
{
    DBG_ASSERT(message != NULL);
    if(g_compiler.werror && diag == DIAG_WARNING)
        diag = DIAG_ERROR;
    fprintf(stderr, "%s: \033[%s%s:%s ", 
            g_compiler.compiler_name, 
            color_str(diag), 
            DIAG_NAME[diag],
            reset_color_str()); 
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
    const char* const color = color_str(diag);
    const char* const reset = reset_color_str();

    fprintf(stderr, "%s:%u:%u: %s%s:%s ",
            file->rel_path, source_lc.line, source_lc.col, color, DIAG_NAME[diag], reset);
    
    vfprintf(stderr, message, va);

    const char* line_start = file->src + file->line_starts.data[source_lc.line - 1];

    // FIXME: Rewrite this to support multi-line.
    fprintf(stderr, "\n%4u | %.*s%s%.*s%s%.*s\n     | ", 
            source_lc.line, 
            source_lc.col - 1, line_start,
            color,
            loc.len, file->src + loc.start,
            reset,
            file->line_starts.data[source_lc.line] - (loc.start + loc.len) - 1, file->src + loc.start + loc.len);
    for(uint32_t i = 0; i < source_lc.col - 1; ++i)
        putc(line_start[i] == '\t' ? '\t' : ' ', stderr);

    fprintf(stderr, "%s^", color);
    for(uint32_t i = 1; i < loc.len; ++i)
        putc('~', stderr);
    fprintf(stderr, "%s\n", reset);
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
    const char* const reset = reset_color_str();

    fprintf(stderr, "%s:%u:%u: %s%s:%s ",
            file->rel_path, source_lc.line, source_lc.col, color_str(diag), DIAG_NAME[diag], reset);
    
    vfprintf(stderr, message, va);

    const char* line_start = file->src + file->line_starts.data[source_lc.line - 1];
    fprintf(stderr, "\n%4u | %.*s %.*s\n     | ", 
            source_lc.line, 
            source_lc.col, line_start,
            file->line_starts.data[source_lc.line] - (loc.start + loc.len) - 1, file->src + loc.start + loc.len);

    for(uint32_t i = 0; i < source_lc.col; ++i)
        putc(line_start[i] == '\t' ? '\t' : ' ', stderr);

    const char* const color = g_compiler.stderr_is_tty ? "\033[34m" : "";
    fprintf(stderr, "%s^%s\n", color, reset);

    if(under != NULL)
    {
        fprintf(stderr, "     | ");
        for(uint32_t i = 0; i < source_lc.col; ++i)
            putc(line_start[i] == '\t' ? '\t' : ' ', stderr);
        fprintf(stderr, "%s%s%s\n", color, under, reset);
    }
    if(diag == DIAG_ERROR)
        g_error_cnt++;
    else if(diag == DIAG_WARNING)
        g_warning_cnt++;
}

