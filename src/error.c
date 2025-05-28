#include "error.h"
#include "core.h"

#include <ctype.h>
#include <string.h>

void sic_error_fatal(const char* message)
{
    SIC_ASSERT(message != NULL);
    fprintf(stderr, "sic: \033[31merror:\033[0m %s\n", message); 
}

void sic_error(const char* filepath, size_t line,
               const char* source_start, const char* err_loc, 
               const char* message)
{
    SIC_ASSERT(source_start != NULL);
    SIC_ASSERT(err_loc != NULL);
    SIC_ASSERT(message != NULL);
    const char* line_start = err_loc;
    while(line_start >= source_start && *line_start != '\n')
        line_start--;
    line_start++;
    char* next_line = strchr(line_start, '\n');
    int line_size = next_line == NULL ? 
                        strlen(line_start) :
                        (uintptr_t)next_line - (uintptr_t)line_start;

    fprintf(stderr, "%s:%lu: \033[31merror:\033[0m %s\n", 
            filepath, 
            line, 
            message);
    fprintf(stderr, "%4lu | %.*s\n     | ", 
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
