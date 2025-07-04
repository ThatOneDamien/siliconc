#include "lib.h"

ScratchBuffer g_scratch = {{0}, 0};

void scratch_appendf(const char* restrict fmt, ...)
{
    va_list va;
    va_start(va, fmt);
    int size = vsnprintf(g_scratch.data + g_scratch.len, SCRATCH_SIZE - g_scratch.len, fmt, va);
    if(size + g_scratch.len > SCRATCH_SIZE)
        sic_error_fatal("Ran out of space in the scratch buffer. This shouldn't happen.");
    va_end(va);

}

char* str_format(const char* restrict fmt, ...)
{
    va_list va;
    va_start(va, fmt);
    int size = vsnprintf(NULL, 0, fmt, va);
    va_end(va);

    char* buf = cmalloc(size + 1);

    va_start(va, fmt);
    vsnprintf(buf, size + 1, fmt, va);
    va_end(va);
    buf[size] = '\0';
    return buf;
}
