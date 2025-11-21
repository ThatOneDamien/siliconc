#include "lib.h"
#include <stdarg.h>

ScratchBuffer g_scratch = {{0}, 0};

void scratch_appendf(const char* fmt, ...)
{
    va_list va;
    va_start(va, fmt);
    int size = vsnprintf(g_scratch.data + g_scratch.len, SCRATCH_SIZE - g_scratch.len, fmt, va);
    if(size + g_scratch.len > SCRATCH_SIZE)
        sic_fatal_error("Ran out of space in the scratch buffer. This shouldn't happen.");
    va_end(va);

}

char* str_format(const char* fmt, ...)
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

char* str_dupn(const char* str, size_t len)
{
    SIC_ASSERT(str != NULL);
    SIC_ASSERT(len != 0);
    // TODO: Replace this with dedicated string arena allocation
    //       to reduce fragmentation.
    char* res = MALLOC(len + 1, 1);
    memcpy(res, str, len);
    res[len] = '\0';
    return res;
}
