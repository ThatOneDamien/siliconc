#include "lib.h"

char* str_format(const char* restrict format, ...)
{
    va_list va;
    va_start(va, format);
    int size = vsnprintf(NULL, 0, format, va);
    va_end(va);

    char* buf = cmalloc(size + 1);

    va_start(va, format);
    vsnprintf(buf, size + 1, format, va);
    va_end(va);
    buf[size] = '\0';
    return buf;
}
