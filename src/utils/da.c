#include "da.h"

#include <stdarg.h>

void sb_appendf(StringBuilder* sb, const char* restrict format, ...)
{
    DA_ASSERT(sb != NULL);
    DA_ASSERT(format != NULL);
    va_list va;
    va_start(va, format);
    int cnt = vsnprintf(NULL, 0, format, va);
    va_end(va);

    da_reserve(sb, sb->size + cnt + 1);
    va_start(va, format);
    vsnprintf(sb->data + sb->size, cnt + 1, format, va);
    va_end(va);
    sb->size += cnt;
}
