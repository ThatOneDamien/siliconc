#pragma once
#include "core/internal.h"

#include <stdarg.h>
#include <stdbool.h>
#include <stddef.h>

__attribute__((format(printf, 1, 2),noreturn))
void sic_error_fatal(const char* restrict message, ...);

__attribute__((format(printf, 1, 2)))
void sic_error(const char* restrict message, ...);

void sic_error_atv(const char* filepath, const Token* t, const char* restrict message, va_list va);

__attribute__((format(printf, 3, 4)))
static inline void sic_error_at(const char* filepath, const Token* t, const char* restrict message, ...)
{
    va_list va;
    va_start(va, message);
    sic_error_atv(filepath, t, message, va);
    va_end(va);
}

int sic_error_cnt(void);
