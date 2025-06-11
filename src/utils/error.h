#pragma once

#include <stdarg.h>
#include <stdbool.h>
#include <stddef.h>

__attribute__((format(printf, 1, 2),noreturn))
void sic_error_fatal(const char* restrict message, ...);

__attribute__((format(printf, 1, 2)))
void sic_error(const char* restrict message, ...);

__attribute__((format(printf, 1, 2)))
void sic_error_weak(const char* restrict message, ...);

int sic_error_cnt(void);
