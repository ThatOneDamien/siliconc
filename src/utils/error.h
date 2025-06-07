#pragma once

#include <stdbool.h>
#include <stddef.h>

__attribute__((format(printf, 1, 2),noreturn))
void sic_error_fatal(const char* restrict message, ...);

__attribute__((format(printf, 1, 2)))
void sic_error(const char* restrict message, ...);

__attribute__((format(printf, 5, 6)))
void sic_error_in_src(const char* filepath, size_t line,
                      const char* line_start, const char* err_loc, 
                      const char* restrict message, ...);

bool sic_has_error(void);
