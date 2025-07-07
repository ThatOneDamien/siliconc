#pragma once
#include "core/core.h"

#include <stdarg.h>
#include <stdbool.h>
#include <stddef.h>

PRINTF_FMT(1, 2) NORETURN
void sic_error_fatal(const char* restrict message, ...);

PRINTF_FMT(1, 2)
void sic_error(const char* restrict message, ...);

PRINTF_FMT(1, 2)
void sic_error_weak(const char* restrict message, ...);

int sic_error_cnt(void);

#define SIC_UNREACHABLE() sic_error_fatal("Compiler encountered an unexpected error, should be unreachable. %s:%d(%s)", __FILE__, __LINE__, __FUNCTION__)
#define SIC_TODO()        sic_error_fatal("\033[32mTODO: %s:%d(%s)\033[0m Not yet implemented.", __FILE__, __LINE__, __FUNCTION__)
#define SIC_TODO_MSG(msg) sic_error_fatal("\033[32mTODO: %s:%d(%s)\033[0m %s", __FILE__, __LINE__, __FUNCTION__, msg)
