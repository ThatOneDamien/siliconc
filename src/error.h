#pragma once

#include <stddef.h>

void sic_error_fatal(const char* message);
void sic_error(const char* filepath, size_t line,
               const char* source_start, const char* err_loc, 
               const char* message);
