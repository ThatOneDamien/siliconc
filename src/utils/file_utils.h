#pragma once

#include <stdbool.h>

typedef enum
{
    FT_NONE = 0,
    FT_ASM,
    FT_OBJ,
    FT_SI
} FileType;

bool     file_exists(const char* path);
char*    read_entire_file(const char* path);
FileType get_filetype(const char* filename);
