#pragma once

#include <stdbool.h>
#include <stdio.h>

typedef enum
{
    FT_NONE = 0, // Invalid
    FT_SI,       // Silicon source file
    FT_ASM,      // Assembly file
    FT_OBJ,      // Object file
    FT_STATIC,   // Static library (.a)
    FT_SHARED,   // Shared object/library (.so)
    FT_EXEC,     // ELF Executable
} FileType;

bool        file_exists(const char* path);
char*       read_entire_file(const char* path);
FILE*       open_out_file(const char* path);
char*       convert_ext_to(const char* orig_path, FileType desired);
FileType    get_filetype(const char* filename);
const char* ft_to_extension(FileType ft);
