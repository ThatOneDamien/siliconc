#pragma once
#include "core/core.h"

typedef struct SIFile SIFile;

typedef enum
{
    FT_UNKNOWN = 0, // Unknown
    FT_SI,          // Silicon source file
    FT_LLVM_IR,     // LLVM IR File (.ll)
    FT_ASM,         // Assembly file
    FT_OBJ,         // Object file
    FT_STATIC,      // Static library (.a)
    FT_SHARED,      // Shared object/library (.so)
} FileType;

struct SIFile
{
    const char* full_path;
    const char* file_name;
    const char* file_ext;
    const char* path_end;
    FileType    type;
};

SIFile      sifile_new(const char* full_path);
bool        file_exists(const char* path);
char*       sifile_read(const SIFile* file);
FILE*       sifile_open_write(const SIFile* file);
SIFile      convert_file_to(const SIFile* file, FileType desired);
const char* convert_ext_to(const char* path, FileType desired);
const char* ft_to_extension(FileType ft);
SIFile      create_tempfile(FileType ft);
void        close_tempfiles(void);

static inline bool sifile_exists(const SIFile* file)
{
    SIC_ASSERT(file != NULL);
    return file_exists(file->full_path);
}
