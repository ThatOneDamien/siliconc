#pragma once
#include "core/internal.h"

SourceFile* source_file_add_or_get(const char* path);
const char* convert_ext_to(const char* path, FileType desired);
bool        file_exists(const char* path);
const char* create_tempfile(FileType ft);
void        close_tempfiles(void);
FileType    get_filetype(const char* path);

static inline SourceFile* file_from_id(FileId id)
{
    SIC_ASSERT(id < g_args.input_files.size);
    return g_compiler.sources.data + id;
}
