#pragma once
#include "core/internal.h"

#define FILE_NULL ((FileId)0)

void        get_current_dir();
FileId      source_file_add_or_get(const char* path, ObjModule* module);
FileId      find_and_open_module_path(ObjModule* module);
const char* convert_ext_to(const char* path, FileType desired);
bool        file_exists(const char* path);
const char* create_tempfile(FileType ft);
void        close_tempfiles(void);
FileType    get_filetype(const char* path);

static inline SourceFile* file_from_id(FileId id)
{
    SIC_ASSERT(id > 0 && id <= g_compiler.sources.size);
    return g_compiler.sources.data + (id - 1);
}
