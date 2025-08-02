#pragma once
#include "core/internal.h"

void        input_file_new(const char* path);
void        input_file_read(InputFile* file);
const char* convert_ext_to(const char* path, FileType desired);
bool        file_exists(const char* path);
const char* create_tempfile(FileType ft);
void        close_tempfiles(void);

static inline InputFile* file_from_id(FileId id)
{
    SIC_ASSERT(id < g_args.input_files.size);
    return g_args.input_files.data + id;
}
