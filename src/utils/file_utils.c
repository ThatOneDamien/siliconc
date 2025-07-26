#define _POSIX_C_SOURCE 200809L
#define _DEFAULT_SOURCE
#include "file_utils.h"
#include "da.h"
#include "core/structs.h"

#include <fcntl.h>
#include <unistd.h>
#include <sys/stat.h>

#include <stdlib.h>
#include <string.h>

#define FILE_FOUND 0

static StringDA s_tempfiles = {0};

static FileType get_filetype(const char* extension);

SIFile sifile_new(const char* full_path)
{
    SIC_ASSERT(full_path != NULL);
    SIFile file;
    file.full_path = full_path;
    file.file_name = full_path;
    file.file_ext  = NULL;
    if(*full_path == '\0')
        sic_fatal_error("Tried to use file with empty path.");
    
    for(file.path_end = full_path; *file.path_end != '\0'; ++file.path_end)
    {
        if(*file.path_end == '/')
        {
            if(file.path_end[1] == '\0')
                sic_fatal_error("File path ends with /. (Indicates directory)");
            file.file_name = file.path_end + 1;
        }
        else if(*file.path_end == '.')
            file.file_ext = file.path_end;
    }
    if(file.file_ext == NULL || (uintptr_t)file.file_ext <= (uintptr_t)file.file_name)
    {
        file.type = FT_UNKNOWN;
        file.file_ext = file.path_end;
    }
    else
        file.type = get_filetype(file.file_ext + 1);

    return file;
}

bool file_exists(const char* path)
{
    SIC_ASSERT(path != NULL);
    struct stat st;
    return stat(path, &st) == FILE_FOUND;
}

char* sifile_read(const SIFile* file)
{
    SIC_ASSERT(file != NULL);
    SIC_ASSERT(file->full_path != NULL);

    char* res = NULL;
    int fd = open(file->full_path, O_RDONLY);

    if(fd == -1)
        goto ERR;

    long size = lseek(fd, 0, SEEK_END);
    if(size <= 0)
        goto ERR;

    if(lseek(fd, 0, SEEK_SET) < 0)
        goto ERR;

    res = cmalloc(sizeof(char) * size + 2);

    long orig_size = size;
    char* ptr = res;
    while(size > 0)
    {
        ssize_t bytes_read = read(fd, ptr, size); 
        if(bytes_read <= 0)
            goto ERR;
        size -= bytes_read;
        ptr += bytes_read;
    }

    if(res[orig_size - 1] == '\n')
        res[orig_size] = '\0';
    else
    {
        res[orig_size] = '\n';
        res[orig_size + 1] = '\0';
    }

    return res;
ERR:
    if(fd != -1)
        close(fd);
    sic_fatal_error("Failed to open file \'%s\'", file->full_path);
    return NULL;
}

FILE* sifile_open_write(const SIFile* file)
{
    SIC_ASSERT(file != NULL);
    SIC_ASSERT(file->full_path != NULL);
    FILE* fp = fopen(file->full_path, "w");
    if(!fp)
        sic_fatal_error("Unable to open/create output file \'%s\'.", file->full_path);
    return fp;
}

SIFile convert_file_to(const SIFile* file, FileType desired)
{
    SIC_ASSERT(file != NULL);
    SIC_ASSERT(file->full_path != NULL);
    SIC_ASSERT(desired != FT_UNKNOWN);

    size_t filename_len = (uintptr_t)file->file_ext - (uintptr_t)file->full_path;

    const char* ext = ft_to_extension(desired);
    size_t ext_len = strlen(ext);
    char* new_name = cmalloc(filename_len + ext_len + 1);
    memcpy(new_name, file->full_path, filename_len);
    strncpy(new_name + filename_len, ext, ext_len);
    new_name[filename_len + ext_len] = '\0';

    SIFile new_file;
    new_file.full_path = new_name;
    new_file.file_name = new_name + ((uintptr_t)file->file_name - (uintptr_t)file->full_path);
    new_file.file_ext  = new_name + filename_len;
    new_file.type = desired;
    return new_file;
}

const char* convert_ext_to(const char* path, FileType desired)
{
    SIC_ASSERT(path != NULL);
    SIC_ASSERT(desired != FT_UNKNOWN);

    SIFile temp = sifile_new(path);
    temp = convert_file_to(&temp, desired);
    return temp.full_path;
}

static FileType get_filetype(const char* extension)
{
    SIC_ASSERT(extension != NULL);
    if(strcmp(extension, "ll") == 0)
        return FT_LLVM_IR;
    if(strcmp(extension, "s") == 0 ||
       strcmp(extension, "asm") == 0)
        return FT_ASM;
    if(strcmp(extension, "o") == 0)
        return FT_OBJ;
    if(strcmp(extension, "a") == 0)
        return FT_STATIC;
    if(strcmp(extension, "so") == 0)
        return FT_SHARED;
    if(strcmp(extension, "si") == 0)
        return FT_SI;

    return FT_UNKNOWN;
}

const char* ft_to_extension(FileType ft)
{
    static const char* ft_to_ext[] = {
        NULL, 
        [FT_SI]      = ".si",
        [FT_LLVM_IR] = ".ll",
        [FT_ASM]     = ".s", 
        [FT_OBJ]     = ".o", 
        [FT_STATIC]  = ".a", 
        [FT_SHARED]  = ".so", 
    };
    return ft_to_ext[ft];
}

SIFile create_tempfile(FileType ft)
{
    SIC_ASSERT(ft != FT_UNKNOWN);
    static const char template[21] = "/tmp/siliconc-XXXXXX";
    const char* ext = ft_to_extension(ft);
    int ext_len = strlen(ext);
    char* tmppath = cmalloc(sizeof(template) + ext_len);
    memcpy(tmppath, template, sizeof(template) - 1);
    memcpy(tmppath + sizeof(template) - 1, ext, ext_len + 1);

    int fd = mkstemps(tmppath, ext_len);
    if(fd == -1)
        sic_fatal_error("Failed to create temporary file.");

    close(fd);
    da_append(&s_tempfiles, tmppath);

    SIFile res;
    res.full_path = tmppath;
    res.file_name = tmppath + 5;
    res.file_ext = tmppath + sizeof(template);
    res.type = ft;
    return res;
}

void close_tempfiles(void)
{
    for(size_t i = 0; i < s_tempfiles.size; ++i)
        unlink(s_tempfiles.data[i]);
}
