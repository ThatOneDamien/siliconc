#define _POSIX_C_SOURCE 200809L
#define _DEFAULT_SOURCE
#include "file_utils.h"
#include "da.h"
#include "lib.h"
#include "core/core.h"

#include <fcntl.h>
#include <unistd.h>
#include <sys/stat.h>

#include <stdlib.h>
#include <string.h>

#define FILE_FOUND 0

static StringArray s_tempfiles = {0};

SIFile sifile_new(const char* path)
{
    SIC_ASSERT(path != NULL);
    SIFile file;
    file.full_path = path;
    file.file_name = path;
    if(*path == '\0')
        sic_error_fatal("Tried to use file with empty path.");
    
    for(const char* s = path; *s != '\0'; ++s)
        if(*s == '/')
        {
            if(s[1] == '\0')
                sic_error_fatal("File path ends with /. (Indicates directory)");
            file.file_name = s + 1;
        }

    file.type = get_filetype(file.file_name);
    return file;
}

bool file_exists(const char* path)
{
    SIC_ASSERT(path != NULL);
    struct stat st;
    return stat(path, &st) == FILE_FOUND;
}

char* read_entire_file(const SIFile* file)
{
    SIC_ASSERT(file != NULL);
    SIC_ASSERT(file->full_path != NULL);

    char* res = NULL;
    int fd = open(file->full_path, O_RDONLY);

    if(fd == -1)
        goto end;

    long size = lseek(fd, 0, SEEK_END);
    if(size <= 0)
        goto end;

    if(lseek(fd, 0, SEEK_SET) < 0)
        goto end;

    res = cmalloc(sizeof(char) * size + 2);
    ssize_t total_read = 0;

    while(true)
    {
        ssize_t bytes_read = read(fd, res + total_read, size); 
        if(bytes_read == 0)
            break;
        if(bytes_read < 0)
            goto end;
        total_read += bytes_read;
    }

    if(total_read != size)
        goto end;
    else if(res[size - 1] == '\n')
        res[size] = '\0';
    else
    {
        res[size] = '\n';
        res[size + 1] = '\0';
    }

    return res;
end:
    if(fd != -1)
        close(fd);
    sic_error_fatal("Failed to open file \'%s\'", file->full_path);
    return NULL;
}

FILE* open_out_file(const SIFile* file)
{
    SIC_ASSERT(file != NULL);
    SIC_ASSERT(file->full_path != NULL);
    FILE* fp = fopen(file->full_path, "w");
    if(!fp)
        sic_error_fatal("Unable to open/create output file \'%s\'.", file->full_path);
    return fp;
}

SIFile convert_ext_to(const SIFile* file, FileType desired)
{
    SIC_ASSERT(file != NULL);
    SIC_ASSERT(file->full_path != NULL);
    SIC_ASSERT(desired != FT_UNKNOWN);

    size_t filename_len = 0;
    for(const char* s = file->file_name; *s != '\0' && *s != '.'; ++s)
        filename_len++;

    const char* ext = ft_to_extension(desired);
    size_t ext_len = strlen(ext);
    char* new_name = malloc(filename_len + ext_len + 1);
    memcpy(new_name, file->file_name, filename_len);
    strncpy(new_name + filename_len, ext, ext_len);
    new_name[filename_len + ext_len] = '\0';

    SIFile new_file;
    new_file.full_path = new_name;
    new_file.file_name = new_file.full_path;
    new_file.type = desired;
    return new_file;
}

FileType get_filetype(const char* filename)
{
    const char* ext = strrchr(filename, '.');
    if(ext == NULL)
        return FT_UNKNOWN;
    if(strcmp(ext, ".s") == 0 ||
       strcmp(ext, ".asm") == 0)
        return FT_ASM;
    if(strcmp(ext, ".o") == 0)
        return FT_OBJ;
    if(strcmp(ext, ".a") == 0)
        return FT_STATIC;
    if(strcmp(ext, ".so") == 0)
        return FT_SHARED;
    if(strcmp(ext, ".si") == 0)
        return FT_SI;

    return FT_UNKNOWN;
}

const char* ft_to_extension(FileType ft)
{
    static const char* ft_to_ext[] = {
        NULL, 
        [FT_SI]     = ".si",
        [FT_ASM]    = ".s", 
        [FT_OBJ]    = ".o", 
        [FT_STATIC] = ".a", 
        [FT_SHARED] = ".so", 
    };
    return ft_to_ext[ft];
}

SIFile create_tempfile(FileType ft)
{
    if(s_tempfiles.capacity == 0)
        da_init(&s_tempfiles, 0);
    static const char template[21] = "/tmp/siliconc-XXXXXX";
    const char* ext = ft_to_extension(ft);
    int ext_len = strlen(ext);
    char* tmppath = malloc(sizeof(template) + ext_len);
    memcpy(tmppath, template, sizeof(template) - 1);
    memcpy(tmppath + sizeof(template) - 1, ext, ext_len + 1);

    int fd = mkstemps(tmppath, ext_len);
    if(fd == -1)
        sic_error_fatal("Failed to create temporary file.");

    close(fd);
    da_append(&s_tempfiles, tmppath);

    SIFile res;
    res.full_path = tmppath;
    res.file_name = tmppath + 5;
    res.type = ft;
    return res;
}

void close_tempfiles(void)
{
    for(size_t i = 0; i < s_tempfiles.size; ++i)
        unlink(s_tempfiles.data[i]);
}
