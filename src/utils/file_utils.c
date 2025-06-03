#include "file_utils.h"
#include "error.h"
#include "core/core.h"

#include <fcntl.h>
#include <unistd.h>
#include <sys/stat.h>

#include <string.h>

#define FILE_FOUND 0

bool file_exists(const char* path)
{
    SIC_ASSERT(path != NULL);
    struct stat st;
    return stat(path, &st) == FILE_FOUND;
}

char* read_entire_file(const char* path)
{
    SIC_ASSERT(path != NULL);

    char* res = NULL;
    int fd = open(path, O_RDONLY);

    if(fd == -1)
        return NULL;

    long size = lseek(fd, 0, SEEK_END);
    if(size <= 0)
        goto end;

    if(lseek(fd, 0, SEEK_SET) < 0)
        goto end;

    res = malloc(sizeof(char) * size + 2);
    if(res == NULL)
        goto end;
    ssize_t total_read = 0;

    while(true)
    {
        ssize_t bytes_read = read(fd, res + total_read, size); 
        if(bytes_read == 0)
            break;
        if(bytes_read < 0)
        {
            free(res);
            res = NULL;
            goto end;
        }
        total_read += bytes_read;
    }

    if(total_read != size)
    {
        free(res);
        res = NULL;
    }
    else if(res[size - 1] == '\n')
        res[size] = '\0';
    else
    {
        res[size] = '\n';
        res[size + 1] = '\0';
    }

end:
    close(fd);
    return res;
}

FILE* open_out_file(const char* path)
{
    SIC_ASSERT(path != NULL);
    FILE* fp = fopen(path, "w");
    if(!fp)
        sic_error_fatal("Unable to open/create output file \'%s\'.", path);
    return fp;
}

char* convert_ext_to(const char* orig_path, FileType desired)
{
    
    SIC_ASSERT(orig_path != NULL);
    SIC_ASSERT(desired != FT_NONE);
    const char* filename_start = strrchr(orig_path, '/');
    if(filename_start == NULL)
        filename_start = orig_path;
    else
        filename_start++;

    const char* ext_start = strrchr(filename_start, '.');
    size_t filename_len;
    if(ext_start == NULL)
        filename_len = strlen(filename_start);
    else
        filename_len = ext_start - filename_start;

    const char* ext = ft_to_extension(desired);
    size_t ext_len = strlen(ext);
    char* new_name = malloc(filename_len + ext_len + 1);
    strncpy(new_name, orig_path, filename_len);
    strncpy(new_name + filename_len, ext, ext_len);
    new_name[filename_len + ext_len] = '\0';
    return new_name;
}

FileType get_filetype(const char* filename)
{
    const char* ext = strrchr(filename, '.');
    if(ext == NULL)
        return FT_EXEC;
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

    sic_error_fatal("Unrecognized file extension: '%s'", ext);
    return FT_NONE;
}

const char* ft_to_extension(FileType ft)
{
    static const char* ft_to_ext[] = {
        NULL, 
        [FT_EXEC]   = "", 
        [FT_ASM]    = ".s", 
        [FT_OBJ]    = ".o", 
        [FT_STATIC] = ".a", 
        [FT_SHARED] = ".so", 
        [FT_SI]     = ".si"
    };
    return ft_to_ext[ft];
}
