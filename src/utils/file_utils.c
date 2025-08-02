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

typedef struct
{
    char   ext[3];
    size_t len;
} FTStorage;

static FTStorage s_ft_to_ext[] = {
    [FT_UNKNOWN] = { "\0", 0 },
    [FT_SI]      = { "si", 2 },
    [FT_LLVM_IR] = { "ll", 2 },
    [FT_ASM]     = { "s" , 1 }, 
    [FT_OBJ]     = { "o" , 1 }, 
    [FT_STATIC]  = { "a" , 1 }, 
    [FT_SHARED]  = { "so", 2 }, 
};

static StringDA s_tempfiles = {0};

static FileType get_filetype(const char* extension);
static inline const char* ft_to_extension(FileType ft, size_t* len);
static void get_name_and_ext(const char* path, const char** name, 
                                 const char** ext, const char** end);

void input_file_new(const char* path)
{
    SIC_ASSERT(path != NULL);
    da_reserve(&g_args.input_files, g_args.input_files.size + 1);
    InputFile* file = g_args.input_files.data + g_args.input_files.size;
    file->path = path;
    file->src  = NULL;
    if(*path == '\0')
        sic_fatal_error("Tried to use file with empty path.");
    
    const char* file_name;
    const char* file_ext;
    get_name_and_ext(path, &file_name, &file_ext, NULL);
    file->type = file_ext <= file_name ? FT_UNKNOWN : get_filetype(file_ext + 1);
    file->id = g_args.input_files.size;
    g_args.input_files.size++;
}

void input_file_read(InputFile* file)
{
    SIC_ASSERT(file != NULL);
    SIC_ASSERT(file->path != NULL);

    char* res = NULL;
    int fd = open(file->path, O_RDONLY);

    if(fd == -1)
        goto ERR;

    long size = lseek(fd, 0, SEEK_END);
    if(size < 0)
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

    file->src = res;
    return;
ERR:
    if(fd != -1)
        close(fd);
    sic_fatal_error("Failed to open file \'%s\'", file->path);
}

const char* convert_ext_to(const char* path, FileType desired)
{
    SIC_ASSERT(path != NULL);
    size_t ext_len;
    const char* new_ext = ft_to_extension(desired, &ext_len);
    const char* file_name;
    const char* file_ext;
    const char* file_end;
    get_name_and_ext(path, &file_name, &file_ext, &file_end);
    if(file_ext <= file_name)
        file_ext = file_end;

    size_t path_len = (uintptr_t)file_ext - (uintptr_t)path;
    char* new_name = cmalloc(path_len + ext_len + 2);

    memcpy(new_name, path, path_len);
    new_name[path_len] = '.';
    strncpy(new_name + path_len + 1, new_ext, ext_len);
    new_name[path_len + ext_len + 1] = '\0';
    return new_name;
}

bool file_exists(const char* path)
{
    SIC_ASSERT(path != NULL);
    struct stat st;
    return stat(path, &st) == FILE_FOUND;
}

const char* create_tempfile(FileType ft)
{
    SIC_ASSERT(ft != FT_UNKNOWN);
    static const char template[21] = "/tmp/siliconc-XXXXXX";
    size_t ext_len;
    const char* ext = ft_to_extension(ft, &ext_len);
    char* tmppath = cmalloc(sizeof(template) + ext_len);
    memcpy(tmppath, template, sizeof(template) - 1);
    memcpy(tmppath + sizeof(template) - 1, ext, ext_len + 1);

    int fd = mkstemps(tmppath, (int)ext_len);
    if(fd == -1)
        sic_fatal_error("Failed to create temporary file.");

    close(fd);
    da_append(&s_tempfiles, tmppath);
    return tmppath;
}

void close_tempfiles(void)
{
    for(size_t i = 0; i < s_tempfiles.size; ++i)
        unlink(s_tempfiles.data[i]);
}

static FileType get_filetype(const char* extension)
{
    SIC_ASSERT(extension != NULL);
    for(size_t i = 1; i < sizeof(s_ft_to_ext) / sizeof(s_ft_to_ext[0]); ++i)
        if(memcmp(extension, s_ft_to_ext[i].ext, s_ft_to_ext[i].len) == 0)
            return (FileType)i;
    return FT_UNKNOWN;
}

static inline const char* ft_to_extension(FileType ft, size_t* len)
{
    SIC_ASSERT(ft > FT_UNKNOWN && ft <= FT_SHARED);
    *len = s_ft_to_ext[ft].len;
    return s_ft_to_ext[ft].ext;
}

static void get_name_and_ext(const char* path, const char** name, 
                             const char** ext, const char** end)
{
    const char* file_name = path;
    const char* file_ext = NULL;
    const char* file_end;
    for(file_end = path; *file_end != '\0'; ++file_end)
    {
        char c = file_end[0];
        if(c == '/')
        {
            if(file_end[1] == '\0')
                sic_fatal_error("File path ends with /. (Indicates directory)");
            file_name = file_end + 1;
        }
        else if(c == '.')
            file_ext = file_end;
    }

    *name = file_name;
    *ext = file_ext;
    if(end != NULL)
        *end = file_end;
}
