#define _POSIX_C_SOURCE 200809L
#define _DEFAULT_SOURCE
#include "file_utils.h"
#include "da.h"
#include "core/structs.h"

#include <fcntl.h>
#include <unistd.h>
#include <sys/stat.h>

#include <errno.h>
#include <limits.h>
#include <stdlib.h>
#include <string.h>

#define FILE_FOUND 0

typedef struct
{
    char ext[4];
    int  len;
} FTStorage;

static FTStorage s_ft_to_ext[] = {
    [FT_UNKNOWN] = { "\0" , 0 },
    [FT_SI]      = { ".si", 3 },
    [FT_LLVM_IR] = { ".ll", 3 },
    [FT_ASM]     = { ".s" , 2 }, 
    [FT_OBJ]     = { ".o" , 2 }, 
    [FT_STATIC]  = { ".a" , 2 }, 
    [FT_SHARED]  = { ".so", 3 }, 
};

static char s_cwd[PATH_MAX];
static StringDA s_tempfiles = {0};

static inline const char* ft_to_extension(FileType ft, size_t* len);
static void get_name_and_ext(const char* path, const char** name, 
                             const char** ext, const char** end);
static const char* normalize_rel_path(const char* abs_path);

void get_current_dir()
{
    if(getcwd(s_cwd, PATH_MAX) == NULL)
        sic_fatal_error("Failed to get current working directory.");
}

FileId source_file_add_or_get(const char* path, ObjModule* module)
{
    SIC_ASSERT(path != NULL);

    bool is_abs = path[0] == '/';

    char abs_path[PATH_MAX];
    if(realpath(path, abs_path) == NULL)
        sic_fatal_error("Failed to resolve path '%s'. (Errno %d: %s)", path, errno, strerror(errno));

    for(uint32_t i = 0; i < g_compiler.sources.size; ++i)
    {
        SourceFile* file = g_compiler.sources.data + i;
        if(strcmp(abs_path, file->abs_path) == 0)
            return i;
    }

    da_reserve(&g_compiler.sources, g_compiler.sources.size + 1);
    FileId id = g_compiler.sources.size++;
    SourceFile* file = g_compiler.sources.data + id;
    file->abs_path = str_dup(abs_path);
    file->rel_path = is_abs ? file->abs_path : normalize_rel_path(file->abs_path);
    file->module   = module;

    int fd = open(abs_path, O_RDONLY);

    if(fd == -1)
        goto ERR;

    long size = lseek(fd, 0, SEEK_END);

    if(size < 0 || lseek(fd, 0, SEEK_SET) < 0)
        goto ERR;

    char* buf = MALLOC(size + 2, sizeof(char));
    file->src = buf;
    long total_read = 0;
    while(total_read < size)
    {
        long bytes_read = read(fd, buf + total_read, size); 
        if(bytes_read <= 0)
            goto ERR;
        total_read += bytes_read;
    }

    if(buf[size - 1] == '\n')
        buf[size] = '\0';
    else
    {
        buf[size] = '\n';
        buf[size + 1] = '\0';
    }

    return id + 1;
ERR:
    if(fd != -1)
        close(fd);
    sic_fatal_error("Failed to read source file \'%s\'", path);
}

FileId find_and_open_module_path(ObjModule* module)
{
    // FIXME: Check overflow of buffer.
    const char* base_path = file_from_id(g_compiler.input_file)->rel_path;
    char path_one[PATH_MAX];
    char path_two[PATH_MAX];
    size_t len = 0;
    for(size_t i = 0; base_path[i] != '\0'; ++i)
        if(base_path[i] == '/')
            len = i + 1;
    memcpy(path_one, base_path, len);
    ObjModuleDA stack = {0};
    ObjModule* temp = module;
    while(temp->parent != NULL)
    {
        da_append(&stack, temp);
        temp = temp->parent;
    }

    for(uint32_t i = stack.size - 1; i < stack.size; --i)
    {
        size_t next_len = strlen(stack.data[i]->header.symbol);
        memcpy(path_one + len, stack.data[i]->header.symbol, next_len);
        len += next_len;
        path_one[len++] = '/';
    }
    FREE(stack.data, stack.capacity * sizeof(ObjModule*));
    memcpy(path_two, path_one, len);
    strcpy(path_two + len - 1, ".si");
    strcpy(path_one + len, "mod.si");
    if(file_exists(path_one))
    {
        if(file_exists(path_two))
        {
            sic_error_at(module->header.loc, "Module %s has conflicting paths \'%s\' and \'%s\'. Please remove one.", 
                         module->header.symbol, path_one, path_two);
            return FILE_NULL;
        }
        return source_file_add_or_get(path_one, module);
    }

    if(!file_exists(path_two))
    {
        sic_error_at(module->header.loc, "Module %s was not found at path \'%s\' or \'%s\'. Please add one of those files.", 
                     module->header.symbol, path_one, path_two);
        return FILE_NULL;
    }
    return source_file_add_or_get(path_two, module);
}

const char* convert_ext_to(const char* path, FileType desired)
{
    SIC_ASSERT(path != NULL);
    SIC_ASSERT(desired <= FT_SHARED);
    size_t ext_len;
    const char* new_ext = ft_to_extension(desired, &ext_len);
    const char* file_name;
    const char* file_ext;
    const char* file_end;
    get_name_and_ext(path, &file_name, &file_ext, &file_end);
    if(file_ext <= file_name)
        file_ext = file_end;

    size_t path_len = (uintptr_t)file_ext - (uintptr_t)path;
    char* new_name = cmalloc(path_len + ext_len + 1);

    memcpy(new_name, path, path_len);
    strncpy(new_name + path_len, new_ext, ext_len);
    new_name[path_len + ext_len] = '\0';
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
    for(uint32_t i = 0; i < s_tempfiles.size; ++i)
        unlink(s_tempfiles.data[i]);
}

FileType get_filetype(const char* path)
{
    SIC_ASSERT(path != NULL);
    const char* name;
    const char* ext;
    const char* end;
    get_name_and_ext(path, &name, &ext, &end); 
    int ext_len = (uintptr_t)end - (uintptr_t)ext;
    for(FileType type = FT_UNKNOWN; type <= FT_SHARED; type++)
    {
        FTStorage* e = s_ft_to_ext + type;
        if(ext_len == e->len && memcmp(ext, e->ext, e->len) == 0)
            return type;
    }
    return FT_UNKNOWN;
}

static inline const char* ft_to_extension(FileType ft, size_t* len)
{
    SIC_ASSERT(ft <= FT_SHARED);
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

static const char* normalize_rel_path(const char* abs_path)
{
    char temp[PATH_MAX];
    const char* abs_last = abs_path;
    const char* cwd_last = s_cwd;
    const char* cwd_cur  = s_cwd;
    while(true)
    {
        char cwd = *cwd_cur;
        char abs = *abs_path;
        if(cwd == '\0')
        {
            SIC_ASSERT(abs != '\0');
            if(abs == '/')
                return abs_path + 1;
            break;
        }
        if(abs == '\0')
        {
            if(cwd == '/')
            {
                abs_last = abs_path;
                cwd_last = cwd_cur;
            }
            break;
        }
        if(abs != cwd)
            break;
        if(cwd == '/')
        {
            abs_last = abs_path + 1;
            cwd_last = cwd_cur;
        }
        cwd_cur++;
        abs_path++;
    }
    
    char* t = temp;
    while(*cwd_last != '\0')
    {
        if(*cwd_last == '/')
        {
            t[0] = '.';
            t[1] = '.';
            t[2] = '/';
            t += 3;
        }
        cwd_last++;
    }
    strcpy(t, abs_last);
    return str_dup(temp);
}
