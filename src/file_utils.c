#include "file_utils.h"
#include "core.h"

#include <fcntl.h>
#include <unistd.h>
#include <sys/stat.h>

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
