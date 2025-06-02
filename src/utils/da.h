#pragma once
#include "core/core.h"

#include <stdarg.h>
#include <stddef.h>
#include <stdint.h>
#include <string.h>

#define DA_MIN_CAPACITY 16
#define DA_ASSERT(cond) SIC_ASSERT(cond)
#define DA_ENSURE(cond) { if(!(cond)) exit(1); }

#define da_init(da, initial_cap)                                \
    {                                                           \
        (da)->capacity = (initial_cap) > DA_MIN_CAPACITY?       \
                         (initial_cap) : DA_MIN_CAPACITY;       \
        (da)->size = 0;                                         \
        (da)->data = malloc((da)->capacity *                    \
                            sizeof(*((da)->data)));             \
        DA_ENSURE((da)->data != NULL);                          \
    }

#define da_reserve(da, desired_cap)                                         \
    {                                                                       \
        if((da)->capacity < (desired_cap))                                  \
        {                                                                   \
            (da)->capacity = (desired_cap) + ((desired_cap) >> 1);          \
            (da)->data = realloc((da)->data,                                \
                                 (da)->capacity * sizeof(*((da)->data)));   \
            DA_ENSURE((da)->data != NULL);                                  \
        }                                                                   \
    }

#define da_append(da, item)                 \
    {                                       \
        DA_ASSERT((da) != NULL);            \
        da_reserve((da), (da)->size + 1);   \
        (da)->data[(da)->size++] = (item);  \
    }

#define da_append_arr(da, item_arr, item_count)         \
    {                                                   \
        DA_ASSERT((da) != NULL);                        \
        DA_ASSERT((item_arr) != NULL);                  \
        DA_ASSERT((item_count) > 0);                    \
        da_reserve((da), (da)->size + (item_count));    \
        memcpy((da)->data + (da)->size, (item_arr),     \
               (item_count) * sizeof(*((da)->data)));   \
        (da)->size += (item_count);                     \
    }

#define da_insert(da, item, index)                                          \
    {                                                                       \
        DA_ASSERT((da) != NULL);                                            \
        DA_ASSERT((index) <= (da)->size);                                   \
        da_reserve((da), (da)->size + 1);                                   \
        for(size_t __da_idx = (index); __da_idx < (da)->size; ++__da_idx)   \
            (da)->data[__da_idx + 1] = (da)->data[__da_idx];                \
        (da)->data[(index)] = (item);                                       \
        (da)->size++;                                                       \
    }

#define da_insert_arr(da, item_arr, item_count, index)                      \
    {                                                                       \
        DA_ASSERT((da) != NULL);                                            \
        DA_ASSERT((item_arr) != NULL);                                      \
        DA_ASSERT((item_count) > 0);                                        \
        DA_ASSERT((index) <= (da)->size);                                   \
        da_reserve((da), (da)->size + (item_count));                        \
        for(size_t __da_idx = (index); __da_idx < (da)->size; ++__da_idx)   \
            (da)->data[__da_idx + (item_count)] = (da)->data[__da_idx];     \
        memcpy((da)->data + (index), (item_arr),                            \
               (item_count) * sizeof(*((da)->data)));                       \
        (da)->size += (item_count);                                         \
    }

#define da_resize(da, new_size)         \
    {                                   \
        da_reserve((da), (new_size));   \
        (da)->size = (new_size);        \
    }

#define da_free_data(da) { free((da)->data); }



typedef struct StringBuilder StringBuilder;
struct StringBuilder
{
    char*  data;
    size_t size;
    size_t capacity;
};

typedef struct StringArray StringArray;
struct StringArray
{
    char** data;
    size_t size;
    size_t capacity;
};

static inline void sb_append(StringBuilder* sb, const char* str)
{
    size_t len = strlen(str);
    da_append_arr(sb, str, len);
}

static inline void sb_append_null(StringBuilder* sb)
{
    da_append(sb, '\0');
}

__attribute__((format(printf, 2, 3)))
void sb_appendf(StringBuilder* sb, const char* restrict format, ...);
