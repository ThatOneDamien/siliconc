#pragma once
#include "lib.h"
#include "core/core.h"

#include <stdarg.h>
#include <stddef.h>
#include <stdint.h>
#include <string.h>

#define DA_MIN_CAPACITY 8
#define DA_ASSERT(cond) SIC_ASSERT(cond)

#define da_init(da, initial_cap)                                \
    do                                                          \
    {                                                           \
        (da)->capacity = (initial_cap) > DA_MIN_CAPACITY?       \
                         (initial_cap) : DA_MIN_CAPACITY;       \
        (da)->size = 0;                                         \
        (da)->data = MALLOC((da)->capacity *                    \
                            sizeof(*((da)->data)));             \
    } while(0)

#define da_reserve(da, desired_cap)                                         \
    do                                                                      \
    {                                                                       \
        if((da)->capacity < (desired_cap))                                  \
        {                                                                   \
            (da)->capacity = (desired_cap) << 1;                            \
            void* __new_da = MALLOC((da)->capacity *                        \
                                    sizeof(*((da)->data)));                 \
            memcpy(__new_da, (da)->data,                                    \
                   (da)->size * sizeof(*((da)->data)));                     \
            (da)->data = __new_da;                                          \
                                                                            \
        }                                                                   \
    } while(0)

#define da_append(da, item)                 \
    do                                      \
    {                                       \
        DA_ASSERT((da) != NULL);            \
        da_reserve((da), (da)->size + 1);   \
        (da)->data[(da)->size++] = (item);  \
    } while(0)

#define da_append_arr(da, item_arr, item_count)         \
    do                                                  \
    {                                                   \
        DA_ASSERT((da) != NULL);                        \
        DA_ASSERT((item_arr) != NULL);                  \
        DA_ASSERT((item_count) > 0);                    \
        da_reserve((da), (da)->size + (item_count));    \
        memcpy((da)->data + (da)->size, (item_arr),     \
               (item_count) * sizeof(*((da)->data)));   \
        (da)->size += (item_count);                     \
    } while(0)

#define da_insert(da, item, index)                                          \
    do                                                                      \
    {                                                                       \
        DA_ASSERT((da) != NULL);                                            \
        DA_ASSERT((index) <= (da)->size);                                   \
        da_reserve((da), (da)->size + 1);                                   \
        for(size_t __da_idx = (index); __da_idx < (da)->size; ++__da_idx)   \
            (da)->data[__da_idx + 1] = (da)->data[__da_idx];                \
        (da)->data[(index)] = (item);                                       \
        (da)->size++;                                                       \
    } while(0)

#define da_insert_arr(da, item_arr, item_count, index)                      \
    do                                                                      \
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
    } while(0)

#define da_resize(da, new_size)         \
    do                                  \
    {                                   \
        da_reserve((da), (new_size));   \
        (da)->size = (new_size);        \
    } while(0)

typedef struct StringArray StringArray;
struct StringArray
{
    char** data;
    size_t size;
    size_t capacity;
};
