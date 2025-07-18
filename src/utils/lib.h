#pragma once
#include "core/core.h"
#include "utils/error.h"

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#define MIN(x, y) ((x) < (y) ? (x) : (y))
#define MAX(x, y) ((x) > (y) ? (x) : (y))
#define ALIGN_UP(x, align) (((x) + (align) - 1) & ~((align) - 1))
#define SCRATCH_SIZE (1 << 14)

typedef struct HashEntry     HashEntry;
typedef struct HashMap       HashMap;
typedef struct MemArena      MemArena;
typedef struct ScratchBuffer ScratchBuffer;

// TODO: Rename this to something like StringMap,
//       because I will also need a map for integral types
//       and it will be much more efficient to have separate
//       structs and functions than to have 1 general struct
struct HashEntry
{
    HashEntry*  next;
    const char* key;
    size_t      key_len;
    void*       value;
};

struct HashMap
{
    HashEntry* buckets;
    uint32_t   bucket_cnt;
    uint32_t   entry_cnt;
    uint32_t   max_load;
};

struct MemArena
{
    uint8_t* base;
    size_t   capacity;
    size_t   allocated;
};

struct ScratchBuffer
{
    char   data[SCRATCH_SIZE];
    size_t len;
};

void  hashmap_initn(HashMap* map, uint32_t entry_cnt);
void  hashmap_free(HashMap* map);
void  hashmap_putn(HashMap* map, const char* key, size_t len, void* val);
void* hashmap_getn(HashMap* map, const char* key, size_t len);
bool  hashmap_deleten(HashMap* map, const char* key, size_t len);
void  hashmap_clear(HashMap* map);

void  global_arenas_init(void);
void  arena_init(MemArena* arena, size_t capacity);
void* arena_alloc(MemArena* arena, size_t size, uint32_t align);
void* global_arena_malloc(size_t size, uint32_t align);
void* global_arena_calloc(size_t nmemb, size_t size, uint32_t align);

PRINTF_FMT(1, 2)
char* str_format(const char* restrict fmt, ...);

extern ScratchBuffer g_scratch;

static inline void scratch_clear() { g_scratch.len = 0; }
static inline void scratch_appendc(char c) 
{
    if(g_scratch.len >= SCRATCH_SIZE)
        sic_error_fatal("Ran out of space in the scratch buffer. This shouldn't happen.");
    g_scratch.data[g_scratch.len] = c;
    g_scratch.len++;
}
static inline void scratch_appendn(const char* str, size_t len)
{
    if(len + g_scratch.len > SCRATCH_SIZE)
        sic_error_fatal("Ran out of space in the scratch buffer. This shouldn't happen.");
    memcpy(g_scratch.data + g_scratch.len, str, len);
    g_scratch.len += len;
}
static inline void scratch_append(const char* str) { scratch_appendn(str, strlen(str)); }
static inline const char* scratch_string(void) { g_scratch.data[g_scratch.len] = '\0'; return g_scratch.data; }
PRINTF_FMT(1, 2)
void scratch_appendf(const char* restrict fmt, ...);

static inline void* cmalloc(size_t size)
{
    void* res = malloc(size);
    if(res == NULL)
        sic_error_fatal("Failed to cmalloc %zu bytes.", size);
    return res;
}

static inline void* ccalloc(size_t nmemb, size_t size)
{
    void* res = calloc(nmemb, size);
    if(res == NULL)
        sic_error_fatal("Failed to ccalloc %zu bytes.", size);
    return res;
}

static inline void* crealloc(void* ptr, size_t size)
{
    SIC_ASSERT(ptr != NULL);
    void* res = realloc(ptr, size);
    if(res == NULL)
        sic_error_fatal("Failed to ccalloc %zu bytes.", size);
    return res;
}

#ifdef SIC_CMALLOC_ONLY
#define MALLOC(size)        cmalloc(size)
#define CALLOC(nmemb, size) ccalloc(nmemb, size)
#else
#define MALLOC(size)        global_arena_malloc(size, 8)
#define CALLOC(nmemb, size) global_arena_calloc(nmemb, size, size)
#define MALLOC_STRUCT(type) global_arena_malloc(sizeof(type), _Alignof(type))
#define CALLOC_STRUCT(type) global_arena_calloc(1, sizeof(type), _Alignof(type))
#endif

static inline void hashmap_init(HashMap* map)
{
    hashmap_initn(map, 0);
}

static inline void hashmap_put(HashMap* map, const char* key, void* val)
{
    SIC_ASSERT(key != NULL);
    hashmap_putn(map, key, strlen(key), val); 
}

static inline void* hashmap_get(HashMap* map, const char* key)
{
    SIC_ASSERT(key != NULL);
    return hashmap_getn(map, key, strlen(key));
}

static inline bool hashmap_delete(HashMap* map, const char* key)
{
    SIC_ASSERT(key != NULL);
    return hashmap_deleten(map, key, strlen(key));
}

static inline uint32_t next_pow_of_2(uint32_t value)
{
    value--;
    value |= value >>  1;
    value |= value >>  2;
    value |= value >>  4;
    value |= value >>  8;
    value |= value >> 16;
    value++;
    return value;
}

static inline bool is_pow_of_2(uint32_t value)
{
    return value != 0 && (value & (value - 1)) == 0;
}

static inline bool c_is_upper(char c)
{
    return c >= 'A' && c <= 'Z';
}

static inline bool c_is_lower(char c)
{
    return c >= 'a' && c <= 'z';
}

static inline bool c_is_alpha(char c)
{
    return c_is_upper(c) || c_is_lower(c);
}

static inline bool c_is_num(char c)
{
    return c >= '0' && c <= '9';
}

static inline bool c_is_alphanum(char c)
{
    return c_is_alpha(c) || c_is_num(c);
}

static inline bool c_is_undalphanum(char c)
{
    return c_is_alphanum(c) || c == '_';
}

