#pragma once
#include <stdarg.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "core/structs.h"

#define MIN(x, y) ((x) < (y) ? (x) : (y))
#define MAX(x, y) ((x) > (y) ? (x) : (y))
#define ALIGN_UP(x, align) (((x) + (align) - 1) & ~((align) - 1))
#define SCRATCH_SIZE (1 << 14)
#ifdef __GNUC__
    #define UNUSED __attribute__((unused))
    #define FALLTHROUGH __attribute__((fallthrough))
    #define NORETURN __attribute__((noreturn))
    #define PRINTF_FMT(fmt, va_args) __attribute__((format(printf, fmt, va_args)))
#else
    #define UNUSED
    #define FALLTHROUGH
    #define NORETURN
    #define PRINTF_FMT(fmt, va_args)
#endif

#ifdef SI_DEBUG
    #include <signal.h>
    #define SIC_ERROR_DBG(msg)                      \
        do                                          \
        {                                           \
            fprintf(stderr, "\033[31m[DEBUG]: ");   \
            fprintf(stderr, msg);                   \
            fprintf(stderr, "\033[0m\n");           \
            raise(SIGTRAP);                         \
        } while(0)
    #define SIC_ERROR_DBG_ARGS(msg, ...)        \
        do                                      \
        {                                       \
            fprintf(stderr, "\033[31m");        \
            fprintf(stderr, msg, __VA_ARGS__);  \
            fprintf(stderr, "\033[0m\n");       \
            raise(SIGTRAP);                     \
        } while(0)
    #define SIC_ASSERT(cond)                                        \
        do                                                          \
        {                                                           \
            if(!(cond))                                             \
                SIC_ERROR_DBG_ARGS("Assertion failed (%s:%d): %s",  \
                                   __FILE__, __LINE__, #cond);      \
        } while(0)
    #define SIC_ASSERT_MSG(cond, msg)   \
        do                              \
        {                               \
            if(!(cond))                 \
                SIC_ERROR_DBG(msg);     \
        } while(0)
    #define SIC_ASSERT_ARGS(cond, msg, ...)             \
        do                                              \
        {                                               \
            if(!(cond))                                 \
                SIC_ERROR_DBG_ARGS(msg, __VA_ARGS__);   \
        } while(0)
#else
    #define SIC_ERROR_DBG(msg)
    #define SIC_ERROR_DBG_ARGS(msg, ...)
    #define SIC_ASSERT(cond)
    #define SIC_ASSERT_MSG(cond, msg)
    #define SIC_ASSERT_ARGS(cond, msg, ...)
#endif

struct ScratchBuffer
{
    char   data[SCRATCH_SIZE];
    size_t len;
};

// hash.c - Hashmap functions
void  hashmap_reserve(HashMap* map, uint32_t entry_cnt);
void  hashmap_put(HashMap* map, Symbol key, void* val);
void* hashmap_get(HashMap* map, Symbol key);
void  hashmap_clear(HashMap* map);

// arena.c - Arena functions
void   arena_init(MemArena* arena, size_t capacity);
void*  arena_alloc(MemArena* arena, size_t size, uint32_t align);
void   global_arenas_init(void);
void*  global_arena_malloc(size_t size, uint32_t align);
void*  global_arena_calloc(size_t nmemb, size_t size, uint32_t align);
size_t global_arena_allocated();

// error.c - Error functions
extern int g_error_cnt;
extern int g_warning_cnt;

PRINTF_FMT(2, 3)
void sic_diagnostic(DiagnosticType diag, const char* restrict message, ...);
PRINTF_FMT(3, 4)
void sic_diagnostic_at(SourceLoc loc, DiagnosticType diag, const char* restrict message, ...);
void sic_diagnosticv(DiagnosticType diag, const char* restrict message, va_list va);
void sic_diagnostic_atv(SourceLoc loc, DiagnosticType diag, const char* restrict message, va_list va);
void sic_error_redef(Object* redef, Object* orig);

PRINTF_FMT(2, 3)
static inline void sic_error_at(SourceLoc loc, const char* restrict message, ...)
{
    va_list va;
    va_start(va, message);
    sic_diagnostic_atv(loc, DIAG_ERROR, message, va);
    va_end(va);
}

PRINTF_FMT(1, 2) NORETURN
static inline void sic_fatal_error(const char* restrict message, ...)
{
    va_list va;
    va_start(va, message);
    sic_diagnosticv(DIAG_FATAL, message, va);
    va_end(va);
    SIC_ERROR_DBG("Fatal Error Triggered Debug Break.");
    exit(EXIT_FAILURE);
}

#define SIC_UNREACHABLE() sic_fatal_error("Compiler encountered an unexpected error, should be unreachable. %s:%d(%s)", __FILE__, __LINE__, __FUNCTION__)
#define SIC_TODO()        sic_fatal_error("\033[32mTODO: %s:%d(%s)\033[0m Not yet implemented.", __FILE__, __LINE__, __FUNCTION__)
#define SIC_TODO_MSG(msg) sic_fatal_error("\033[32mTODO: %s:%d(%s)\033[0m %s", __FILE__, __LINE__, __FUNCTION__, msg)


// string.c - Scratch Buffer and String format

extern ScratchBuffer g_scratch;

PRINTF_FMT(1, 2)
void scratch_appendf(const char* restrict fmt, ...);
PRINTF_FMT(1, 2)
char* str_format(const char* restrict fmt, ...);
char* str_dupn(const char* str, size_t len);
static inline char* str_dup(const char* str) { return str_dupn(str, strlen(str)); }

static inline void scratch_appendc(char c) 
{
    if(g_scratch.len >= SCRATCH_SIZE)
        sic_fatal_error("Ran out of space in the scratch buffer. This shouldn't happen.");
    g_scratch.data[g_scratch.len] = c;
    g_scratch.len++;
}
static inline void scratch_appendn(const char* str, size_t len)
{
    if(len + g_scratch.len > SCRATCH_SIZE)
        sic_fatal_error("Ran out of space in the scratch buffer. This shouldn't happen.");
    memcpy(g_scratch.data + g_scratch.len, str, len);
    g_scratch.len += len;
}

static inline void scratch_clear()
{ 
    g_scratch.len = 0;
}

static inline void scratch_append(const char* str)
{
    scratch_appendn(str, strlen(str));
}
static inline const char* scratch_string(void)
{
    g_scratch.data[g_scratch.len] = '\0';
    return g_scratch.data;
}

static inline void* cmalloc(size_t size)
{
    void* res = malloc(size);
    if(res == NULL)
        sic_fatal_error("Failed to cmalloc %zu bytes.", size);
    return res;
}

static inline void* ccalloc(size_t nmemb, size_t size)
{
    void* res = calloc(nmemb, size);
    if(res == NULL)
        sic_fatal_error("Failed to ccalloc %zu bytes.", size);
    return res;
}

static inline void* crealloc(void* ptr, size_t size)
{
    SIC_ASSERT(ptr != NULL);
    void* res = realloc(ptr, size);
    if(res == NULL)
        sic_fatal_error("Failed to ccalloc %zu bytes.", size);
    return res;
}

#ifdef SIC_CMALLOC_ONLY
#define MALLOC(size)                cmalloc(size)
#define CALLOC(nmemb, size)         ccalloc(nmemb, size)
#define MALLOC_STRUCT(type)         cmalloc(sizeof(type))
#define CALLOC_STRUCT(type)         ccalloc(1, sizeof(type))
#define MALLOC_STRUCTS(type, count) cmalloc(sizeof(type) * (count))
#define CALLOC_STRUCTS(type, count) ccalloc(count, sizeof(type))
#else
#define MALLOC(size, align)         global_arena_malloc(size, align)
#define CALLOC(nmemb, size, align)  global_arena_calloc(nmemb, size, align)
#define MALLOC_STRUCT(type)         global_arena_malloc(sizeof(type), _Alignof(type))
#define CALLOC_STRUCT(type)         global_arena_calloc(1, sizeof(type), _Alignof(type))
#define MALLOC_STRUCTS(type, count) global_arena_malloc(sizeof(type) * (count), _Alignof(type))
#define CALLOC_STRUCTS(type, count) global_arena_calloc(count, sizeof(type), _Alignof(type))
#endif

#define FNV_SEED   0xCBF29CE484222325
#define FNV_FACTOR 0x100000001B3

static inline uint64_t fnv_hash(const char* str, size_t len)
{
    uint64_t hash = FNV_SEED;
    for(size_t i = 0; i < len; ++i)
        hash = ((uint8_t)str[i] ^ hash) * FNV_FACTOR;
    return hash;
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

static inline bool c_is_undnum(char c)
{
    return c_is_num(c) || c == '_';
}

static inline bool c_is_alphanum(char c)
{
    return c_is_alpha(c) || c_is_num(c);
}

static inline bool c_is_undalphanum(char c)
{
    return c_is_alphanum(c) || c == '_';
}

