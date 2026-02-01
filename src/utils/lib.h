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
    #define SIC_ERROR_DBG(...)                      \
        do                                          \
        {                                           \
            fprintf(stderr, "\033[31m[DEBUG]: ");   \
            fprintf(stderr, __VA_ARGS__);           \
            fprintf(stderr, "\033[0m\n");           \
            __asm__ volatile("int3");               \
        } while(0)
    #define SIC_ASSERT(cond)                                    \
        do                                                      \
        {                                                       \
            if(!(cond))                                         \
                SIC_ERROR_DBG("Assertion failed %s:%d(%s): %s", \
                              __FILE__, __LINE__, __FUNCTION__, \
                              #cond);                           \
        } while(0)
#else
    #define SIC_ERROR_DBG(...)
    #define SIC_ASSERT(cond) ((void)(cond))
#endif

struct ScratchBuffer
{
    char   data[SCRATCH_SIZE];
    size_t len;
};

// arith.c
#define INT128_MIN  ((Int128){ INT64_MIN, 0 })
#define INT128_MAX  ((Int128){ INT64_MAX, UINT64_MAX })
#define UINT128_MIN ((Int128){ 0, 0 })
#define UINT128_MAX ((Int128){ UINT64_MAX, UINT64_MAX })
Int128   i128_add(Int128 lhs, Int128 rhs);
Int128   i128_add64(Int128 lhs, uint64_t rhs);
Int128   i128_sub(Int128 lhs, Int128 rhs);
Int128   i128_sub64(Int128 lhs, uint64_t rhs);
Int128   i128_and(Int128 lhs, Int128 rhs);
Int128   i128_or(Int128 lhs, Int128 rhs);
Int128   i128_xor(Int128 lhs, Int128 rhs);
Int128   i128_neg(Int128 lhs);
Int128   i128_not(Int128 lhs);
Int128   i128_mult(Int128 lhs, Int128 rhs);
Int128   i128_mult64(Int128 lhs, uint64_t rhs);
char     *i128_to_string(Int128 op, uint64_t base, bool is_signed, bool use_prefix);
bool     i128_is_neg(Int128 op);
int      i128_ucmp(Int128 lhs, Int128 rhs);
int      i128_scmp(Int128 lhs, Int128 rhs);
int      i128_cmp(Int128 lhs, Int128 rhs, TypeKind kind);
Int128   i128_shl64(Int128 lhs, uint64_t amount);
Int128   i128_shl(Int128 lhs, Int128 rhs);
Int128   i128_ashr64(Int128 lhs, uint64_t amount);
Int128   i128_ashr(Int128 lhs, Int128 rhs);
Int128   i128_lshr64(Int128 lhs, uint64_t amount);
Int128   i128_lshr(Int128 lhs, Int128 rhs);
Int128   i128_udiv(Int128 lhs, Int128 rhs);
Int128   i128_sdiv(Int128 lhs, Int128 rhs);
Int128   i128_div(Int128 lhs, Int128 rhs, TypeKind kind);
Int128   i128_urem(Int128 lhs, Int128 rhs);
Int128   i128_srem(Int128 lhs, Int128 rhs);
Int128   i128_rem(Int128 lhs, Int128 rhs, TypeKind kind);
void     i128_udivrem(Int128 lhs, Int128 rhs, Int128 *div, Int128 *rem);
double   i128_to_float(Int128 op, TypeKind kind);
double   i128_to_float_signed(Int128 op);
double   i128_to_float_unsigned(Int128 op);
bool     i128_is_zero(Int128 op);
uint32_t i128_clz(const Int128 *op);
uint32_t i128_ctz(const Int128 *op);
Int128   i128_from_s64(int64_t i);
Int128   i128_from_u64(uint64_t i);
Int128   i128_from_double(double x, TypeKind kind);
Int128   i128_unsigned_from_double(double x);
Int128   i128_signed_from_double(double x);

// hash.c - Hashmap functions
void    hashmap_reserve(HashMap* map, uint32_t entry_cnt);
void    hashmap_put(HashMap* map, Symbol key, Object* val);
Object* hashmap_get(HashMap* map, Symbol key);
void    hashmap_clear(HashMap* map);

// arena.c - Arena functions
extern MemArena g_global_arena;

void   arena_init(MemArena* arena, size_t capacity);
void*  arena_malloc(MemArena* arena, size_t size, uint32_t align);
void*  arena_calloc(MemArena* arena, size_t size, uint32_t align);
void   arena_free(MemArena* arena, const void* ptr, size_t prev_size);
void*  arena_realloc(MemArena* arena, void* ptr, size_t size, uint32_t align, size_t prev_size);
void   global_arenas_init(void);

// error.c - Error functions
// TODO: Add specialized inline error functions for specific cases, similar to sic_error_redef.
//       This will just make it easier to repeat similar error messages without manually typing
//       things which could result in differing messages for the same issue.
extern int g_error_cnt;
extern int g_warning_cnt;

void sic_diagnosticv(DiagnosticType diag, const char* message, va_list va);
void sic_diagnostic_atv(DiagnosticType diag, SourceLoc loc, const char* message, va_list va);
void sic_diagnostic_afterv(DiagnosticType diag, SourceLoc loc, const char* under, const char* message, va_list va);
void sic_error_redef(Object* redef, Object* orig);

PRINTF_FMT(2, 3)
static inline void sic_diagnostic(DiagnosticType diag, const char* message, ...)
{
    va_list va;
    va_start(va, message);
    sic_diagnosticv(diag, message, va);
    va_end(va);
}

PRINTF_FMT(3, 4)
static inline void sic_diagnostic_at(DiagnosticType diag, SourceLoc loc, const char* message, ...)
{
    va_list va;
    va_start(va, message);
    sic_diagnostic_atv(diag, loc, message, va);
    va_end(va);
}

PRINTF_FMT(4, 5)
static inline void sic_diagnostic_after(DiagnosticType diag, SourceLoc loc, const char* under, const char* message, ...)
{
    va_list va;
    va_start(va, message);
    sic_diagnostic_afterv(diag, loc, under, message, va);
    va_end(va);
}

PRINTF_FMT(2, 3)
static inline void sic_error_at(SourceLoc loc, const char* message, ...)
{
    va_list va;
    va_start(va, message);
    sic_diagnostic_atv(DIAG_ERROR, loc, message, va);
    va_end(va);
}

PRINTF_FMT(1, 2) NORETURN
static inline void sic_fatal_error(const char* message, ...)
{
    va_list va;
    va_start(va, message);
    sic_diagnosticv(DIAG_FATAL, message, va);
    va_end(va);
    SIC_ERROR_DBG("Breaking because of fatal error!");
    exit(EXIT_FAILURE);
}

#ifdef SI_DEBUG
    #define SIC_TODO()        sic_fatal_error("\033[32mTODO: %s:%d(%s)\033[0m Not yet implemented.", __FILE__, __LINE__, __FUNCTION__)
    #define SIC_TODO_MSG(msg) sic_fatal_error("\033[32mTODO: %s:%d(%s)\033[0m %s", __FILE__, __LINE__, __FUNCTION__, msg)
#else
    #define SIC_TODO()        static_assert(false, "TODO not implemented in release.")
    #define SIC_TODO_MSG(msg) static_assert(false, "TODO not implemented in release. " msg)
#endif
#define SIC_UNREACHABLE() sic_fatal_error("Compiler encountered an unexpected error, should be unreachable. %s:%d(%s)", __FILE__, __LINE__, __FUNCTION__)


// string.c - Scratch Buffer and String format

extern ScratchBuffer g_scratch;

PRINTF_FMT(1, 2)
void scratch_appendf(const char* fmt, ...);
void scratch_append_module_path(const ObjModule* module);
PRINTF_FMT(1, 2)
char* str_format(const char* fmt, ...);
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
#define MALLOC(size, align)         cmalloc(size)
#define CALLOC(nmemb, size, align)  ccalloc(nmemb, size)
#define FREE(ptr, prev_size)
#define REALLOC(ptr, sz, al, psz)   crealloc(ptr, size)
#define MALLOC_STRUCT(type)         cmalloc(sizeof(type))
#define CALLOC_STRUCT(type)         ccalloc(1, sizeof(type))
#define MALLOC_STRUCTS(type, count) cmalloc(sizeof(type) * (count))
#define CALLOC_STRUCTS(type, count) ccalloc(count, sizeof(type))
#else
#define MALLOC(size, align)         arena_malloc(&g_global_arena, size, align)
#define CALLOC(nmemb, size, align)  arena_calloc(&g_global_arena, (nmemb) * (size), align)
#define FREE(ptr, prev_size)        arena_free(&g_global_arena, ptr, prev_size)
#define REALLOC(ptr, sz, al, psz)   arena_realloc(&g_global_arena, ptr, sz, al, psz)
#define MALLOC_STRUCT(type)         ((type*)arena_malloc(&g_global_arena, sizeof(type), _Alignof(type)))
#define CALLOC_STRUCT(type)         ((type*)arena_calloc(&g_global_arena, sizeof(type), _Alignof(type)))
#define MALLOC_STRUCTS(type, count) ((type*)arena_malloc(&g_global_arena, sizeof(type) * (count), _Alignof(type)))
#define CALLOC_STRUCTS(type, count) ((type*)arena_calloc(&g_global_arena, sizeof(type) * (count), _Alignof(type)))
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
    return c <= '9' && c >= '0';
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

static inline bool c_is_binary(char c)
{
    return c == '0' || c == '1';
}

static inline bool c_is_octal(char c)
{
    return c <= '7' && c >= '0';
}
