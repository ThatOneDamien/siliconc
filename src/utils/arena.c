#include "lib.h"
#include "error.h"

#include <errno.h>
#include <sys/mman.h>

#define GLOBAL_ARENA_INIT_SIZE (64ul * 1024ul * 1024ul)

extern CompilerContext g_compiler;
MemArena g_global_arena;

void global_arenas_init(void)
{
    arena_init(&g_global_arena, GLOBAL_ARENA_INIT_SIZE);
}

void arena_init(MemArena* arena, size_t capacity)
{
    DBG_ASSERT(arena != NULL);
    DBG_ASSERT(capacity > 0);
    void* base = NULL;
    size_t min_cap = capacity >> 4;
    if(min_cap == 0)
        min_cap = capacity;

    while(capacity >= min_cap)
    {
        base = mmap(NULL, capacity, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANON, -1, 0);
        if((base != MAP_FAILED && base != NULL) ||
            errno != ENOMEM)
            break;

        capacity >>= 1;
    }

    if(base == MAP_FAILED || base == NULL)
        sic_fatal_error("Failed virtual memory mapping: \'%s\'", strerror(errno));

    arena->base = base;
    arena->capacity = capacity;
    arena->allocated = 0;
}

void* arena_malloc(MemArena* arena, size_t size, uint32_t align)
{
    // Some useful debug checks (These are not done in release).
    DBG_ASSERT(arena != NULL);
    DBG_ASSERT(size > 0);
    DBG_ASSERT(is_pow_of_2(align));
    DBG_ASSERT(arena->capacity > 0);

    arena->allocated = (arena->allocated + (align - 1)) & ~(align - 1);
    void* res = arena->base + arena->allocated;
    arena->allocated += size;
    if(arena->capacity < size)
        sic_fatal_error("Ran out of memory!!! An arena with capacity %zu overflowed.", arena->capacity);

    return res;
}

void* arena_calloc(MemArena* arena, size_t size, uint32_t align)
{
    void* res = arena_malloc(arena, size, align);
    memset(res, 0, size);
    return res;
}

void arena_free(MemArena* arena, const void* ptr, size_t prev_size)
{
    DBG_ASSERT(arena != NULL);
    uintptr_t p = (uintptr_t)ptr;
    uintptr_t end = (uintptr_t)arena->base + arena->allocated;
    DBG_ASSERT(p >= (uintptr_t)arena->base && p < end);
    if(ptr == NULL || p + prev_size != end) return;
    arena->allocated -= prev_size;
}

void* arena_realloc(MemArena* arena, void* ptr, size_t size, uint32_t align, size_t prev_size)
{
    DBG_ASSERT(arena != NULL);
    DBG_ASSERT(size > 0);
    DBG_ASSERT(is_pow_of_2(align));
    uintptr_t p = (uintptr_t)ptr;
    uintptr_t end = (uintptr_t)arena->base + arena->allocated;
    if(ptr != NULL && (p >= (uintptr_t)arena->base && p < end) && (p + prev_size == end))
    {
        // Reclaim memory if at the end of the arena.
        if(prev_size >= size)
        {
            arena->allocated -= prev_size - size;
#ifdef SI_DEBUG
            if(g_compiler.debug_output & DEBUG_MEMORY)
                printf("Reduced from %lu to %lu. Saved %lu bytes.\n", prev_size, size, prev_size - size);
#endif
            return ptr;
        }
        size_t needed = size - prev_size;
        if(arena->capacity - arena->allocated < needed)
            sic_fatal_error("Ran out of memory!!! An arena with capacity %zu overflowed.", arena->capacity);
        arena->allocated += needed; 
        return ptr;
    }

    if(prev_size >= size) return ptr;

    void* new_ptr = arena_malloc(arena, size, align);
    memcpy(new_ptr, ptr, prev_size);
    return new_ptr;
}
