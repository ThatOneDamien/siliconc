#include "lib.h"
#include "error.h"

#include <errno.h>
#include <sys/mman.h>

#define GLOBAL_ARENA_INIT_SIZE (64ul * 1024ul * 1024ul)

MemArena g_global_arena;

void global_arenas_init(void)
{
    arena_init(&g_global_arena, GLOBAL_ARENA_INIT_SIZE);
}

void arena_init(MemArena* arena, size_t capacity)
{
    SIC_ASSERT(arena != NULL);
    SIC_ASSERT(capacity > 0);
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
    arena->last_alloced = NULL;
}

void* arena_malloc(MemArena* arena, size_t size, uint32_t align)
{
    // Some useful debug checks (These are not done in release).
    SIC_ASSERT(arena != NULL);
    SIC_ASSERT(size > 0);
    SIC_ASSERT(is_pow_of_2(align));
    SIC_ASSERT(arena->capacity > 0);

    arena->allocated = (arena->allocated + (align - 1)) & ~(align - 1);
    void* res = arena->last_alloced = arena->base + arena->allocated;
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

void arena_free(MemArena* arena, const void* ptr)
{
    if(arena->last_alloced == NULL || ptr != arena->last_alloced) return;
    arena->allocated = (uintptr_t)arena->last_alloced - (uintptr_t)arena->base;
    arena->last_alloced = NULL;
}
