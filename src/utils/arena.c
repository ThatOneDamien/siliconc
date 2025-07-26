#include "lib.h"
#include "error.h"

#include <errno.h>
#include <sys/mman.h>

#define GLOBAL_ARENA_INIT_SIZE (64ul * 1024ul * 1024ul)

static MemArena s_global_arena;

void global_arenas_init(void)
{
    arena_init(&s_global_arena, GLOBAL_ARENA_INIT_SIZE);
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
}

void* arena_alloc(MemArena* arena, size_t size, uint32_t align)
{
    // Some useful debug checks (These are not done in release).
    SIC_ASSERT(arena != NULL);
    SIC_ASSERT(size > 0);
    SIC_ASSERT(is_pow_of_2(align));
    SIC_ASSERT(arena->capacity > 0);

    arena->allocated = (arena->allocated + (align - 1)) & ~(align - 1);
    void* res = arena->base + arena->allocated;
    arena->allocated += size;
    if(arena->capacity < size)
        sic_fatal_error("Ran out of memory!!! An arena with capacity %zu overflowed.", arena->capacity);

    return res;
}

void* global_arena_malloc(size_t size, uint32_t align)
{
    return arena_alloc(&s_global_arena, size, align);
}

void* global_arena_calloc(size_t nmemb, size_t size, uint32_t align)
{
    void* res = arena_alloc(&s_global_arena, nmemb * size, align);
    memset(res, 0, nmemb * size);
    return res;
}

size_t global_arena_allocated()
{
    return s_global_arena.allocated;
}
