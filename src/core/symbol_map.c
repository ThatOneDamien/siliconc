#include "internal.h"
#include "utils/lib.h"

#include <sys/mman.h>

#define SYM_MAP_DEFAULT_CAP (1024ul * 256ul)
#define SYM_ADD(s) str = s; len = sizeof(s) - 1; sym_map_internal_add(str, len, fnv_hash(str, len), &kind, true)

typedef struct SymMapEntry SymMapEntry;
typedef struct SymbolMap   SymbolMap;

struct SymMapEntry
{
    SymMapEntry* next;
    Symbol       symbol;
    uint32_t     hash;
    uint16_t     len;
    TokenKind    kind;
};

struct SymbolMap
{
    SymMapEntry** buckets;
    size_t        mask;
};

static Symbol sym_map_internal_add(const char* str, uint32_t len, uint32_t hash, TokenKind* kind, bool take_ptr);
static Symbol sym_map_internal_get(const char* str, uint32_t len, uint32_t hash, TokenKind* kind);

static SymbolMap s_sym_map;

Symbol g_sym_len;
Symbol g_sym_main;
Symbol g_attr_list[__ATTR_COUNT];

void sym_map_init(void)
{
    size_t capacity = SYM_MAP_DEFAULT_CAP * sizeof(SymMapEntry*);
    s_sym_map.buckets = mmap(NULL, capacity, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANON, -1, 0);
    s_sym_map.mask = capacity - 1;
    memset(s_sym_map.buckets, 0, capacity);

    TokenKind kind;
    const char* str;
    size_t len;
    for(kind = TOKEN_KEYWORD_START; kind <= TOKEN_KEYWORD_END; ++kind)
    {
        str = tok_kind_to_str(kind);
        len = strlen(str);
        sym_map_internal_add(str, len, fnv_hash(str, len), &kind, true);
    }

    kind = TOKEN_IDENT;
    g_sym_len = SYM_ADD("len");
    g_sym_main = SYM_ADD("main");
    
    kind = TOKEN_ATTRIBUTE_IDENT;
    g_attr_list[ATTR_INLINE]    = SYM_ADD("@inline");
    g_attr_list[ATTR_NODISCARD] = SYM_ADD("@nodiscard");
    g_attr_list[ATTR_NOINLINE]  = SYM_ADD("@noinline");
    g_attr_list[ATTR_PACKED]    = SYM_ADD("@packed");
    g_attr_list[ATTR_PURE]      = SYM_ADD("@pure");
}

Symbol sym_map_addn(const char* str, uint32_t len, TokenKind* kind)
{
    return sym_map_internal_add(str, len, fnv_hash(str, len), kind, false);
}

Symbol sym_map_getn(const char* str, uint32_t len, TokenKind* kind)
{
    return sym_map_internal_get(str, len, fnv_hash(str, len), kind);
}

static Symbol sym_map_internal_add(const char* str, uint32_t len, uint32_t hash, TokenKind* kind, bool take_ptr)
{
    size_t index = hash & s_sym_map.mask;
    SymMapEntry* bucket = s_sym_map.buckets[index];
    SymMapEntry* cur = bucket;
    
    Symbol found = sym_map_internal_get(str, len, hash, kind);
    if(found != NULL)
        return found;

    cur = MALLOC_STRUCT(SymMapEntry);
    s_sym_map.buckets[index] = cur;
    cur->symbol = take_ptr ? str : str_dupn(str, len);
    cur->len    = len;
    cur->hash   = hash;
    cur->kind   = *kind;
    cur->next   = bucket;
    return cur->symbol;
}

static Symbol sym_map_internal_get(const char* str, uint32_t len, uint32_t hash, TokenKind* kind)
{
    size_t index = hash & s_sym_map.mask;
    SymMapEntry* cur = s_sym_map.buckets[index];
    while(cur != NULL)
    {
        if(cur->hash == hash && cur->len == len && memcmp(cur->symbol, str, len) == 0)
        {
            *kind = cur->kind;
            return cur->symbol;
        }
        cur = cur->next;
    }
    return NULL;
}
