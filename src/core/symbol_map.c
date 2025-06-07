#include "internal.h"
#include "utils/lib.h"

static HashMap s_sym_map;

void sym_map_init(void)
{
    hashmap_initn(&s_sym_map, TOKEN_KEYWORD_END - TOKEN_KEYWORD_START + 1);
    for(TokenKind kw = TOKEN_KEYWORD_START; kw <= TOKEN_KEYWORD_END; ++kw)
        hashmap_put(&s_sym_map, tok_kind_to_str(kw), (void*)kw);
}

TokenKind sym_map_get(const char* str)
{
    return (TokenKind)(uintptr_t)hashmap_get(&s_sym_map, str);
}

TokenKind sym_map_getn(const char* str, size_t len)
{
    return (TokenKind)(uintptr_t)hashmap_getn(&s_sym_map, str, len);
}
