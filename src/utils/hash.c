#include "lib.h"

#include <stdint.h>
#include <stdarg.h>

#define INITIAL_HASH_CAP 16

#define LOAD_FACTOR_C  4u
#define LOAD_FACTOR_HI 3u
#define LOAD_FACTOR_LO 2u

static HashEntry* get_entry(HashMap* map, Symbol key, uint64_t hash);
static void       rehash(HashMap* map, uint32_t new_cap);

static inline uint64_t hash_ptr(Symbol symbol);

void hashmap_reserve(HashMap* map, uint32_t entry_cnt)
{
    DBG_ASSERT(map != NULL);
    uint32_t new_cap = MAX(INITIAL_HASH_CAP, next_pow_of_2(entry_cnt * LOAD_FACTOR_C / LOAD_FACTOR_LO));
    if(map->bucket_cnt < new_cap)
        rehash(map, new_cap);
}

void hashmap_put(HashMap* map, Symbol key, Object* val)
{
    DBG_ASSERT(map != NULL);
    DBG_ASSERT(key != NULL);
    if(map->buckets == NULL)
        rehash(map, INITIAL_HASH_CAP);
    else
        hashmap_reserve(map, map->entry_cnt + 1);

    uint64_t hash = hash_ptr(key);
    HashEntry* entry = get_entry(map, key, hash);
    if(entry->key == NULL)
    {
        entry->key     = key;
        entry->hash    = hash;
        entry->value   = val;
        map->entry_cnt++;
    }
    else
        entry->value = val;
}

Object* hashmap_get(HashMap* map, Symbol key)
{
    DBG_ASSERT(map != NULL);
    DBG_ASSERT(key != NULL);
    if(map->buckets == NULL)
        return NULL;
    HashEntry* entry = get_entry(map, key, hash_ptr(key));
    return entry->key == NULL ? NULL : entry->value;
}

void hashmap_clear(HashMap* map)
{
    DBG_ASSERT(map != NULL);
    if(map->buckets == NULL || map->entry_cnt == 0)
        return;

    memset(map->buckets, 0, map->bucket_cnt * sizeof(HashEntry));
    map->entry_cnt = 0;
}

static HashEntry* get_entry(HashMap* map, Symbol key, uint64_t hash)
{
    DBG_ASSERT(is_pow_of_2(map->bucket_cnt));
    size_t bucket_mask = map->bucket_cnt - 1;
    size_t index = hash & bucket_mask;
    HashEntry* entry;
    while((entry = map->buckets + index)->key != NULL)
    {
        if(entry->key == key)
            break;
        index = (index + 1) & bucket_mask;
    }
    return entry;
}

static void rehash(HashMap* map, uint32_t new_cap)
{
    HashMap temp_map;
    temp_map.bucket_cnt = new_cap;
    temp_map.buckets    = CALLOC_STRUCTS(HashEntry, new_cap); 
    temp_map.entry_cnt  = map->entry_cnt;
    temp_map.max_load   = new_cap * LOAD_FACTOR_HI / LOAD_FACTOR_C;
    if(map->buckets != NULL)
    {
        for(uint32_t i = 0, cnt = 0; cnt < map->entry_cnt; ++i)
        {
            HashEntry* bucket = map->buckets + i;
            if(bucket->key != NULL)
            {
                HashEntry* new_bucket = get_entry(&temp_map, bucket->key, bucket->hash);
                memcpy(new_bucket, bucket, sizeof(HashEntry));
                cnt++;
            }
        }
    }

    *map = temp_map;
}

static inline uint64_t hash_ptr(Symbol symbol)
{
    uint64_t hash = (uint64_t)symbol;
    hash ^= hash >> 32;
    hash ^= hash >> 16;
    return hash;
}
