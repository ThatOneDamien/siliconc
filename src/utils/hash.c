#include "lib.h"
#include "core/core.h"

#include <stdint.h>
#include <stdarg.h>

#define INITIAL_HASH_CAP 16

#define LOAD_FACTOR_C  4u
#define LOAD_FACTOR_HI 3u
#define LOAD_FACTOR_LO 2u

static HashEntry* get_entry(HashMap* map, const char* key, size_t len, HashEntry** bucket);
static void       rehash(HashMap* map);
static uint64_t   fnv_hash(const char* str, size_t len);

void hashmap_initn(HashMap* map, uint32_t entry_cnt)
{
    SIC_ASSERT(map != NULL);
    uint32_t cap = MAX(INITIAL_HASH_CAP, next_pow_of_2(entry_cnt * LOAD_FACTOR_C / LOAD_FACTOR_LO));

    map->bucket_cnt = cap;
    map->buckets = calloc(map->bucket_cnt, sizeof(HashEntry));
    map->entry_cnt = 0;
    map->max_load = cap * LOAD_FACTOR_HI / LOAD_FACTOR_C;
}

void hashmap_free(HashMap* map)
{
    SIC_ASSERT(map != NULL);
    hashmap_clear(map);
    free(map->buckets);
}

void hashmap_putn(HashMap* map, const char* key, size_t len, void* val)
{
    SIC_ASSERT(map != NULL);
    SIC_ASSERT(key != NULL);
    SIC_ASSERT(map->buckets != NULL);
    if(map->entry_cnt >= map->max_load)
        rehash(map);

    HashEntry* bucket;
    HashEntry* entry = get_entry(map, key, len, &bucket);
    if(entry == NULL)
    {
        entry = malloc(sizeof(HashEntry));
        entry->key     = key;
        entry->key_len = len;
        entry->value   = val;
        entry->next    = bucket->next;
        bucket->next   = entry;
        map->entry_cnt++;
    }
    else
        entry->value = val;
}

void* hashmap_getn(HashMap* map, const char* key, size_t len)
{
    SIC_ASSERT(map != NULL);
    SIC_ASSERT(key != NULL);
    SIC_ASSERT(map->buckets != NULL);
    HashEntry* entry = get_entry(map, key, len, NULL);
    return entry == NULL ? NULL : entry->value;
}

bool hashmap_deleten(HashMap* map, const char* key, size_t len)
{
    SIC_ASSERT(map != NULL);
    SIC_ASSERT(key != NULL);
    SIC_ASSERT(map->buckets != NULL);
    SIC_ASSERT(is_pow_of_2(map->bucket_cnt));
    uint64_t hash = fnv_hash(key, len);
    HashEntry* prev = map->buckets + (hash & (uint64_t)(map->bucket_cnt - 1));
    HashEntry* cur = prev->next;
    while(cur != NULL)
    {
        if(cur->key_len == len && memcmp(cur->key, key, len) == 0)
        {
            prev->next = cur->next;
            map->entry_cnt--;
            free(cur);
            return true;
        }
        prev = cur;
        cur = cur->next;
    }
    return false;
}

void hashmap_clear(HashMap* map)
{
    SIC_ASSERT(map != NULL);
    if(map->buckets == NULL || map->entry_cnt == 0)
        return;

    for(size_t i = 0; i < map->bucket_cnt; ++i)
    {
        HashEntry* bucket = map->buckets + i;
        while(bucket->next != NULL)
        {
            HashEntry* entry = bucket->next;
            bucket->next = entry->next;
            free(entry);
        }
    }

    map->entry_cnt = 0;
}

static HashEntry* get_entry(HashMap* map, const char* key, size_t len, HashEntry** obucket)
{
    SIC_ASSERT(is_pow_of_2(map->bucket_cnt));
    uint64_t hash = fnv_hash(key, len);
    HashEntry* bucket = map->buckets + (hash & (uint64_t)(map->bucket_cnt - 1));
    if(obucket != NULL)
        *obucket = bucket;
    HashEntry* cur = bucket->next;
    while(cur != NULL)
    {
        if(cur->key_len == len && memcmp(cur->key, key, len) == 0)
            return cur;
        cur = cur->next;
    }
    return NULL;
}

static void rehash(HashMap* map)
{
    SIC_ASSERT(map->buckets != NULL);
    uint32_t new_cap = next_pow_of_2(map->entry_cnt * LOAD_FACTOR_C / LOAD_FACTOR_LO);

    HashMap new_map;
    new_map.bucket_cnt = (size_t)new_cap;
    new_map.buckets = calloc(new_map.bucket_cnt, sizeof(HashEntry));
    new_map.entry_cnt = map->entry_cnt;
    for(uint32_t i = 0; i < map->bucket_cnt; ++i)
    {
        HashEntry* bucket = map->buckets[i].next;
        while(bucket != NULL)
        {
            HashEntry* entry = bucket;
            bucket = bucket->next;
            uint64_t hash = fnv_hash(entry->key, entry->key_len);
            HashEntry* new_bucket = new_map.buckets + (hash % new_map.bucket_cnt);
            entry->next = new_bucket->next;
            new_bucket->next = entry;
        }
    }
    free(map->buckets);

    *map = new_map;
}

static uint64_t fnv_hash(const char* str, size_t len)
{
    uint64_t hash = 0xCBF29CE484222325;
    for(size_t i = 0; i < len; ++i)
    {
        hash *= 0x100000001B3;
        hash ^= (uint8_t)str[i];
    }
    return hash;
}
