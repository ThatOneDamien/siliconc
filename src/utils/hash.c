#include "hash.h"
#include "core/core.h"

#include <stdint.h>
#include <stdarg.h>

#define INITIAL_HASH_CAP 16

#define LOAD_FACTOR_HI 75ull
#define LOAD_FACTOR_LO 50ull

static HashEntry* get_entry(HashMap* map, const char* key, size_t len, HashEntry** bucket);
static void       rehash(HashMap* map);
static uint64_t   fnv_hash(const char* str, size_t len);

void hashmap_initn(HashMap* map, size_t entry_cnt)
{
    SIC_ASSERT(map != NULL);
    size_t cap = INITIAL_HASH_CAP;
    while((entry_cnt * 100) / cap >= LOAD_FACTOR_LO)
        cap *= 2;

    map->bucket_cnt = (size_t)cap;
    map->buckets = calloc(map->bucket_cnt, sizeof(HashEntry));
    map->entry_cnt = 0;
}

void hashmap_free(HashMap* map)
{
    SIC_ASSERT(map != NULL);
    hashmap_clear(map);
    free(map->buckets);
}

void  hashmap_putn(HashMap* map, const char* key, size_t len, void* val)
{
    SIC_ASSERT(map != NULL);
    SIC_ASSERT(key != NULL);
    SIC_ASSERT(map->buckets != NULL);
    size_t load_factor = (map->entry_cnt * 100) / map->bucket_cnt;
    if(load_factor > LOAD_FACTOR_HI)
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
    uint64_t hash = fnv_hash(key, len);
    HashEntry* prev = map->buckets + (hash % map->bucket_cnt);
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
    uint64_t hash = fnv_hash(key, len);
    HashEntry* bucket = map->buckets + (hash % map->bucket_cnt);
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
    size_t new_cap = map->bucket_cnt;
    while((map->bucket_cnt * 100) / new_cap >= LOAD_FACTOR_LO)
        new_cap *= 2;

    printf("here\n");
    HashMap new_map;
    new_map.bucket_cnt = (size_t)new_cap;
    new_map.buckets = calloc(new_map.bucket_cnt, sizeof(HashEntry));
    new_map.entry_cnt = map->entry_cnt;
    for(size_t i = 0; i < map->bucket_cnt; ++i)
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

char* format(const char* restrict fmt, ...)
{
    char *buf;
    va_list va;
    va_start(va, fmt);
    int size = vsnprintf(NULL, 0, fmt, va);
    va_end(va);
    buf = malloc(size + 1);

    va_start(va, fmt);
    vsnprintf(buf, size + 1, fmt, va);
    va_end(va);
    buf[size] = '\0';

    return buf;
}
