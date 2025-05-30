#include "hash.h"
#include "core/core.h"

#include <stdint.h>
#include <string.h>
#include <stdarg.h>

#define INITIAL_CAP    16
#define LOAD_FACTOR_HI 0.75
#define LOAD_FACTOR_LO 0.50

static HashEntry* get_entry(HashMap* map, char* key, size_t len, HashEntry** bucket);
static void       rehash(HashMap* map);
static uint64_t   fnv_hash(char* str, size_t len);

void hashmap_init(HashMap* map)
{
    SIC_ASSERT(map != NULL);
    map->buckets = calloc(INITIAL_CAP, sizeof(HashEntry));
    map->bucket_cnt = 16;
    map->entry_cnt = 0;
}

void hashmap_free(HashMap* map)
{
    SIC_ASSERT(map != NULL);
    if(map->buckets == NULL)
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
    free(map->buckets);
}

void hashmap_put(HashMap* map, char* key, void* val)
{
    SIC_ASSERT(map != NULL);
    SIC_ASSERT(key != NULL);
    SIC_ASSERT(map->buckets != NULL);
    double load_factor = (double)map->entry_cnt / (double)map->bucket_cnt;
    if(load_factor > LOAD_FACTOR_HI)
        rehash(map);

    size_t len = strlen(key);
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
    }
    else
        entry->value = val;
}

void* hashmap_get(HashMap* map, char* key)
{
    SIC_ASSERT(map != NULL);
    SIC_ASSERT(key != NULL);
    HashEntry* entry = get_entry(map, key, strlen(key), NULL);
    printf("Value: %s %s %lu\n", key, entry->key, entry == NULL ? (size_t)-1 : (size_t)entry->value);
    return entry == NULL ? NULL : entry->value;
}

bool hashmap_delete(HashMap* map, char* key)
{
    size_t len = strlen(key);
    uint64_t hash = fnv_hash(key, len);
    HashEntry* prev = map->buckets + (hash % map->bucket_cnt);
    HashEntry* cur = prev->next;
    while(cur != NULL)
    {
        if(cur->key_len == len && memcmp(cur->key, key, len) == 0)
        {
            prev->next = cur->next;
            free(cur);
            return true;
        }
        prev = cur;
        cur = cur->next;
    }
    return false;
}

static HashEntry* get_entry(HashMap* map, char* key, size_t len, HashEntry** obucket)
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
    double entries = map->entry_cnt;
    double new_cap = map->bucket_cnt;
    while(entries / new_cap >= LOAD_FACTOR_LO)
        new_cap *= 2;

    HashMap new_map;
    new_map.buckets = calloc(new_cap, sizeof(HashEntry));
    new_map.bucket_cnt = new_cap;
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

static uint64_t fnv_hash(char* str, size_t len)
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
    printf("%s\n", fmt);
    char *buf;
    va_list va;
    va_start(va, fmt);
    int size = vsnprintf(NULL, 0, fmt, va);
    printf("size: %d\n", size);
    va_end(va);
    buf = malloc(size + 1);
    if(buf == NULL)
        printf("here\n");

    va_start(va, fmt);
    vsnprintf(buf, size, fmt, va);
    va_end(va);
    buf[size] = '\0';
    printf("%s\n", buf);

    return buf;
}

bool hashmap_test(void) {
    HashMap map;
    hashmap_init(&map);

    for(size_t i = 0; i < 5000; i++)
        hashmap_put(&map, format("key %lu", i), (void *)(size_t)i);
    for(size_t i = 1000; i < 2000; i++)
        hashmap_delete(&map, format("key %lu", i));
    for(size_t i = 1500; i < 1600; i++)
        hashmap_put(&map, format("key %lu", i), (void *)(size_t)i);
    for(size_t i = 6000; i < 7000; i++)
        hashmap_put(&map, format("key %lu", i), (void *)(size_t)i);

    for(size_t i = 0; i < 1000; i++)
        SIC_ASSERT((size_t)hashmap_get(&map, format("key %lu", i)) == i);
    for(size_t i = 1000; i < 1500; i++)
        SIC_ASSERT(hashmap_get(&map, "no such key") == NULL);
    for(size_t i = 1500; i < 1600; i++)
        SIC_ASSERT((size_t)hashmap_get(&map, format("key %lu", i)) == i);
    for(size_t i = 1600; i < 2000; i++)
        SIC_ASSERT(hashmap_get(&map, "no such key") == NULL);
    for(size_t i = 2000; i < 5000; i++)
        SIC_ASSERT((size_t)hashmap_get(&map, format("key %lu", i)) == i);
    for(size_t i = 5000; i < 6000; i++)
        SIC_ASSERT(hashmap_get(&map, "no such key") == NULL);
    for(size_t i = 6000; i < 7000; i++)
        hashmap_put(&map, format("key %lu", i), (void *)(size_t)i);

    SIC_ASSERT(hashmap_get(&map, "no such key") == NULL);
    printf("OK\n");
    return true;
}
