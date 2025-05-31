#pragma once

#include <stdbool.h>
#include <stddef.h>
#include <string.h>


typedef struct HashEntry HashEntry;
typedef struct HashMap   HashMap;

struct HashEntry
{
    HashEntry*  next;
    const char* key;
    size_t      key_len;
    void*       value;
};

struct HashMap
{
    HashEntry* buckets;
    size_t     bucket_cnt;
    size_t     entry_cnt;
};

void  hashmap_initn(HashMap* map, size_t entry_cnt);
void  hashmap_free(HashMap* map);
void  hashmap_putn(HashMap* map, const char* key, size_t len, void* val);
void* hashmap_getn(HashMap* map, const char* key, size_t len);
bool  hashmap_deleten(HashMap* map, const char* key, size_t len);
void  hashmap_clear(HashMap* map);

static inline void hashmap_init(HashMap* map)
{
    hashmap_initn(map, 0);
}

static inline void hashmap_put(HashMap* map, const char* key, void* val)
{ 
    hashmap_putn(map, key, strlen(key), val); 
}

static inline void* hashmap_get(HashMap* map, const char* key)
{
    return hashmap_getn(map, key, strlen(key));
}

static inline bool hashmap_delete(HashMap* map, const char* key)
{
    return hashmap_deleten(map, key, strlen(key));
}
