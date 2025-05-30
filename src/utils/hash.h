#pragma once

#include <stdbool.h>
#include <stddef.h>

typedef struct HashEntry HashEntry;
typedef struct HashMap   HashMap;

struct HashEntry
{
    HashEntry* next;
    char*  key;
    size_t key_len;
    void*  value;
};

struct HashMap
{
    HashEntry* buckets;
    size_t     bucket_cnt;
    size_t     entry_cnt;
};

void  hashmap_init(HashMap* map);
void  hashmap_free(HashMap* map);
void  hashmap_put(HashMap* map, char* key, void* val);
void* hashmap_get(HashMap* map, char* key);
bool  hashmap_delete(HashMap* map, char* key);
bool hashmap_test(void);
