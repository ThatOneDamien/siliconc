#pragma once

#include <stdint.h>

typedef enum
{
    TYPE_VOID,
    TYPE_U8,
    TYPE_S8,
    TYPE_I8 = TYPE_S8,
    TYPE_U16,
    TYPE_S16,
    TYPE_I16 = TYPE_S16,
    TYPE_U32,
    TYPE_S32,
    TYPE_I32 = TYPE_S32,
    TYPE_U64,
    TYPE_S64,
    TYPE_I64 = TYPE_S64,
    TYPE_F32,
    TYPE_F64,
    TYPE_F128,
    // TODO: Add structs, unions, arrays, pointers, etc

} TypeKind;

typedef struct Type Type;
struct Type
{
    TypeKind kind;
    uint32_t size;
    uint32_t alignment;
};

// Builtin types (defined in type.c)
extern Type* t_void;
extern Type* t_u8;
extern Type* t_s8;
extern Type* t_u16;
extern Type* t_s16;
extern Type* t_u32;
extern Type* t_s32;
extern Type* t_u64;
extern Type* t_s64;
extern Type* t_f32;
extern Type* t_f64;
extern Type* t_f128;
