#include "internal.h"
#include "utils/error.h"

#define BUILTIN_DEF(TYPE, SIZE) { .kind = TYPE, .qualifiers = QUALIFIER_NONE, .builtin = {SIZE} }

static Type s_void = BUILTIN_DEF(TYPE_VOID,  0);
static Type s_u8   = BUILTIN_DEF(TYPE_U8  ,  1);
static Type s_s8   = BUILTIN_DEF(TYPE_S8  ,  1);
static Type s_u16  = BUILTIN_DEF(TYPE_U16 ,  2);
static Type s_s16  = BUILTIN_DEF(TYPE_S16 ,  2);
static Type s_u32  = BUILTIN_DEF(TYPE_U32 ,  4);
static Type s_s32  = BUILTIN_DEF(TYPE_S32 ,  4);
static Type s_u64  = BUILTIN_DEF(TYPE_U64 ,  8);
static Type s_s64  = BUILTIN_DEF(TYPE_S64 ,  8);
static Type s_f32  = BUILTIN_DEF(TYPE_F32 ,  4);
static Type s_f64  = BUILTIN_DEF(TYPE_F64 ,  8);

// Builtin-types
Type* g_type_void = &s_void;
Type* g_type_u8   = &s_u8;
Type* g_type_s8   = &s_s8;
Type* g_type_u16  = &s_u16;
Type* g_type_s16  = &s_s16;
Type* g_type_u32  = &s_u32;
Type* g_type_s32  = &s_s32;
Type* g_type_u64  = &s_u64;
Type* g_type_s64  = &s_s64;
Type* g_type_f32  = &s_f32;
Type* g_type_f64  = &s_f64;

static Type* builtin_type_lookup[] = {
    [TOKEN_VOID - TOKEN_TYPENAME_START] = &s_void,
    [TOKEN_U8   - TOKEN_TYPENAME_START] = &s_u8,
    [TOKEN_S8   - TOKEN_TYPENAME_START] = &s_s8,
    [TOKEN_U16  - TOKEN_TYPENAME_START] = &s_u16,
    [TOKEN_S16  - TOKEN_TYPENAME_START] = &s_s16,
    [TOKEN_U32  - TOKEN_TYPENAME_START] = &s_u32,
    [TOKEN_S32  - TOKEN_TYPENAME_START] = &s_s32,
    [TOKEN_U64  - TOKEN_TYPENAME_START] = &s_u64,
    [TOKEN_S64  - TOKEN_TYPENAME_START] = &s_s64,
    [TOKEN_F32  - TOKEN_TYPENAME_START] = &s_f32,
    [TOKEN_F64  - TOKEN_TYPENAME_START] = &s_f64,
};

Type* builtin_type(TokenKind type_token)
{
    SIC_ASSERT(is_builtin_type(type_token));
    return builtin_type_lookup[type_token - TOKEN_TYPENAME_START];
}

Type* type_copy(Type* orig)
{
    SIC_ASSERT(orig != NULL);
    Type* new_type = MALLOC(sizeof(Type));
    memcpy(new_type, orig, sizeof(Type));
    return new_type;
}

Type* pointer_to(Type* base)
{
    SIC_ASSERT(base != NULL);
    Type* new_type = MALLOC(sizeof(Type));
    new_type->kind = TYPE_POINTER;
    new_type->qualifiers = QUALIFIER_NONE;
    new_type->pointer_base = base;
    return new_type;
}

const char* type_to_string(Type* type)
{
    SIC_ASSERT(type != NULL);
    static const char* type_names[] = {
        [TYPE_INVALID] = "Invalid",
        [TYPE_VOID]    = "void",
        [TYPE_S8]      = "s8",
        [TYPE_U8]      = "u8",
        [TYPE_S16]     = "s16",
        [TYPE_U16]     = "u16",
        [TYPE_S32]     = "s32",
        [TYPE_U32]     = "u32",
        [TYPE_S64]     = "s64",
        [TYPE_U64]     = "u64",
        [TYPE_F32]     = "f32",
        [TYPE_F64]     = "f64",
    };

    if(type->kind >= TYPE_BUILTIN_START && type->kind <= TYPE_BUILTIN_END)
        return type_names[type->kind];
    if(type->kind == TYPE_POINTER)
        return str_format("%s*", type_to_string(type->pointer_base));

    sic_error_fatal("Unreachable.");
    return NULL;
}
