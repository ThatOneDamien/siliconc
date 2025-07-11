#include "internal.h"
#include "utils/error.h"

#define BUILTIN_DEF(TYPE, SIZE) { .kind = TYPE, .qualifiers = QUALIFIER_NONE, .builtin = {SIZE} }

static Type s_void = BUILTIN_DEF(TYPE_VOID,  0);
static Type s_bool = BUILTIN_DEF(TYPE_BOOL,  1);
static Type s_u8   = BUILTIN_DEF(TYPE_U8  ,  1);
static Type s_u16  = BUILTIN_DEF(TYPE_U16 ,  2);
static Type s_u32  = BUILTIN_DEF(TYPE_U32 ,  4);
static Type s_u64  = BUILTIN_DEF(TYPE_U64 ,  8);
static Type s_s8   = BUILTIN_DEF(TYPE_S8  ,  1);
static Type s_s16  = BUILTIN_DEF(TYPE_S16 ,  2);
static Type s_s32  = BUILTIN_DEF(TYPE_S32 ,  4);
static Type s_s64  = BUILTIN_DEF(TYPE_S64 ,  8);
static Type s_f32  = BUILTIN_DEF(TYPE_F32 ,  4);
static Type s_f64  = BUILTIN_DEF(TYPE_F64 ,  8);

// Builtin-types
Type* g_type_void = &s_void;
Type* g_type_bool = &s_bool;
Type* g_type_u8   = &s_u8;
Type* g_type_u16  = &s_u16;
Type* g_type_u32  = &s_u32;
Type* g_type_u64  = &s_u64;
Type* g_type_s8   = &s_s8;
Type* g_type_s16  = &s_s16;
Type* g_type_s32  = &s_s32;
Type* g_type_s64  = &s_s64;
Type* g_type_f32  = &s_f32;
Type* g_type_f64  = &s_f64;

static Type* builtin_type_lookup[] = {
    [TOKEN_VOID - TOKEN_TYPENAME_START] = &s_void,
    [TOKEN_BOOL - TOKEN_TYPENAME_START] = &s_bool,
    [TOKEN_U8   - TOKEN_TYPENAME_START] = &s_u8,
    [TOKEN_U16  - TOKEN_TYPENAME_START] = &s_u16,
    [TOKEN_U32  - TOKEN_TYPENAME_START] = &s_u32,
    [TOKEN_U64  - TOKEN_TYPENAME_START] = &s_u64,
    [TOKEN_S8   - TOKEN_TYPENAME_START] = &s_s8,
    [TOKEN_S16  - TOKEN_TYPENAME_START] = &s_s16,
    [TOKEN_S32  - TOKEN_TYPENAME_START] = &s_s32,
    [TOKEN_S64  - TOKEN_TYPENAME_START] = &s_s64,
    [TOKEN_F32  - TOKEN_TYPENAME_START] = &s_f32,
    [TOKEN_F64  - TOKEN_TYPENAME_START] = &s_f64,
};

Type* type_from_token(TokenKind type_token)
{
    SIC_ASSERT(token_is_typename(type_token));
    return builtin_type_lookup[type_token - TOKEN_TYPENAME_START];
}

Type* type_copy(Type* orig)
{
    SIC_ASSERT(orig != NULL);
    Type* new_type = MALLOC_STRUCT(Type);
    memcpy(new_type, orig, sizeof(Type));
    return new_type;
}

Type* type_pointer_to(Type* base)
{
    SIC_ASSERT(base != NULL);
    Type* new_type = CALLOC_STRUCT(Type);
    new_type->kind = TYPE_POINTER;
    new_type->pointer_base = base;
    return new_type;
}

Type* type_array_of(Type* elem_ty, ASTExpr* size_expr)
{
    Type* new_type = CALLOC_STRUCT(Type);
    new_type->kind = TYPE_PRE_SEMA_ARRAY;
    new_type->pre_sema_array.elem_type = elem_ty;
    new_type->pre_sema_array.size_expr = size_expr;
    return new_type;
}

bool type_equal(Type* t1, Type* t2)
{
    SIC_ASSERT(t1 != NULL);
    SIC_ASSERT(t2 != NULL);
    switch(t1->kind)
    {
    case TYPE_VOID:
    case TYPE_BOOL:
    case TYPE_U8:
    case TYPE_U16:
    case TYPE_U32:
    case TYPE_U64:
    case TYPE_S8:
    case TYPE_S16:
    case TYPE_S32:
    case TYPE_S64:
    case TYPE_F32:
    case TYPE_F64:
        return t1->kind == t2->kind;
    case TYPE_POINTER:
    case TYPE_DS_ARRAY:
        return type_is_pointer(t2) && type_equal(type_get_base(t1), type_get_base(t2));
    case TYPE_SS_ARRAY:
        return t2->kind == TYPE_SS_ARRAY && 
               t1->ss_array.elem_cnt == t2->ss_array.elem_cnt &&
               type_equal(t1->ss_array.elem_type, t2->ss_array.elem_type);
    default:
        SIC_UNREACHABLE();
    }
}

uint32_t type_size(Type* ty)
{
    SIC_ASSERT(ty != NULL);
    if(type_is_builtin(ty))
        return ty->builtin.size;
    if(type_is_pointer(ty))
        return 8;
    if(ty->kind == TYPE_SS_ARRAY)
        return type_size(ty->ss_array.elem_type) * ty->ss_array.elem_cnt;

    SIC_UNREACHABLE();
}

uint32_t type_alignment(Type* ty)
{
    SIC_ASSERT(ty != NULL);
    if(type_is_builtin(ty))
        return ty->builtin.size;
    if(type_is_pointer(ty) || ty->kind == TYPE_SS_ARRAY)
        return 8;

    SIC_UNREACHABLE();
}

const char* type_to_string(Type* type)
{
    SIC_ASSERT(type != NULL);
    static const char* type_names[] = {
        [TYPE_VOID]    = "void",
        [TYPE_BOOL]    = "bool",
        [TYPE_U8]      = "u8",
        [TYPE_U16]     = "u16",
        [TYPE_U32]     = "u32",
        [TYPE_U64]     = "u64",
        [TYPE_S8]      = "s8",
        [TYPE_S16]     = "s16",
        [TYPE_S32]     = "s32",
        [TYPE_S64]     = "s64",
        [TYPE_F32]     = "f32",
        [TYPE_F64]     = "f64",
    };

    if(type_is_builtin(type))
        return type_names[type->kind];
    if(type->kind == TYPE_POINTER)
        return str_format("%s*", type_to_string(type->pointer_base));
    if(type->kind == TYPE_DS_ARRAY)
        return str_format("%s[]", type_to_string(type->pre_sema_array.elem_type));
    if(type->kind == TYPE_SS_ARRAY)
        return str_format("%s[%lu]", type_to_string(type->ss_array.elem_type), type->ss_array.elem_cnt);

    SIC_UNREACHABLE();
}
