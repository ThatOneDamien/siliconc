#include "internal.h"

#define TYPE_DEF(type, ...)     \
{                               \
    .kind = type,               \
    .status = STATUS_RESOLVED,  \
    .visibility = VIS_PUBLIC,   \
    __VA_ARGS__                 \
}

#define BUILTIN_DEF(type, size, ptr)    \
{                                       \
    .kind = type,                       \
    .status = STATUS_RESOLVED,          \
    .visibility = VIS_PUBLIC,           \
    .builtin = {                        \
        .bit_size  = size * 8,          \
        .byte_size = size               \
    },                                  \
    .ptr_cache = ptr,                   \
}

static Type s_invalid = TYPE_DEF(TYPE_INVALID);
static Type s_voidptr;
static Type s_void    = BUILTIN_DEF(TYPE_VOID  , 0, &s_voidptr);
static Type s_bool    = BUILTIN_DEF(TYPE_BOOL  , 1, NULL);
static Type s_char    = BUILTIN_DEF(TYPE_CHAR  , 1, NULL);
static Type s_byte    = BUILTIN_DEF(TYPE_BYTE  , 1, NULL);
static Type s_ubyte   = BUILTIN_DEF(TYPE_UBYTE , 1, NULL);
static Type s_short   = BUILTIN_DEF(TYPE_SHORT , 2, NULL);
static Type s_ushort  = BUILTIN_DEF(TYPE_USHORT, 2, NULL);
static Type s_int     = BUILTIN_DEF(TYPE_INT   , 4, NULL);
static Type s_uint    = BUILTIN_DEF(TYPE_UINT  , 4, NULL);
static Type s_long    = BUILTIN_DEF(TYPE_LONG  , 8, NULL);
static Type s_ulong   = BUILTIN_DEF(TYPE_ULONG , 8, NULL);
static Type s_iptr    = BUILTIN_DEF(TYPE_IPTR  , 0, NULL);
static Type s_uptr    = BUILTIN_DEF(TYPE_UPTR  , 0, NULL);
static Type s_isz     = BUILTIN_DEF(TYPE_ISZ   , 0, NULL);
static Type s_usz     = BUILTIN_DEF(TYPE_USZ   , 0, NULL);
static Type s_float   = BUILTIN_DEF(TYPE_FLOAT , 4, NULL);
static Type s_double  = BUILTIN_DEF(TYPE_DOUBLE, 8, NULL);
static Type s_voidptr = TYPE_DEF(TYPE_POINTER, .pointer_base = &s_void);
static Type s_strlit  = TYPE_DEF(TYPE_STRING_LITERAL);

// Builtin-types
Type* const g_type_invalid = &s_invalid;
Type* const g_type_voidptr = &s_voidptr;
Type* const g_type_void    = &s_void;
Type* const g_type_bool    = &s_bool;
Type* const g_type_char    = &s_char;
Type* const g_type_byte    = &s_byte;
Type* const g_type_ubyte   = &s_ubyte;
Type* const g_type_short   = &s_short;
Type* const g_type_ushort  = &s_ushort;
Type* const g_type_int     = &s_int;
Type* const g_type_uint    = &s_uint;
Type* const g_type_long    = &s_long;
Type* const g_type_ulong   = &s_ulong;
Type* const g_type_iptr    = &s_iptr;
Type* const g_type_uptr    = &s_uptr;
Type* const g_type_isz     = &s_isz;
Type* const g_type_usz     = &s_usz;
Type* const g_type_float   = &s_float;
Type* const g_type_double  = &s_double;
Type* const g_type_strlit  = &s_strlit;


static Type* builtin_type_lookup[] = {
    [TOKEN_VOID    - TOKEN_TYPENAME_START] = &s_void,
    [TOKEN_BOOL    - TOKEN_TYPENAME_START] = &s_bool,
    [TOKEN_CHAR    - TOKEN_TYPENAME_START] = &s_char,
    [TOKEN_BYTE    - TOKEN_TYPENAME_START] = &s_byte,
    [TOKEN_UBYTE   - TOKEN_TYPENAME_START] = &s_ubyte,
    [TOKEN_SHORT   - TOKEN_TYPENAME_START] = &s_short,
    [TOKEN_USHORT  - TOKEN_TYPENAME_START] = &s_ushort,
    [TOKEN_INT     - TOKEN_TYPENAME_START] = &s_int,
    [TOKEN_UINT    - TOKEN_TYPENAME_START] = &s_uint,
    [TOKEN_LONG    - TOKEN_TYPENAME_START] = &s_long,
    [TOKEN_ULONG   - TOKEN_TYPENAME_START] = &s_ulong,
    [TOKEN_IPTR    - TOKEN_TYPENAME_START] = &s_iptr,
    [TOKEN_UPTR    - TOKEN_TYPENAME_START] = &s_uptr,
    [TOKEN_ISZ     - TOKEN_TYPENAME_START] = &s_isz,
    [TOKEN_USZ     - TOKEN_TYPENAME_START] = &s_usz,
    [TOKEN_FLOAT   - TOKEN_TYPENAME_START] = &s_float,
    [TOKEN_DOUBLE  - TOKEN_TYPENAME_START] = &s_double,
};

void builtin_type_init()
{
    // TODO: Make this platform dependent, as that is the whole point of these types.
    //       For right now I am not doing proper platform detection, so they will always
    //       be the same size.
    s_iptr.builtin.bit_size = 64;
    s_iptr.builtin.byte_size = 8;
    s_uptr.builtin.bit_size = 64;
    s_uptr.builtin.byte_size = 8;
    s_isz.builtin.bit_size = 64;
    s_isz.builtin.byte_size = 8;
    s_usz.builtin.bit_size = 64;
    s_usz.builtin.byte_size = 8;
}

Type* type_from_token(TokenKind type_token)
{
    SIC_ASSERT(token_is_typename(type_token));
    return builtin_type_lookup[type_token - TOKEN_TYPENAME_START];
}

Type* type_pointer_to(Type* base)
{
    SIC_ASSERT(base != NULL);
    if(base->ptr_cache == NULL)
    {
        base->ptr_cache = CALLOC_STRUCT(Type);
        base->ptr_cache->kind = TYPE_POINTER;
        base->ptr_cache->pointer_base = base;
    }
    return base->ptr_cache;
}

Type* type_func_ptr(FuncSignature* signature)
{
    SIC_ASSERT(signature != NULL);
    Type* new_type = CALLOC_STRUCT(Type);
    new_type->kind = TYPE_FUNC_PTR;
    new_type->func_ptr = signature;
    return new_type;
}

Type* type_array_of(Type* elem_ty, ASTExpr* size_expr)
{
    SIC_ASSERT(elem_ty != NULL);
    SIC_ASSERT(size_expr != NULL);
    Type* new_type = CALLOC_STRUCT(Type);
    new_type->kind = TYPE_PRE_SEMA_ARRAY;
    new_type->array.elem_type = elem_ty;
    new_type->array.size_expr = size_expr;
    return new_type;
}

bool type_equal(Type* t1, Type* t2)
{
    SIC_ASSERT(t1 != NULL);
    SIC_ASSERT(t2 != NULL);
    if(t1->kind != t2->kind)
        return false;
    switch(t1->kind)
    {
    case TYPE_VOID:
    case INT_TYPES:
    case FLOAT_TYPES:
        return true;
    case TYPE_POINTER:
        return type_equal(t1->pointer_base, t2->pointer_base);
    case TYPE_FUNC_PTR: {
        FuncSignature* s1 = t1->func_ptr;
        FuncSignature* s2 = t2->func_ptr;
        if(s1->is_var_arg != s2->is_var_arg || 
           s1->params.size != s2->params.size ||
           !type_equal(s1->ret_type, s2->ret_type))
            return false;
        for(uint32_t i = 0; i < s1->params.size; ++i)
            if(!type_equal(s1->params.data[i]->type, s2->params.data[i]->type))
                return false;
        return true;
    }
    case TYPE_STATIC_ARRAY:
        return t1->array.ss_size == t2->array.ss_size &&
               type_equal(t1->array.elem_type, t2->array.elem_type);
    case TYPE_RUNTIME_ARRAY:
        return false;
    case TYPE_ENUM:
    case TYPE_STRUCT:
    case TYPE_TYPEDEF:
    case TYPE_UNION:
        return t1->user_def == t2->user_def;
    case TYPE_INVALID:
    case TYPE_PRE_SEMA_ARRAY:
    case TYPE_PRE_SEMA_USER:
    case TYPE_STRING_LITERAL:
    case TYPE_AUTO:
    case TYPE_TYPEOF:
    case __TYPE_COUNT:
        break;
    }
    SIC_UNREACHABLE();
}

ByteSize type_size(Type* ty)
{
    SIC_ASSERT(ty != NULL);
    switch(ty->kind)
    {
    case INT_TYPES:
    case FLOAT_TYPES:
        return ty->builtin.byte_size;
    case TYPE_POINTER:
    case TYPE_FUNC_PTR:
        return 8;
    case TYPE_STATIC_ARRAY:
        return type_size(ty->array.elem_type) * ty->array.ss_size;
    case TYPE_ENUM:
        return type_size(ty->user_def->enum_.underlying);
    case TYPE_TYPEDEF:
        SIC_TODO();
    case TYPE_STRUCT:
        return ty->user_def->struct_.size;
    case TYPE_UNION:
        return type_size(ty->user_def->struct_.largest_type);
    case TYPE_INVALID:
    case TYPE_VOID:
    case TYPE_RUNTIME_ARRAY:
    case TYPE_PRE_SEMA_ARRAY:
    case TYPE_PRE_SEMA_USER:
    case TYPE_STRING_LITERAL:
    case TYPE_AUTO:
    case TYPE_TYPEOF:
    case __TYPE_COUNT:
        break;
    }
    SIC_UNREACHABLE();
}

uint32_t type_alignment(Type* ty)
{
    SIC_ASSERT(ty != NULL);
    SIC_ASSERT(ty->status == STATUS_RESOLVED);
    switch(ty->kind)
    {
    case INT_TYPES:
    case FLOAT_TYPES:
        return ty->builtin.byte_size;
    case TYPE_POINTER:
    case TYPE_FUNC_PTR:
        // TODO: Make this specific to build arch
        return 8;
    case TYPE_STATIC_ARRAY:
    case TYPE_RUNTIME_ARRAY:
        return type_alignment(ty->array.elem_type);
    case TYPE_ENUM:
        return type_alignment(ty->user_def->enum_.underlying);
    case TYPE_TYPEDEF:
        SIC_TODO();
    case TYPE_STRUCT:
        return ty->user_def->struct_.align;
    case TYPE_UNION:
        return type_alignment(ty->user_def->struct_.largest_type);
    case TYPE_INVALID:
    case TYPE_VOID:
    case TYPE_PRE_SEMA_ARRAY:
    case TYPE_PRE_SEMA_USER:
    case TYPE_STRING_LITERAL:
    case TYPE_AUTO:
    case TYPE_TYPEOF:
    case __TYPE_COUNT:
        break;
    }

    SIC_UNREACHABLE();
}

const char* type_to_string(Type* type)
{
    SIC_ASSERT(type != NULL);
    SIC_ASSERT(type->status == STATUS_RESOLVED);
    switch(type->kind)
    {
    case TYPE_VOID:
    case INT_TYPES:
    case FLOAT_TYPES:
        static_assert(TYPE_INT - TYPE_VOID + TOKEN_VOID == TOKEN_INT, "Check enum conversion");
        return tok_kind_to_str(type->kind - TYPE_VOID + TOKEN_VOID); // Convert type enum to token enum
    case TYPE_POINTER:
        return str_format("%s*", type_to_string(type->pointer_base));
    case TYPE_FUNC_PTR: {
        FuncSignature* sig = type->func_ptr;
        scratch_clear();
        scratch_append("fn ");
        scratch_appendc('(');
        if(sig->params.size > 0)
            scratch_append(type_to_string(sig->params.data[0]->type));
        for(uint32_t i = 1; i < sig->params.size; ++i)
        {
            scratch_appendc(',');
            scratch_append(type_to_string(sig->params.data[i]->type));
        }
        scratch_append(") -> ");
        scratch_append(type_to_string(sig->ret_type));
        return str_dupn(scratch_string(), g_scratch.len);
    }
    case TYPE_STATIC_ARRAY:
        return str_format("%s[%lu]", type_to_string(type->array.elem_type), type->array.ss_size);
    case TYPE_RUNTIME_ARRAY:
        return str_format("%s[]", type_to_string(type->array.elem_type));
    case TYPE_ENUM:
    case TYPE_STRUCT:
    case TYPE_TYPEDEF:
    case TYPE_UNION:
        return str_format("%s", type->user_def->symbol);
    case TYPE_INVALID:
    case TYPE_PRE_SEMA_ARRAY:
    case TYPE_PRE_SEMA_USER:
    case TYPE_STRING_LITERAL:
    case TYPE_AUTO:
    case TYPE_TYPEOF:
    case __TYPE_COUNT:
        break;
    }

    SIC_UNREACHABLE();
}
