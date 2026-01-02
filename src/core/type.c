#include "internal.h"

#define TYPE_DEF(name, type, ...)   \
static Type name =                  \
{                                   \
    .kind = type,                   \
    .status = STATUS_RESOLVED,      \
    .visibility = VIS_PUBLIC,       \
    .canonical = &name,             \
    __VA_ARGS__                     \
}

#define BUILTIN_TYPE_DEF(name, type, size)  \
static Type name =                          \
{                                           \
    .kind = type,                           \
    .status = STATUS_RESOLVED,              \
    .visibility = VIS_PUBLIC,               \
    .canonical = &name,                     \
    .builtin = {                            \
        .bit_size  = size * 8,              \
        .byte_size = size                   \
    }                                       \
}

#define ALIAS_TYPE_DEF(name, obj_name)  \
static Type name;                       \
static ObjTypedef obj_name =            \
{                                       \
    .header = {                         \
        .kind = OBJ_TYPEDEF,            \
        .visibility = VIS_PUBLIC,       \
        .status = STATUS_RESOLVED       \
    },                                  \
    .type_ref = &name                   \
};                                      \
static Type name =                      \
{                                       \
    .kind = TYPE_ALIAS,                 \
    .status = STATUS_RESOLVED,          \
    .visibility = VIS_PUBLIC,           \
    .typedef_ = &obj_name,              \
}

static Type s_voidptr;

BUILTIN_TYPE_DEF(s_bool  , TYPE_BOOL  , 1);
BUILTIN_TYPE_DEF(s_char  , TYPE_CHAR  , 1);
BUILTIN_TYPE_DEF(s_byte  , TYPE_BYTE  , 1);
BUILTIN_TYPE_DEF(s_ubyte , TYPE_UBYTE , 1);
BUILTIN_TYPE_DEF(s_short , TYPE_SHORT , 2);
BUILTIN_TYPE_DEF(s_ushort, TYPE_USHORT, 2);
BUILTIN_TYPE_DEF(s_int   , TYPE_INT   , 4);
BUILTIN_TYPE_DEF(s_uint  , TYPE_UINT  , 4);
BUILTIN_TYPE_DEF(s_long  , TYPE_LONG  , 8);
BUILTIN_TYPE_DEF(s_ulong , TYPE_ULONG , 8);
BUILTIN_TYPE_DEF(s_float , TYPE_FLOAT , 4);
BUILTIN_TYPE_DEF(s_double, TYPE_DOUBLE, 8);

TYPE_DEF(s_invalid , TYPE_INVALID);
TYPE_DEF(s_void    , TYPE_VOID, .ptr_cache = &s_voidptr);
TYPE_DEF(s_voidptr , TYPE_POINTER, .pointer_base = &s_void);
TYPE_DEF(s_anon_arr, TYPE_ANON_ARRAY);
TYPE_DEF(s_auto    , TYPE_AUTO);

ALIAS_TYPE_DEF(s_iptr , s_iptr_obj);
ALIAS_TYPE_DEF(s_uptr , s_uptr_obj);
ALIAS_TYPE_DEF(s_isize, s_isize_obj);
ALIAS_TYPE_DEF(s_usize, s_usize_obj);

// Builtin-types
Type* const g_type_invalid  = &s_invalid;
Type* const g_type_voidptr  = &s_voidptr;
Type* const g_type_void     = &s_void;
Type* const g_type_bool     = &s_bool;
Type* const g_type_char     = &s_char;
Type* const g_type_byte     = &s_byte;
Type* const g_type_ubyte    = &s_ubyte;
Type* const g_type_short    = &s_short;
Type* const g_type_ushort   = &s_ushort;
Type* const g_type_int      = &s_int;
Type* const g_type_uint     = &s_uint;
Type* const g_type_long     = &s_long;
Type* const g_type_ulong    = &s_ulong;
Type* const g_type_float    = &s_float;
Type* const g_type_double   = &s_double;
Type* const g_type_anon_arr = &s_anon_arr;
Type* const g_type_auto     = &s_auto;
Type* const g_type_iptr     = &s_iptr;
Type* const g_type_uptr     = &s_uptr;
Type* const g_type_isize    = &s_isize;
Type* const g_type_usize    = &s_usize;

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
    [TOKEN_ISIZE   - TOKEN_TYPENAME_START] = &s_isize,
    [TOKEN_USIZE   - TOKEN_TYPENAME_START] = &s_usize,
    [TOKEN_FLOAT   - TOKEN_TYPENAME_START] = &s_float,
    [TOKEN_DOUBLE  - TOKEN_TYPENAME_START] = &s_double,
};

void builtin_type_init()
{
    // TODO: Make this platform dependent, as that is the whole point of these types.
    //       For right now I am not doing proper platform detection, so they will always
    //       be the same size.
    s_iptr_obj.header.symbol  = tok_kind_to_str(TOKEN_IPTR);
    s_iptr_obj.alias.type     = &s_long;
    s_iptr.canonical          = &s_long;

    s_uptr_obj.header.symbol  = tok_kind_to_str(TOKEN_UPTR);
    s_uptr_obj.alias.type     = &s_ulong;
    s_uptr.canonical          = &s_ulong;

    s_isize_obj.header.symbol = tok_kind_to_str(TOKEN_ISIZE);
    s_isize_obj.alias.type    = &s_long;
    s_isize.canonical         = &s_long;

    s_usize_obj.header.symbol = tok_kind_to_str(TOKEN_USIZE);
    s_usize_obj.alias.type    = &s_ulong;
    s_usize.canonical         = &s_ulong;
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
        base->ptr_cache->canonical = base->ptr_cache;
    }
    return base->ptr_cache;
}

Type* type_func_ptr(FuncSignature* signature)
{
    SIC_ASSERT(signature != NULL);
    Type* new_type = CALLOC_STRUCT(Type);
    new_type->kind = TYPE_FUNC_PTR;
    new_type->func_ptr = signature;
    new_type->canonical = new_type;
    return new_type;
}

Type* type_array_of(Type* elem_ty, ASTExpr* size_expr)
{
    SIC_ASSERT(elem_ty != NULL);
    Type* new_type = CALLOC_STRUCT(Type);
    new_type->kind = TYPE_PS_ARRAY;
    new_type->array.elem_type = elem_ty;
    new_type->array.size_expr = size_expr;
    new_type->canonical = new_type;
    return new_type;
}

Type* type_reduce(Type* t)
{
    while(true)
    {
        t = t->canonical;
        switch(t->kind)
        {
        case TYPE_ALIAS_DISTINCT:
            t = t->typedef_->alias.type;
            continue;
        default:
            return t;
        }
    }
}

bool type_equal(Type* t1, Type* t2)
{
    SIC_ASSERT(t1 != NULL);
    SIC_ASSERT(t2 != NULL);
    t1 = t1->canonical;
    t2 = t2->canonical;
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
           !type_equal(s1->ret_type.type, s2->ret_type.type))
            return false;
        for(uint32_t i = 0; i < s1->params.size; ++i)
            if(!type_equal(s1->params.data[i]->type_loc.type, s2->params.data[i]->type_loc.type))
                return false;
        return true;
    }
    case TYPE_STATIC_ARRAY:
        return t1->array.static_len == t2->array.static_len &&
               type_equal(t1->array.elem_type, t2->array.elem_type);
    case TYPE_RUNTIME_ARRAY:
        return false;
    case TYPE_ALIAS:
    case TYPE_ALIAS_DISTINCT:
        return t1->typedef_ == t2->typedef_;
    case TYPE_ENUM:
    case TYPE_ENUM_DISTINCT:
        return t1->enum_ == t2->enum_;
    case TYPE_STRUCT:
    case TYPE_UNION:
        return t1->struct_ == t2->struct_;
    case TYPE_INVALID:
    case TYPE_ANON_ARRAY:
    case TYPE_AUTO:
    case TYPE_PS_ARRAY:
    case TYPE_PS_USER:
    case TYPE_TYPEOF:
    case __TYPE_COUNT:
        break;
    }
    SIC_UNREACHABLE();
}

ByteSize type_size(Type* ty)
{
    SIC_ASSERT(ty != NULL);
    ty = ty->canonical;
    switch(ty->kind)
    {
    case INT_TYPES:
    case FLOAT_TYPES:
        return ty->builtin.byte_size;
    case TYPE_POINTER:
    case TYPE_FUNC_PTR:
        return 8;
    case TYPE_STATIC_ARRAY:
        return type_size(ty->array.elem_type) * ty->array.static_len;
    case TYPE_ALIAS_DISTINCT:
        return type_size(ty->typedef_->alias.type);
    case TYPE_ENUM_DISTINCT:
        return type_size(ty->enum_->underlying.type);
    case TYPE_STRUCT:
        return ty->struct_->size;
    case TYPE_UNION:
        return type_size(ty->struct_->largest_type);
    case TYPE_INVALID:
    case TYPE_VOID:
    case TYPE_RUNTIME_ARRAY:
    case TYPE_ALIAS:
    case TYPE_ENUM:
    case TYPE_ANON_ARRAY:
    case TYPE_AUTO:
    case TYPE_PS_ARRAY:
    case TYPE_PS_USER:
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
    ty = ty->canonical;
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
    case TYPE_ALIAS_DISTINCT:
        return type_alignment(ty->typedef_->alias.type);
    case TYPE_ENUM_DISTINCT:
        return type_alignment(ty->enum_->underlying.type);
    case TYPE_STRUCT:
        return ty->struct_->align;
    case TYPE_UNION:
        return type_alignment(ty->struct_->largest_type);
    case TYPE_INVALID:
    case TYPE_VOID:
    case TYPE_ALIAS:
    case TYPE_ENUM:
    case TYPE_ANON_ARRAY:
    case TYPE_AUTO:
    case TYPE_PS_ARRAY:
    case TYPE_PS_USER:
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
            scratch_append(type_to_string(sig->params.data[0]->type_loc.type));
        for(uint32_t i = 1; i < sig->params.size; ++i)
        {
            scratch_appendc(',');
            scratch_append(type_to_string(sig->params.data[i]->type_loc.type));
        }
        scratch_append(") -> ");
        scratch_append(type_to_string(sig->ret_type.type));
        return str_dupn(scratch_string(), g_scratch.len);
    }
    case TYPE_STATIC_ARRAY:
        return str_format("%s[%lu]", type_to_string(type->array.elem_type), type->array.static_len);
    case TYPE_RUNTIME_ARRAY:
        return str_format("%s[]", type_to_string(type->array.elem_type));
    case TYPE_ALIAS:
        return str_format("%s (a.k.a. %s)", type->typedef_->header.symbol, type_to_string(type->canonical));
    case TYPE_ALIAS_DISTINCT:
        return str_format("%s", type->typedef_->header.symbol);
    case TYPE_ENUM:
    case TYPE_ENUM_DISTINCT:
        return str_format("%s", type->enum_->header.symbol);
    case TYPE_STRUCT:
    case TYPE_UNION:
        return str_format("%s", type->struct_->header.symbol);
    case TYPE_PS_ARRAY:
        return str_format("%s[*]", type_to_string(type->array.elem_type));
    case TYPE_INVALID:
    case TYPE_PS_USER:
    case TYPE_ANON_ARRAY:
    case TYPE_AUTO:
    case TYPE_TYPEOF:
    case __TYPE_COUNT:
        break;
    }

    SIC_UNREACHABLE();
}
