#include "internal.h"
#include "semantics.h"

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
    .user_def = &obj_name.header,       \
}

static Type s_voidptr;

BUILTIN_TYPE_DEF(s_bool   , TYPE_BOOL   , 1);
BUILTIN_TYPE_DEF(s_char   , TYPE_CHAR   , 1);
BUILTIN_TYPE_DEF(s_char16 , TYPE_CHAR16 , 2);
BUILTIN_TYPE_DEF(s_char32 , TYPE_CHAR32 , 4);
BUILTIN_TYPE_DEF(s_byte   , TYPE_BYTE   , 1);
BUILTIN_TYPE_DEF(s_ubyte  , TYPE_UBYTE  , 1);
BUILTIN_TYPE_DEF(s_short  , TYPE_SHORT  , 2);
BUILTIN_TYPE_DEF(s_ushort , TYPE_USHORT , 2);
BUILTIN_TYPE_DEF(s_int    , TYPE_INT    , 4);
BUILTIN_TYPE_DEF(s_uint   , TYPE_UINT   , 4);
BUILTIN_TYPE_DEF(s_long   , TYPE_LONG   , 8);
BUILTIN_TYPE_DEF(s_ulong  , TYPE_ULONG  , 8);
BUILTIN_TYPE_DEF(s_int128 , TYPE_INT128 , 16);
BUILTIN_TYPE_DEF(s_uint128, TYPE_UINT128, 16);
BUILTIN_TYPE_DEF(s_float  , TYPE_FLOAT  , 4);
BUILTIN_TYPE_DEF(s_double , TYPE_DOUBLE , 8);
BUILTIN_TYPE_DEF(s_pos_int_lit, TYPE_UINT128, 16);
BUILTIN_TYPE_DEF(s_neg_int_lit, TYPE_INT128, 16);

TYPE_DEF(s_invalid    , TYPE_INVALID);
// Void is a special case
static Type s_void = {
    .kind = TYPE_VOID,
    .status = STATUS_RESOLVING,
    .visibility = VIS_PUBLIC,
    .canonical = &s_void,
    .cache = &s_voidptr,
};
TYPE_DEF(s_voidptr    , TYPE_POINTER_SINGLE, .pointer.base = &s_void);
TYPE_DEF(s_init_list  , TYPE_INIT_LIST);
TYPE_DEF(s_str_lit    , TYPE_STRING_LITERAL);

ALIAS_TYPE_DEF(s_iptr , s_iptr_obj);
ALIAS_TYPE_DEF(s_uptr , s_uptr_obj);
ALIAS_TYPE_DEF(s_isize, s_isize_obj);
ALIAS_TYPE_DEF(s_usize, s_usize_obj);

// Builtin-types
Type* const g_type_invalid     = &s_invalid;
Type* const g_type_voidptr     = &s_voidptr;
Type* const g_type_void        = &s_void;
Type* const g_type_bool        = &s_bool;
Type* const g_type_char        = &s_char;
Type* const g_type_char16      = &s_char16;
Type* const g_type_char32      = &s_char32;
Type* const g_type_byte        = &s_byte;
Type* const g_type_ubyte       = &s_ubyte;
Type* const g_type_short       = &s_short;
Type* const g_type_ushort      = &s_ushort;
Type* const g_type_int         = &s_int;
Type* const g_type_uint        = &s_uint;
Type* const g_type_long        = &s_long;
Type* const g_type_ulong       = &s_ulong;
Type* const g_type_int128      = &s_int128;
Type* const g_type_uint128     = &s_uint128;
Type* const g_type_float       = &s_float;
Type* const g_type_double      = &s_double;
Type* const g_type_iptr        = &s_iptr;
Type* const g_type_uptr        = &s_uptr;
Type* const g_type_isize       = &s_isize;
Type* const g_type_usize       = &s_usize;

Type* const g_type_init_list   = &s_init_list;
Type* const g_type_pos_int_lit = &s_pos_int_lit;
Type* const g_type_neg_int_lit = &s_neg_int_lit;
Type* const g_type_str_lit     = &s_str_lit;

static Type* const builtin_type_lookup[TOKEN_TYPENAME_END - TOKEN_TYPENAME_START + 1] = {
    [TOKEN_VOID    - TOKEN_TYPENAME_START] = &s_void,
    [TOKEN_BOOL    - TOKEN_TYPENAME_START] = &s_bool,
    [TOKEN_CHAR    - TOKEN_TYPENAME_START] = &s_char,
    [TOKEN_CHAR16  - TOKEN_TYPENAME_START] = &s_char16,
    [TOKEN_CHAR32  - TOKEN_TYPENAME_START] = &s_char32,
    [TOKEN_BYTE    - TOKEN_TYPENAME_START] = &s_byte,
    [TOKEN_UBYTE   - TOKEN_TYPENAME_START] = &s_ubyte,
    [TOKEN_SHORT   - TOKEN_TYPENAME_START] = &s_short,
    [TOKEN_USHORT  - TOKEN_TYPENAME_START] = &s_ushort,
    [TOKEN_INT     - TOKEN_TYPENAME_START] = &s_int,
    [TOKEN_UINT    - TOKEN_TYPENAME_START] = &s_uint,
    [TOKEN_LONG    - TOKEN_TYPENAME_START] = &s_long,
    [TOKEN_ULONG   - TOKEN_TYPENAME_START] = &s_ulong,
    [TOKEN_INT128  - TOKEN_TYPENAME_START] = &s_int128,
    [TOKEN_UINT128 - TOKEN_TYPENAME_START] = &s_uint128,
    [TOKEN_FLOAT   - TOKEN_TYPENAME_START] = &s_float,
    [TOKEN_DOUBLE  - TOKEN_TYPENAME_START] = &s_double,
    [TOKEN_IPTR    - TOKEN_TYPENAME_START] = &s_iptr,
    [TOKEN_UPTR    - TOKEN_TYPENAME_START] = &s_uptr,
    [TOKEN_ISIZE   - TOKEN_TYPENAME_START] = &s_isize,
    [TOKEN_USIZE   - TOKEN_TYPENAME_START] = &s_usize,
};

void builtin_type_init()
{
    // TODO: Make this platform dependent, as that is the whole point of these types.
    //       For right now I am not doing proper platform detection, so they will always
    //       be the same size.
    s_iptr_obj.header.sym  = tok_kind_to_str(TOKEN_IPTR);
    s_iptr_obj.alias.type     = &s_long;
    s_iptr.canonical          = &s_long;

    s_uptr_obj.header.sym  = tok_kind_to_str(TOKEN_UPTR);
    s_uptr_obj.alias.type     = &s_ulong;
    s_uptr.canonical          = &s_ulong;

    s_isize_obj.header.sym = tok_kind_to_str(TOKEN_ISIZE);
    s_isize_obj.alias.type    = &s_long;
    s_isize.canonical         = &s_long;

    s_usize_obj.header.sym = tok_kind_to_str(TOKEN_USIZE);
    s_usize_obj.alias.type    = &s_ulong;
    s_usize.canonical         = &s_ulong;
}

Type* type_from_token(TokenKind type_token)
{
    DBG_ASSERT(token_is_typename(type_token));
    return builtin_type_lookup[type_token - TOKEN_TYPENAME_START];
}

Type* type_copy(const Type* other)
{
    Type* res = MALLOC_STRUCT(Type);
    memcpy(res, other, sizeof(Type));
    return res;
}

Type* type_apply_qualifiers(Type* base, TypeQualifiers qualifiers)
{
    DBG_ASSERT(base != NULL);
    if((base->qualifiers & qualifiers) == qualifiers) return base;
    Type* res = type_copy(base);
    res->qualifiers = qualifiers;
    res->cache = NULL;
    return res;
}

Type* type_pointer_to_single(Type* base)
{
    DBG_ASSERT(base != NULL);
    if(base->cache == NULL)
    {
        base->cache = CALLOC_STRUCT(Type);
        base->cache->kind = TYPE_POINTER_SINGLE;
        base->cache->pointer.base = base;
        base->cache->canonical = base->cache;
        if(base->status == STATUS_RESOLVED)
        {
            base->cache->status = STATUS_RESOLVED;
            base->cache->visibility = base->visibility;
        }
    }
    return base->cache;
}

Type* type_pointer_to_multi(Type* base, ASTExpr* size_expr)
{
    DBG_ASSERT(base != NULL);
    Type* new_type = CALLOC_STRUCT(Type);
    new_type->kind = TYPE_POINTER_MULTI;
    new_type->pointer.base = base;
    new_type->pointer.size_expr = size_expr;
    new_type->canonical = new_type;
    if(size_expr == NULL && base->status == STATUS_RESOLVED)
    {
        new_type->status = STATUS_RESOLVED;
        new_type->visibility = base->visibility;
    }
    return new_type;
}

Type* type_func_ptr(FuncSignature* signature)
{
    DBG_ASSERT(signature != NULL);
    Type* new_type = CALLOC_STRUCT(Type);
    new_type->kind = TYPE_FUNC_PTR;
    new_type->func_ptr = signature;
    new_type->canonical = new_type;
    return new_type;
}

Type* type_array_of(Type* elem_ty, ASTExpr* size_expr, TypeKind kind)
{
    DBG_ASSERT(elem_ty != NULL);
    Type* new_type = CALLOC_STRUCT(Type);
    new_type->kind = kind;
    new_type->qualifiers = elem_ty->qualifiers;
    new_type->array.elem_type = elem_ty;
    new_type->array.size_expr = size_expr;
    new_type->canonical = new_type;
    return new_type;
}

Type* type_slice_of(Type* elem_ty)
{
    DBG_ASSERT(elem_ty != NULL);
    Type* new_type = CALLOC_STRUCT(Type);
    new_type->kind = TYPE_SLICE;
    new_type->canonical = new_type;
    new_type->slice.base = elem_ty;
    return new_type;
}

Type* type_reduce(Type* t)
{
    DBG_ASSERT(t != NULL);
    while(true)
    {
        switch(t->kind)
        {
        case TYPE_ALIAS:
        case TYPE_ENUM:
            t = t->canonical;
            continue;
        case TYPE_ALIAS_DISTINCT:
            t = obj_as_typedef(t->user_def)->alias.type;
            continue;
        case TYPE_ENUM_DISTINCT:
            t = obj_as_enum(t->user_def)->underlying.type;
            continue;
        default:
            return t;
        }
    }
}

bool type_equal(const Type* t1, const Type* t2)
{
    DBG_ASSERT(t1 != NULL);
    DBG_ASSERT(t2 != NULL);
    DBG_ASSERT(t1->canonical != NULL);
    DBG_ASSERT(t2->canonical != NULL);
    t1 = t1->canonical;
    t2 = t2->canonical;
    if(t1->kind != t2->kind)
        return false;
    switch(t1->kind)
    {
    case TYPE_VOID:
    case TYPE_BOOL:
    case CHAR_TYPES:
    case INT_TYPES:
    case FLOAT_TYPES:
        return true;
    case TYPE_POINTER_MULTI:
        if(t1->pointer.static_len != t2->pointer.static_len) return false;
        FALLTHROUGH;
    case TYPE_POINTER_SINGLE:
    case TYPE_SLICE:
        return type_equal(t1->pointer.base, t2->pointer.base) && 
               t1->pointer.base->qualifiers == t2->pointer.base->qualifiers;
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
    case TYPE_ALIAS_DISTINCT:
    case TYPE_ENUM_DISTINCT:
    case TYPE_STRUCT:
    case TYPE_UNION:
        return t1->user_def == t2->user_def;
    case TYPE_INVALID:
    case TYPE_ALIAS:
    case TYPE_ENUM:
    case SEMA_ONLY_TYPES:
        break;
    }
    SIC_UNREACHABLE();
}

ByteSize type_size(const Type* ty)
{
    DBG_ASSERT(ty != NULL);
    DBG_ASSERT(ty->status == STATUS_RESOLVED);
RETRY:
    switch(ty->kind)
    {
    case TYPE_BOOL:
    case CHAR_TYPES:
    case INT_TYPES:
    case FLOAT_TYPES:
        return ty->builtin.byte_size;
    case TYPE_POINTER_SINGLE:
    case TYPE_POINTER_MULTI:
    case TYPE_FUNC_PTR:
        return g_compiler.target.ptr_size;
    case TYPE_STATIC_ARRAY:
        return type_size(ty->array.elem_type) * ty->array.static_len;
    case TYPE_SLICE:
        // TODO: Make this simpler, or better yet, cache the size/alignment of types somewhere else.
        return ALIGN_UP(g_compiler.target.ptr_size + type_size(g_type_usize), type_alignment(ty));
    case TYPE_ALIAS:
    case TYPE_ENUM:
        ty = ty->canonical;
        goto RETRY;
    case TYPE_ALIAS_DISTINCT:
        ty = obj_as_typedef(ty->user_def)->alias.type;
        goto RETRY;
    case TYPE_ENUM_DISTINCT:
        ty = obj_as_enum(ty->user_def)->underlying.type;
        goto RETRY;
    case TYPE_STRUCT:
        return obj_as_struct(ty->user_def)->size;
    case TYPE_UNION:
        ty = obj_as_struct(ty->user_def)->largest_type;
        goto RETRY;
    case TYPE_INVALID:
    case TYPE_VOID:
    case SEMA_ONLY_TYPES:
        break;
    }
    SIC_UNREACHABLE();
}

ByteSize type_alignment(const Type* ty)
{
    DBG_ASSERT(ty != NULL);
RETRY:
    switch(ty->kind)
    {
    case TYPE_VOID:
        return 0;
    case NUMERIC_TYPES:
        return ty->builtin.byte_size;
    case TYPE_POINTER_SINGLE:
    case TYPE_POINTER_MULTI:
    case TYPE_FUNC_PTR:
        return g_compiler.target.ptr_size;
    case TYPE_STATIC_ARRAY:
        ty = ty->array.elem_type;
        goto RETRY;
    case TYPE_SLICE:
        return MAX(g_compiler.target.ptr_size, type_alignment(g_type_usize));
    case TYPE_ALIAS:
    case TYPE_ENUM:
        ty = ty->canonical;
        goto RETRY;
    case TYPE_ALIAS_DISTINCT:
        ty = obj_as_typedef(ty->user_def)->alias.type;
        goto RETRY;
    case TYPE_ENUM_DISTINCT:
        ty = obj_as_enum(ty->user_def)->underlying.type;
        goto RETRY;
    case TYPE_STRUCT:
        return obj_as_struct(ty->user_def)->align;
    case TYPE_UNION:
        ty = obj_as_struct(ty->user_def)->largest_type;
        goto RETRY;
    case TYPE_INVALID:
    case SEMA_ONLY_TYPES:
        break;
    }

    SIC_UNREACHABLE();
}

const char* type_to_string(const Type* type)
{
    DBG_ASSERT(type != NULL);
    DBG_ASSERT(type->status != STATUS_UNRESOLVED);
    // TODO: Improve this
    const char* res;
    switch(type->kind)
    {
    case TYPE_VOID:
    case NUMERIC_TYPES:
        static_assert(TYPE_INT - TYPE_VOID + TOKEN_VOID == TOKEN_INT, "Check enum conversion");
        res = tok_kind_to_str(type->kind - TYPE_VOID + TOKEN_VOID); // Convert type enum to token enum
        break;
    case TYPE_POINTER_SINGLE:
        res = str_format("*%s", type_to_string(type->pointer.base));
        break;
    case TYPE_POINTER_MULTI:
        if(type->pointer.static_len == 0)
            res = str_format("*[*]%s", type_to_string(type->pointer.base));
        else
            res = str_format("*[%lu]%s", type->pointer.static_len, type_to_string(type->pointer.base));
        break;
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
        res = str_dupn(scratch_string(), g_scratch.len);
        break;
    }
    case TYPE_SLICE:
        res = str_format("[]%s", type_to_string(type->pointer.base));
        break;
    case TYPE_STATIC_ARRAY:
        // Dont print qualifiers for array, they shouldn't have any
        return str_format("[%lu]%s", type->array.static_len, type_to_string(type->array.elem_type));
    case TYPE_ALIAS:
        // TODO: Probably needs changing. This isnt a very good way to print the type, but I want to
        // somehow express that it is an alias and not a distinct type.
        res = str_format("%s (a.k.a. %s)", obj_as_typedef(type->user_def)->header.sym, type_to_string(type->canonical));
        break;
    case TYPE_ALIAS_DISTINCT:
        res = obj_as_typedef(type->user_def)->header.sym;
        break;
    case TYPE_ENUM:
    case TYPE_ENUM_DISTINCT:
        res = obj_as_enum(type->user_def)->header.sym;
        break;
    case TYPE_STRUCT: {
        Symbol s = obj_as_struct(type->user_def)->header.sym;
        res = s ? s : "anonymous struct";
        break;
    }
    case TYPE_UNION: {
        Symbol s = obj_as_struct(type->user_def)->header.sym;
        res = s ? s : "anonymous union";
        break;
    }
    case TYPE_INVALID:
    case SEMA_ONLY_TYPES:
        SIC_UNREACHABLE();
    }
    if(type->qualifiers & TYPE_QUAL_CONST)
        res = str_format("const %s", res);
    return res;
}

bool type_kind_is_signed(TypeKind kind)
{
    switch(kind)
    {
    case TYPE_BYTE:
    case TYPE_SHORT:
    case TYPE_INT:
    case TYPE_LONG:
    case TYPE_INT128:
        return true;
    case TYPE_CHAR:
    case TYPE_CHAR16:
    case TYPE_CHAR32:
    case TYPE_UBYTE:
    case TYPE_USHORT:
    case TYPE_UINT:
    case TYPE_ULONG:
    case TYPE_UINT128:
        return false;
    default:
        SIC_UNREACHABLE();
    }
}
