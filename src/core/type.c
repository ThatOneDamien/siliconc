#include "internal.h"
#include "utils/error.h"

#define BUILTIN_DEF(TYPE, SIZE)     \
{                                   \
    .kind = TYPE,                   \
    .status = STATUS_RESOLVED,      \
    .qualifiers = QUALIFIER_NONE,   \
    .builtin = { SIZE }             \
}

static Type s_void   = BUILTIN_DEF(TYPE_VOID   ,  0);
static Type s_bool   = BUILTIN_DEF(TYPE_BOOL   ,  1);
static Type s_ubyte  = BUILTIN_DEF(TYPE_UBYTE  ,  1);
static Type s_ushort = BUILTIN_DEF(TYPE_USHORT ,  2);
static Type s_uint   = BUILTIN_DEF(TYPE_UINT   ,  4);
static Type s_ulong  = BUILTIN_DEF(TYPE_ULONG  ,  8);
static Type s_byte   = BUILTIN_DEF(TYPE_BYTE   ,  1);
static Type s_short  = BUILTIN_DEF(TYPE_SHORT  ,  2);
static Type s_int    = BUILTIN_DEF(TYPE_INT    ,  4);
static Type s_long   = BUILTIN_DEF(TYPE_LONG   ,  8);
static Type s_float  = BUILTIN_DEF(TYPE_FLOAT  ,  4);
static Type s_double = BUILTIN_DEF(TYPE_DOUBLE ,  8);

// Builtin-types
Type* g_type_void   = &s_void;
Type* g_type_bool   = &s_bool;
Type* g_type_ubyte  = &s_ubyte;
Type* g_type_ushort = &s_ushort;
Type* g_type_uint   = &s_uint;
Type* g_type_ulong  = &s_ulong;
Type* g_type_byte   = &s_byte;
Type* g_type_short  = &s_short;
Type* g_type_int    = &s_int;
Type* g_type_long   = &s_long;
Type* g_type_float  = &s_float;
Type* g_type_double = &s_double;

static Type* builtin_type_lookup[] = {
    [TOKEN_VOID   - TOKEN_TYPENAME_START] = &s_void,
    [TOKEN_BOOL   - TOKEN_TYPENAME_START] = &s_bool,
    [TOKEN_UBYTE  - TOKEN_TYPENAME_START] = &s_ubyte,
    [TOKEN_USHORT - TOKEN_TYPENAME_START] = &s_ushort,
    [TOKEN_UINT   - TOKEN_TYPENAME_START] = &s_uint,
    [TOKEN_ULONG  - TOKEN_TYPENAME_START] = &s_ulong,
    [TOKEN_BYTE   - TOKEN_TYPENAME_START] = &s_byte,
    [TOKEN_SHORT  - TOKEN_TYPENAME_START] = &s_short,
    [TOKEN_INT    - TOKEN_TYPENAME_START] = &s_int,
    [TOKEN_LONG   - TOKEN_TYPENAME_START] = &s_long,
    [TOKEN_FLOAT  - TOKEN_TYPENAME_START] = &s_float,
    [TOKEN_DOUBLE - TOKEN_TYPENAME_START] = &s_double,
};

Type* type_from_token(TokenKind type_token)
{
    SIC_ASSERT(token_is_typename(type_token));
    return builtin_type_lookup[type_token - TOKEN_TYPENAME_START];
}

Type* type_copy(const Type* orig)
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
    case TYPE_BOOL:
    case TYPE_UBYTE:
    case TYPE_USHORT:
    case TYPE_UINT:
    case TYPE_ULONG:
    case TYPE_BYTE:
    case TYPE_SHORT:
    case TYPE_INT:
    case TYPE_LONG:
    case TYPE_FLOAT:
    case TYPE_DOUBLE:
        return true;
    case TYPE_POINTER:
        return type_equal(t1->pointer_base, t2->pointer_base);
    case TYPE_DS_ARRAY:
    case TYPE_SS_ARRAY:
        return false;
    case TYPE_ENUM:
    case TYPE_STRUCT:
    case TYPE_TYPEDEF:
    case TYPE_UNION:
        return t1->user_def == t2->user_def;
    case TYPE_INVALID:
    case TYPE_PRE_SEMA_ARRAY:
    case __TYPE_COUNT:
        break;
    }
    SIC_UNREACHABLE();
}

uint32_t type_size(Type* ty)
{
    SIC_ASSERT(ty != NULL);
    if(type_is_builtin(ty))
        return ty->builtin.size;
    if(type_is_pointer(ty))
        return 8;
    if(ty->kind == TYPE_SS_ARRAY)
        return type_size(ty->array.elem_type) * ty->array.ss_size;

    SIC_UNREACHABLE();
}

uint32_t type_alignment(Type* ty)
{
    SIC_ASSERT(ty != NULL);
    SIC_ASSERT(ty->status == STATUS_RESOLVED);
    switch(ty->kind)
    {
    case TYPE_BOOL:
    case TYPE_UBYTE:
    case TYPE_USHORT:
    case TYPE_UINT:
    case TYPE_ULONG:
    case TYPE_BYTE:
    case TYPE_SHORT:
    case TYPE_INT:
    case TYPE_LONG:
    case TYPE_FLOAT:
    case TYPE_DOUBLE:
        return ty->builtin.size;
    case TYPE_POINTER:
        return 8;
    case TYPE_SS_ARRAY:
    case TYPE_DS_ARRAY:
        return type_alignment(ty->array.elem_type);
    case TYPE_ENUM:
    case TYPE_TYPEDEF:
        SIC_TODO();
    case TYPE_STRUCT:
    case TYPE_UNION:
        return ty->user_def->struct_.size;
    case TYPE_INVALID:
    case TYPE_VOID:
    case TYPE_PRE_SEMA_ARRAY:
    case __TYPE_COUNT:
        SIC_UNREACHABLE();
    }

    SIC_UNREACHABLE();
}

const char* type_to_string(Type* type)
{
    SIC_ASSERT(type != NULL);
    static const char* type_names[] = {
        [TYPE_VOID]   = "void",
        [TYPE_BOOL]   = "bool",
        [TYPE_UBYTE]  = "ubyte",
        [TYPE_USHORT] = "ushort",
        [TYPE_UINT]   = "uint",
        [TYPE_ULONG]  = "ulong",
        [TYPE_BYTE]   = "byte",
        [TYPE_SHORT]  = "short",
        [TYPE_INT]    = "int",
        [TYPE_LONG]   = "long",
        [TYPE_FLOAT]  = "float",
        [TYPE_DOUBLE] = "double",
    };
    // TODO: Make this use the scratch buffer.

    switch(type->kind)
    {
    case TYPE_VOID:
    case TYPE_BOOL:
    case TYPE_UBYTE:
    case TYPE_USHORT:
    case TYPE_UINT:
    case TYPE_ULONG:
    case TYPE_BYTE:
    case TYPE_SHORT:
    case TYPE_INT:
    case TYPE_LONG:
    case TYPE_FLOAT:
    case TYPE_DOUBLE:
        return type_names[type->kind];
    case TYPE_POINTER:
        return str_format("%s*", type_to_string(type->pointer_base));
    case TYPE_SS_ARRAY:
        return str_format("%s[%lu]", type_to_string(type->array.elem_type), type->array.ss_size);
    case TYPE_DS_ARRAY:
        return str_format("%s[]", type_to_string(type->array.elem_type));
    case TYPE_ENUM:
    case TYPE_STRUCT:
    case TYPE_TYPEDEF:
    case TYPE_UNION:
        return str_format("%.*s", type->user_def->symbol.len, type->user_def->symbol.start);
    case TYPE_INVALID:
    case TYPE_PRE_SEMA_ARRAY:
    case __TYPE_COUNT:
        break;
    }

    SIC_UNREACHABLE();
}
