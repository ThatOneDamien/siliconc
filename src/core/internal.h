#pragma once
#include <assert.h>
#include "structs.h"
#include "utils/da.h"
#include "utils/lib.h"

extern CompilerContext g_compiler;

// Builtin types (defined in type.c)
extern Type* const g_type_invalid;
extern Type* const g_type_voidptr;
extern Type* const g_type_void;
extern Type* const g_type_bool;
extern Type* const g_type_char;
extern Type* const g_type_char16;
extern Type* const g_type_char32;
extern Type* const g_type_byte;
extern Type* const g_type_ubyte;
extern Type* const g_type_short;
extern Type* const g_type_ushort;
extern Type* const g_type_int;
extern Type* const g_type_uint;
extern Type* const g_type_long;
extern Type* const g_type_ulong;
extern Type* const g_type_int128;
extern Type* const g_type_uint128;
extern Type* const g_type_iptr;
extern Type* const g_type_uptr;
extern Type* const g_type_isize;
extern Type* const g_type_usize;
extern Type* const g_type_float;
extern Type* const g_type_double;
extern Type* const g_type_init_list;
extern Type* const g_type_str_lit;
extern Type* const g_type_auto;

extern ASTExpr* g_bad_expr;
extern ASTStmt* g_bad_stmt;

extern Symbol   g_sym_len;
extern Symbol   g_sym_main;
extern Symbol   g_attr_list[__ATTR_COUNT];

extern const uint8_t g_hex_char_to_val[256];

void run_subprocess(const char** cmd);

// Command line arguments
void process_args(int argc, char* argv[]);

// Token functions
const char*  tok_kind_to_str(TokenKind kind);
BinaryOpKind tok_to_binary_op(TokenKind kind);
UnaryOpKind  tok_to_unary_op(TokenKind kind);

// Lexer functions
Lexer lexer_from_source(FileId fileid);
void  lexer_advance(Lexer* l);
static inline Token* lexer_peek(Lexer* l)
{
    return l->la_buf.buf + l->la_buf.cur;
}
static inline Token* lexer_prev(Lexer* l)
{
    return l->la_buf.buf + l->la_buf.head;
}
static inline Token* lexer_next(Lexer* l)
{
    return l->la_buf.buf + ((l->la_buf.cur + 1) & LOOK_AHEAD_MASK);
}
static inline Token* lexer_nextnext(Lexer* l)
{
    return l->la_buf.buf + ((l->la_buf.cur + 2) & LOOK_AHEAD_MASK);
}
static inline bool token_is_typename(TokenKind kind)
{
    return kind >= TOKEN_TYPENAME_START && kind <= TOKEN_TYPENAME_END;
}

static inline bool token_is_keyword(TokenKind kind)
{
    return kind >= TOKEN_KEYWORD_START && kind <= TOKEN_KEYWORD_END;
}

// Parser functions
void parse_source_file(FileId fileid);

// Semantic analysis functions
void  analyze_module(ObjModule* module);
void  perform_cast(ASTExpr* cast);
Attr* get_builtin_attribute(Object* obj, AttrKind kind);

// Symbol map functions
void   sym_map_init(void);
Symbol sym_map_addn(const char* str, uint32_t len, TokenKind* kind);
Symbol sym_map_getn(const char* str, uint32_t len, TokenKind* kind);
static inline Symbol sym_map_add(const char* str, TokenKind* kind)
{
    return sym_map_addn(str, strlen(str), kind);
}
static inline Symbol sym_map_get(const char* str, TokenKind* kind)
{ 
    return sym_map_getn(str, strlen(str), kind);
}

// Type functions
void        builtin_type_init();
Type*       type_from_token(TokenKind type_token);
Type*       type_pointer_to(Type* base);
Type*       type_func_ptr(FuncSignature* signature);
Type*       type_array_of(Type* elem_ty, ASTExpr* size_expr);
Type*       type_reduce(Type* t);
bool        type_equal(Type* t1, Type* t2);
ByteSize    type_size(Type* ty);
ByteSize    type_alignment(Type* ty);
const char* type_to_string(Type* type);
static inline bool type_kind_is_integer(TypeKind kind)
{
    return kind >= TYPE_INTEGER_START && kind <= TYPE_INTEGER_END;
}
static inline bool type_is_integer(Type* ty) { return type_kind_is_integer(ty->kind); }

static inline Type* type_to_unsigned(Type* ty)
{
    SIC_ASSERT(type_is_integer(ty));
    switch(ty->kind)
    {
    case TYPE_BYTE:
    case TYPE_UBYTE:
        return g_type_ubyte;
    case TYPE_SHORT:
    case TYPE_USHORT:
        return g_type_ushort;
    case TYPE_INT:
    case TYPE_UINT:
        return g_type_uint;
    case TYPE_LONG:
    case TYPE_ULONG:
        return g_type_ulong;
    default:
        SIC_UNREACHABLE();
    }
}

static inline Type* type_to_signed(Type* ty)
{
    SIC_ASSERT(type_is_integer(ty));
    switch(ty->kind)
    {
    case TYPE_BYTE:
    case TYPE_UBYTE:
        return g_type_byte;
    case TYPE_SHORT:
    case TYPE_USHORT:
        return g_type_short;
    case TYPE_INT:
    case TYPE_UINT:
        return g_type_int;
    case TYPE_LONG:
    case TYPE_ULONG:
        return g_type_long;
    default:
        SIC_UNREACHABLE();
    }
}

static inline bool type_kind_is_signed(TypeKind kind)
{
    static_assert((TYPE_BYTE & 1) == 0, "Adjust type methods.");
    SIC_ASSERT(type_kind_is_integer(kind));
    return (kind & 1) == 0;
}
static inline bool type_is_signed(Type* ty) { return type_kind_is_signed(ty->kind); }

static inline bool type_kind_is_unsigned(TypeKind kind)
{
    static_assert((TYPE_UBYTE & 1) == 1, "Adjust type methods.");
    SIC_ASSERT(type_kind_is_integer(kind));
    return (kind & 1) == 1;
}
static inline bool type_is_unsigned(Type* ty) { return type_kind_is_unsigned(ty->kind); }

static inline bool type_is_char(Type* ty)
{
    return ty->kind >= TYPE_CHAR_START && ty->kind <= TYPE_CHAR_END;
}

static inline bool type_is_float(Type* ty)
{
    return ty->kind >= TYPE_FLOAT_START && ty->kind <= TYPE_FLOAT_END;
}

static inline bool type_is_numeric(Type* ty)
{
    return (ty->kind >= TYPE_NUMERIC_START && ty->kind <= TYPE_NUMERIC_END) ||
           ty->kind == TYPE_ENUM;
}

static inline bool type_is_array(Type* ty)
{
    return ty->kind == TYPE_STATIC_ARRAY || ty->kind == TYPE_RUNTIME_ARRAY;
}

static inline bool type_is_trivially_copyable(Type* ty)
{
    return !type_is_array(ty) && type_size(ty) <= 16;
}

// Bad value checkers
static inline bool expr_is_bad(ASTExpr* expr)
{
    return expr->kind == EXPR_INVALID;
}

static inline bool stmt_is_bad(ASTStmt* stmt)
{
    return stmt->kind == STMT_INVALID;
}

static inline bool type_is_bad(Type* type)
{
    return type->kind == TYPE_INVALID;
}

static inline ObjEnum* obj_as_enum(Object* o)
{
    SIC_ASSERT(o->kind == OBJ_ENUM);
    return (ObjEnum*)o;
}

static inline ObjEnumValue* obj_as_enum_value(Object* o)
{
    SIC_ASSERT(o->kind == OBJ_ENUM_VALUE);
    return (ObjEnumValue*)o;
}

static inline ObjFunc* obj_as_func(Object* o)
{
    SIC_ASSERT(o->kind == OBJ_FUNC);
    return (ObjFunc*)o;
}

static inline ObjImport* obj_as_import(Object* o)
{
    SIC_ASSERT(o->kind == OBJ_IMPORT);
    return (ObjImport*)o;
}

static inline ObjModule* obj_as_module(Object* o)
{
    SIC_ASSERT(o->kind == OBJ_MODULE);
    return (ObjModule*)o;
}

static inline ObjStruct* obj_as_struct(Object* o)
{
    SIC_ASSERT(o->kind == OBJ_STRUCT || o->kind == OBJ_UNION);
    return (ObjStruct*)o;
}

static inline ObjTypedef* obj_as_typedef(Object* o)
{
    SIC_ASSERT(o->kind == OBJ_TYPEDEF);
    return (ObjTypedef*)o;
}

static inline ObjVar* obj_as_var(Object* o)
{
    SIC_ASSERT(o->kind == OBJ_VAR);
    return (ObjVar*)o;
}

// Debug printing functions

#ifdef SI_DEBUG

void print_token(const Token* tok);
void print_module(const ObjModule* module, bool allow_unresolved);
void print_func(const ObjFunc* func, bool allow_unresolved);
void print_stmt(const ASTStmt* stmt, bool allow_unresolved);
void print_expr(const ASTExpr* expr, bool allow_unresolved);

#endif
