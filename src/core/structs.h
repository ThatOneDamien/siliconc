#pragma once
#include "enums.h"
#include "utils/file_utils.h"

#define LOOK_AHEAD_SIZE 4 // Should be a power of two for fast modulo.

// Lexing Stage Structs
typedef struct Token            Token;
typedef struct LookAhead        LookAhead;
typedef struct Lexer            Lexer;

// Type Structs
typedef struct TypeBuiltin      TypeBuiltin;
typedef struct Type             Type;

// AST Structs
typedef struct ASTBlock         ASTBlock;
typedef struct ASTExprBinary    ASTExprBinary;
typedef struct ASTExprCast      ASTExprCast;
typedef struct ASTExprIdent     ASTExprIdent;
typedef struct ASTExprUnary     ASTExprUnary;
typedef struct ASTExpr          ASTExpr;
typedef struct ASTReturn        ASTReturn;
typedef struct ASTNode          ASTNode;

// Object Structs (defined symbols)
typedef struct ObjFunc          ObjFunc;
typedef struct ObjVar           ObjVar;
typedef struct Object           Object;
typedef struct ObjectDA         ObjectDA;
typedef struct CompilationUnit  CompilationUnit;

struct Token
{
    TokenKind   kind;
    const char* loc;
    const char* line_start;
    uint32_t    len;
    uint32_t    line_num;
};


struct LookAhead
{
    Token    buf[LOOK_AHEAD_SIZE];
    uint32_t head;
};

struct Lexer
{
    CompilationUnit* unit;
    const char*      src_start;
    const char*      line_start;
    const char*      cur_pos;
    uint32_t         cur_line;
    LookAhead        la_buf;
};

struct TypeBuiltin
{
    uint32_t size;
};

struct Type
{
    TypeKind      kind;
    TypeQualifier qualifiers;

    union
    {
        TypeBuiltin builtin;
        Type*       pointer_base;
    };
};

struct ASTBlock
{
    ASTNode* body;
};

struct ASTExprBinary
{
    ASTExpr* lhs;
    ASTExpr* rhs;
    BinaryOpKind kind;
};

struct ASTExprCast
{
    Type*    type;
    ASTExpr* expr_to_cast;
};

struct ASTExprIdent
{
    
};

struct ASTExprUnary
{
    ASTExpr* child;
    UnaryOpKind kind;
};

struct ASTExpr
{
    Type* type;
    Token token;
    ExprKind kind;

    union
    {
        ASTExprBinary binary;
        ASTExprCast   cast;
        ASTExprIdent  pre_sema_ident;
        ASTExprUnary  unary;
    } expr;
};


struct ASTReturn
{
    ASTExpr* ret_expr;
};

struct ASTNode
{
    NodeKind kind;
    ASTNode* next;
    Token    token;

    union
    {
        ASTBlock  block;
        ASTExpr*  expr;
        ASTReturn return_;
    } stmt;
};

struct ObjFunc
{
    Type*    ret_type;
    Object*  local_objs;
    ASTNode* body;
    Object*  params;
    size_t   param_cnt;
    int      stack_size;
};

struct ObjVar
{
    Type* type;
    int   offset;
};

struct Object
{
    Object*      next;
    Token        symbol;
    ObjKind      kind;
    ObjAccess    access;
    ObjAttr      attribs;

    union
    {
        ObjFunc  func; // Components of function
        ObjVar   var;  // Components of variable
    };

};

struct ObjectDA
{
    Object** data;
    size_t   capacity;
    size_t   size;
};

struct CompilationUnit
{
    SIFile   file;
    ObjectDA funcs;
    ObjectDA vars;
};
