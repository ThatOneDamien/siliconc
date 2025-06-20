#pragma once
#include "enums.h"
#include "utils/file_utils.h"
#include "utils/lib.h"

#define LOOK_AHEAD_SIZE 4 // Should be a power of two for fast modulo.

// Lexing Stage Structs
typedef struct SourceLoc        SourceLoc;
typedef struct Token            Token;
typedef struct LookAhead        LookAhead;
typedef struct Lexer            Lexer;

// Type Structs
typedef struct TypeBuiltin      TypeBuiltin;
typedef struct Type             Type;

// Dynamic Array Structs
typedef struct ObjectDA         ObjectDA;
typedef struct ASTExprDA        ASTExprDA;
typedef struct ASTDeclDA        ASTDeclDA;

// AST Structs
typedef struct ASTBlock         ASTBlock;
typedef struct ASTDeclaration   ASTDeclaration;
typedef struct ASTExprBinary    ASTExprBinary;
typedef struct ASTExprCall      ASTExprCall;
typedef struct ASTExprCast      ASTExprCast;
typedef struct ASTExprConstant  ASTExprConstant;
typedef struct ASTExprIdent     ASTExprIdent;
typedef struct ASTExprUnary     ASTExprUnary;
typedef struct ASTExpr          ASTExpr;
typedef struct ASTReturn        ASTReturn;
typedef struct ASTNode          ASTNode;

// Object Structs (defined symbols)
typedef struct ObjFunc          ObjFunc;
typedef struct ObjVar           ObjVar;
typedef struct Object           Object;

// Semantic Analysis Structs
typedef struct Scope            Scope;

typedef struct CompilationUnit  CompilationUnit;

struct SourceLoc
{
    const char* start;
    const char* line_start;
    uint32_t    len;
    uint32_t    line_num;
};

struct Token
{
    TokenKind kind;
    SourceLoc loc;
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

struct ObjectDA
{
    Object** data;
    size_t   capacity;
    size_t   size;
};

struct ASTExprDA
{
    ASTExpr** data;
    size_t    capacity;
    size_t    size;
};

struct ASTDeclDA
{
    ASTDeclaration* data;
    size_t          capacity;
    size_t          size;
};

struct ASTBlock
{
    ASTNode* body;
};

struct ASTDeclaration
{
    Object*  obj;
    ASTExpr* init_expr;
};

struct ASTExprBinary
{
    ASTExpr*     lhs;
    ASTExpr*     rhs;
    BinaryOpKind kind;
};

struct ASTExprCall
{
    ASTExpr*  func_expr;
    ASTExprDA args;
};

struct ASTExprCast
{
    ASTExpr* expr_to_cast;
    Type*    cast_type;
};

struct ASTExprConstant
{
    ConstantKind kind;
    union
    {
        uint64_t  i;
        double    f;
    } val;
};

struct ASTExprIdent
{
    Object* obj;
};

struct ASTExprUnary
{
    ASTExpr*    child;
    UnaryOpKind kind;
};

struct ASTExpr
{
    Type*     type;
    SourceLoc loc;
    ExprKind  kind;

    union
    {
        ASTExprBinary   binary;
        ASTExprCall     call;
        ASTExprCast     cast;
        ASTExprConstant constant;
        ASTExprIdent    ident;
        ASTExprUnary    unary;
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
        ASTBlock        block;
        ASTDeclaration  single_decl;
        ASTDeclDA       multi_decl;
        ASTExpr*        expr;
        ASTReturn       return_;
    } stmt;
};


struct ObjFunc
{
    Type*    ret_type;
    ASTNode* body;
    ObjectDA local_objs;
    ObjectDA params;
    int      stack_size;
};

struct ObjVar
{
    Type* type;
    int   offset;
};

struct Object
{
    SourceLoc    symbol;
    ObjKind      kind;
    ObjAccess    access;
    ObjAttr      attribs;

    union
    {
        ObjFunc  func; // Components of function
        ObjVar   var;  // Components of variable
    };

};

struct Scope
{
    Scope*  parent;
    HashMap vars;
};

struct CompilationUnit
{
    SIFile   file;
    ObjectDA funcs;
    ObjectDA vars;
};
