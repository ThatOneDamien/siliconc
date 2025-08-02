#pragma once
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <assert.h>
#include "enums.h"

#define LOOK_AHEAD_SIZE 4
#define LOOK_AHEAD_MASK (LOOK_AHEAD_SIZE - 1)
static_assert((LOOK_AHEAD_SIZE & 1) == 0, "Look ahead size must be power of 2");

// Aliases
typedef const char* Symbol;
typedef uint8_t     FileId;

// Compiler-Wide Data Structures
typedef struct HashEntry        HashEntry;
typedef struct HashMap          HashMap;
typedef struct MemArena         MemArena;
typedef struct ScratchBuffer    ScratchBuffer;

// Lexing Stage Structs
typedef struct SourceLoc        SourceLoc;
typedef struct Token            Token;
typedef struct LookAhead        LookAhead;
typedef struct Lexer            Lexer;

// Type Structs
typedef struct TypeArray        TypeArray;
typedef struct TypeBuiltin      TypeBuiltin;
typedef struct FuncSignature*   TypeFuncPtr;
typedef struct Type*            TypePointer;
typedef struct TypeUnresolved   TypeUnresolved;
typedef struct Object*          TypeUserdef;
typedef struct Type             Type;

// Dynamic Array Structs
typedef struct StringDA         StringDA;
typedef struct ObjectDA         ObjectDA;
typedef struct ASTExprDA        ASTExprDA;
typedef struct ASTDeclDA        ASTDeclDA;
typedef struct ScopeDA          ScopeDA;
typedef struct CompUnitDA       CompUnitDA;
typedef struct InputFileDA      InputFileDA;
typedef struct ModulePTRDA      ModulePTRDA;

// AST Structs
typedef struct ASTExprAAccess   ASTExprAAccess;
typedef struct ASTExprBinary    ASTExprBinary;
typedef struct ASTExprCall      ASTExprCall;
typedef struct ASTExprCast      ASTExprCast;
typedef struct ASTExprConstant  ASTExprConstant;
typedef struct Object*          ASTExprIdent;
typedef struct ASTExprMAccess   ASTExprMAccess;
typedef Symbol                  ASTExprPSIdent; // Pre-semantic Identifier
typedef struct ASTExprTernary   ASTExprTernary;
typedef struct ASTExprUnary     ASTExprUnary;
typedef struct ASTExprUAccess   ASTExprUAccess;
typedef struct ASTExpr          ASTExpr;
typedef struct ASTBlock         ASTBlock;
typedef struct ASTDeclaration   ASTDeclaration;
typedef struct ASTFor           ASTFor;
typedef struct ASTIf            ASTIf;
typedef struct ASTReturn        ASTReturn;
typedef struct ASTSwap          ASTSwap;
typedef struct ASTWhile         ASTWhile;
typedef struct ASTStmt          ASTStmt;

// Object Structs (defined symbols)
typedef struct FuncSignature    FuncSignature;
typedef struct ObjFunc          ObjFunc;
typedef struct ObjStruct        ObjStruct;
typedef struct ObjVar           ObjVar;
typedef struct Object           Object;

// Semantic Analysis Structs
typedef struct Scope            Scope;

// Compiler-wide important structs
typedef struct InputFile        InputFile;
typedef struct CompilationUnit  CompilationUnit;
typedef struct Module           Module;
typedef struct Cmdline          Cmdline;
typedef struct CompilerContext  CompilerContext;

struct HashEntry
{
    Symbol     key;
    uint64_t   hash;
    void*      value;
};

struct HashMap
{
    HashEntry* buckets;
    uint32_t   bucket_cnt;
    uint32_t   entry_cnt;
    uint32_t   max_load;
};

struct MemArena
{
    uint8_t* base;
    size_t   capacity;
    size_t   allocated;
};

struct SourceLoc
{
    FileId   file     : 8;
    uint32_t col_num  : 12;
    uint32_t len      : 12;
    uint32_t line_num : 32;
};

struct Token
{
    SourceLoc   loc;
    TokenKind   kind;
    union
    {
        struct
        {
            char*  val;
            size_t len;
        } str;

        struct
        {
            uint64_t val;
            size_t   width;
        } chr;

        Symbol sym;
        const char* start;
    };
};


struct LookAhead
{
    Token    buf[LOOK_AHEAD_SIZE];
    uint32_t head; // Next location to be overwritten
    uint32_t cur;  // Where peek(l) will show.
};

struct Lexer
{
    CompilationUnit* unit;
    const char*      line_start;
    const char*      cur_pos;
    uint32_t         cur_line;
    LookAhead        la_buf;
};

struct TypeArray
{
    Type* elem_type;
    union
    {
        ASTExpr* size_expr;
        uint64_t ss_size;
    };
};

struct TypeBuiltin
{
    uint32_t size;
};

struct TypeUnresolved
{
    SourceLoc loc;
    Symbol    sym;
};

struct Type
{
    TypeKind      kind;
    TypeQualifier qualifiers;
    ResolveStatus status;
    void*         llvm_ref;

    union
    {
        TypeArray      array;
        TypeBuiltin    builtin;
        TypeFuncPtr    func_ptr;
        TypePointer    pointer_base;
        TypeUnresolved unresolved;
        TypeUserdef    user_def;
    };
};

struct StringDA
{
    const char** data;
    size_t size;
    size_t capacity;
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

struct ScopeDA
{
    Scope* data;
    size_t capacity;
    size_t size;
};

struct CompUnitDA
{
    CompilationUnit** data;
    size_t            capacity;
    size_t            size;
};

struct InputFileDA
{
    InputFile* data;
    size_t  capacity;
    size_t  size;
};

struct ModulePTRDA
{
    Module** data;
    size_t   capacity;
    size_t   size;
};

struct ASTExprAAccess
{
    ASTExpr* array_expr;
    ASTExpr* index_expr;
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
    ASTExpr* inner;
    CastKind kind;
};

struct ASTExprConstant
{
    ConstantKind kind;
    union
    {
        uint64_t  i;
        double    f;
        char*     s;
    } val;
};

struct ASTExprMAccess
{
    ASTExpr* parent_expr;
    Object*  member;
};

struct ASTExprTernary
{
    ASTExpr* cond_expr;
    ASTExpr* then_expr;
    ASTExpr* else_expr;
};

struct ASTExprUnary
{
    ASTExpr*    inner;
    UnaryOpKind kind;
};

struct ASTExprUAccess
{
    ASTExpr*  parent_expr;
    Symbol    member_sym;
    SourceLoc member_loc;
};

struct ASTExpr
{
    Type*     type;
    SourceLoc loc;
    ExprKind  kind;

    union
    {
        ASTExprAAccess  array_access;
        ASTExprBinary   binary;
        ASTExprCall     call;
        ASTExprCast     cast;
        ASTExprConstant constant;
        ASTExprIdent    ident;
        ASTExprMAccess  member_access;
        ASTExprPSIdent  pre_sema_ident;
        ASTExprTernary  ternary;
        ASTExprUnary    unary;
        ASTExprUAccess  unresolved_access;
    } expr;
};

struct ASTBlock
{
    ASTStmt* body;
};

struct ASTDeclaration
{
    Object*  obj;
    ASTExpr* init_expr;
};

struct ASTFor
{
    ASTStmt* init_stmt;
    ASTExpr* cond_expr;
    ASTExpr* loop_expr;
    ASTStmt* body;
};

struct ASTIf
{
    ASTExpr* cond;
    ASTStmt* then_stmt;
    ASTStmt* else_stmt;
};

struct ASTReturn
{
    ASTExpr* ret_expr;
};

struct ASTSwap
{
    ASTExpr* left;
    ASTExpr* right;
};

struct ASTWhile
{
    ASTExpr* cond;
    ASTStmt* body;
};

struct ASTStmt
{
    StmtKind  kind;
    ASTStmt*  next;
    SourceLoc loc;

    union
    {
        ASTBlock        block;
        ASTExpr*        expr;
        ASTFor          for_;
        ASTIf           if_;
        ASTDeclDA       multi_decl;
        ASTReturn       return_;
        ASTDeclaration  single_decl;
        ASTSwap         swap;
        ASTWhile        while_;
    } stmt;
};

struct FuncSignature
{
    Type*    ret_type;
    ObjectDA params;
    void*    llvm_func_type;
    bool     is_var_arg;
};

struct ObjFunc
{
    FuncSignature* signature;
    ASTStmt*       body;
    uint32_t       swap_stmt_align;
    uint32_t       swap_stmt_size;
};

struct ObjStruct
{
    ObjectDA      members;
    union
    {
        struct
        {
            uint32_t size;
            uint32_t align;
        };
        Type* largest_type;
    };
    ResolveStatus status;
};

struct ObjVar
{
    Type*    type;
    uint32_t member_idx;
};

struct Object
{
    Symbol    symbol;
    SourceLoc loc;
    ObjKind   kind;
    ObjAccess access;
    ObjAttr   attribs;
    void*     llvm_ref;

    union
    {
        ObjFunc   func;     // Components of function
        ObjStruct struct_;  // Components of bitfield, struct, or union
        ObjVar    var;      // Components of variable
    };

};

struct Scope
{
    HashMap objs;
};

struct InputFile
{
    const char* path;
    const char* src;
    FileId      id;
    FileType    type;
};

struct CompilationUnit
{
    FileId   file;
    Module*  module;
    HashMap  priv_symbols;
    ObjectDA funcs;
    ObjectDA types;
    ObjectDA vars;
};


struct Module
{
    Symbol      name;
    CompUnitDA  units;
    ModulePTRDA submodules;
    HashMap     symbols;
    HashMap     public_symbols;
    bool        used;
};

struct Cmdline
{
    InputFileDA   input_files;
    char*         output_file;

    CompileMode   mode;
    CompileTarget target;
    IRTarget      ir_kind;

    bool          emit_ir;
    bool          emit_asm;
    bool          hash_hash_hash;
#ifdef SI_DEBUG
    bool          emit_debug_output;
#endif
};

struct CompilerContext
{
    StringDA    linker_inputs;
    Module      top_module;
    ModulePTRDA modules_to_compile;
};
