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
typedef uint32_t    BitSize;
typedef uint32_t    ByteSize;

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
typedef struct TypeUnresolved   TypeUnresolved;
typedef struct Object*          TypeUserdef;
typedef struct Type             Type;

// Dynamic Array Structs
typedef struct StringDA         StringDA;
typedef struct ObjectDA         ObjectDA;
typedef struct ASTExprDA        ASTExprDA;
typedef struct ASTCaseDA        ASTCaseDA;
typedef struct ASTDeclDA        ASTDeclDA;
typedef struct CompUnitDA       CompUnitDA;
typedef struct SourceFileDA     SourceFileDA;
typedef struct ModulePTRDA      ModulePTRDA;

// AST Structs
typedef struct Namespace        Namespace;
typedef struct ASTExprAAccess   ASTExprAAccess;
typedef struct ASTExprBinary    ASTExprBinary;
typedef struct ASTExprCall      ASTExprCall;
typedef struct ASTExprCast      ASTExprCast;
typedef union  ConstantValue    ConstantValue;
typedef struct ASTExprConstant  ASTExprConstant;
typedef struct InitListEntry    InitListEntry;
typedef struct ASTExprInitList  ASTExprInitList;
typedef struct ASTExprMAccess   ASTExprMAccess;
typedef struct ASTExprPreIdent  ASTExprPreIdent;
typedef struct ASTExprTernary   ASTExprTernary;
typedef struct ASTExprUnary     ASTExprUnary;
typedef struct ASTExprUAccess   ASTExprUAccess;
typedef struct ASTExpr          ASTExpr;
typedef struct ASTBlock         ASTBlock;
typedef struct ASTCase          ASTCase;
typedef struct ASTDeclaration   ASTDeclaration;
typedef struct ASTFor           ASTFor;
typedef struct ASTIf            ASTIf;
typedef struct ASTReturn        ASTReturn;
typedef struct ASTSwap          ASTSwap;
typedef struct ASTSwitch        ASTSwitch;
typedef struct ASTWhile         ASTWhile;
typedef struct ASTStmt          ASTStmt;

// Object Structs (defined symbols)
typedef struct FuncSignature    FuncSignature;
typedef struct ObjEnum          ObjEnum;
typedef struct ObjEnumValue     ObjEnumValue;
typedef struct ObjFunc          ObjFunc;
typedef struct ObjStruct        ObjStruct;
typedef struct ObjVar           ObjVar;
typedef struct Object           Object;

// Compiler-wide important structs
typedef struct SourceFile       SourceFile;
typedef struct CompUnit         CompUnit;
typedef struct Module           Module;
typedef struct CLIArgs          CLIArgs;
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
    CompUnit*   unit;
    const char* line_start;
    const char* cur_pos;
    uint32_t    cur_line;
    LookAhead   la_buf;
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
    BitSize  bit_size;
    ByteSize byte_size;
};

struct TypeUnresolved
{
    SourceLoc loc;
    Symbol    sym;
};

struct Type
{
    TypeKind      kind;
    ResolveStatus status;
    Visibility    visibility;
    void*         llvm_ref;
    Type*         ptr_cache;

    union
    {
        TypeArray      array;
        SourceLoc      auto_loc;
        TypeBuiltin    builtin;
        FuncSignature* func_ptr;
        Type*          pointer_base;
        ASTExpr*       type_of;
        TypeUnresolved unresolved;
        TypeUserdef    user_def;
    };
};

struct StringDA
{
    const char** data;
    uint32_t size;
    uint32_t capacity;
};

struct ObjectDA
{
    Object** data;
    uint32_t capacity;
    uint32_t size;
};

struct ASTExprDA
{
    ASTExpr** data;
    uint32_t  capacity;
    uint32_t  size;
};

struct ASTCaseDA
{
    ASTCase* data;
    uint32_t capacity;
    uint32_t size;
};

struct ASTDeclDA
{
    ASTDeclaration* data;
    uint32_t        capacity;
    uint32_t        size;
};

struct CompUnitDA
{
    CompUnit** data;
    uint32_t   capacity;
    uint32_t   size;
};

struct SourceFileDA
{
    SourceFile* data;
    uint32_t    capacity;
    uint32_t    size;
};

struct ModulePTRDA
{
    Module** data;
    uint32_t capacity;
    uint32_t size;
};

struct Namespace
{
    Symbol   module;
    uint32_t len;
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

struct InitListEntry
{
    ASTExpr* arr_index;
    ASTExpr* init_value;
};

struct ASTExprInitList
{
    InitListEntry* data;
    uint32_t       capacity;
    uint32_t       size;
};

union ConstantValue
{
    uint64_t        i;
    double          f;
    struct
    {
        char*       str;
        size_t      str_len;
    };
    ASTExprInitList list;
};

struct ASTExprConstant
{
    ConstantKind  kind;
    ConstantValue val;
};

struct ASTExprMAccess
{
    ASTExpr* parent_expr;
    Object*  member;
    uint32_t member_idx;
};

struct ASTExprPreIdent
{
    Namespace ns;
    Symbol    sym;
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
    bool      evaluated : 1;

    union
    {
        ASTExprAAccess  array_access;
        ASTExprBinary   binary;
        ASTExprCall     call;
        ASTExprCast     cast;
        ASTExprConstant constant;
        Object*         ident;
        ASTExprInitList init_list;
        ASTExprMAccess  member_access;
        ASTExprPreIdent pre_sema_ident;
        ASTExprTernary  ternary;
        ASTExprUnary    unary;
        ASTExprUAccess  unresolved_access;

        Type*           ct_sizeof_type;
    } expr;
};

struct ASTBlock
{
    ASTStmt* body;
};

struct ASTCase
{
    ASTExpr* expr;
    ASTStmt* body;
    void*    llvm_block_ref;
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

struct ASTSwitch
{
    ASTExpr*  expr;
    ASTCaseDA cases;
};

struct ASTWhile
{
    ASTExpr* cond;
    ASTStmt* body;
};

struct ASTStmt
{
    StmtKind  kind;
    bool      always_returns : 1;
    ASTStmt*  next;
    SourceLoc loc;

    union
    {
        ASTBlock       block;
        ASTExpr*       expr;
        ASTFor         for_;
        ASTIf          if_;
        ASTDeclDA      multi_decl;
        ASTReturn      return_;
        ASTDeclaration single_decl;
        ASTSwap        swap;
        ASTSwitch      switch_;
        ASTWhile       while_;
    } stmt;
};

struct FuncSignature
{
    Type*    ret_type;
    ObjectDA params;
    bool     is_var_arg;
};

struct ObjEnum
{
    ObjectDA values;
    Type*    underlying;
    uint32_t min_idx;
    uint32_t max_idx;
};

struct ObjEnumValue
{
    union
    {
        ASTExpr* value;
        uint64_t const_val;
    };
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
    ObjectDA members;
    union
    {
        struct
        {
            uint32_t size;
            uint32_t align;
        };
        Type* largest_type;
    };
};

struct ObjVar
{
    union
    {
        ASTExpr*      initial_val; // Initial value for global and const vars.
        ConstantValue default_val; // Default value for parameters (TODO: struct members)
    };
    VarKind  kind;
};

struct Object
{
    Symbol        symbol;
    SourceLoc     loc;
    ObjKind       kind;
    Visibility    visibility;
    ResolveStatus status;
    void*         llvm_ref;
    Type*         type;
    union
    {
        ObjEnum      enum_;    // Components of enum typedef
        ObjEnumValue enum_val; // Components of value in enum
        ObjFunc      func;     // Components of function
        ObjStruct    struct_;  // Components of bitfield, struct, or union
        ObjVar       var;      // Components of variable
    };

};

struct SourceFile
{
    const char* path;
    const char* src;
    FileId      id;
};

struct CompUnit
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
    CompUnit*   unit;
    ModulePTRDA submodules;
    HashMap     public_symbols;
    bool        used;
};

struct CLIArgs
{
    StringDA      input_files;
    char*         output_file;

    CompileMode   mode;
    CompileTarget target;
    IRTarget      ir_kind;

    bool          emit_ir : 1;
    bool          emit_asm : 1;
    bool          hash_hash_hash : 1;
    bool          werror : 1;
#ifdef SI_DEBUG
    DebugOutput   debug_output;
#endif
};

struct CompilerContext
{
    SourceFileDA sources;
    StringDA     linker_inputs;
    Module       top_module;
    ModulePTRDA  modules_to_compile;
    Object*      main_function;
};
