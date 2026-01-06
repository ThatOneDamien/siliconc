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
typedef struct SymbolLoc        SymbolLoc;

// Type Structs
typedef struct TypeArray        TypeArray;
typedef struct TypeBuiltin      TypeBuiltin;
typedef struct Type             Type;
typedef struct TypeLoc          TypeLoc;

// Dynamic Array Structs
typedef struct StringDA         StringDA;
typedef struct ObjectDA         ObjectDA;
typedef struct ObjEnumValueDA   ObjEnumValueDA;
typedef struct ObjFuncDA        ObjFuncDA;
typedef struct ObjImportDA      ObjImportDA;
typedef struct ObjModuleDA      ObjModuleDA;
typedef struct ObjVarDA         ObjVarDA;
typedef struct ASTExprDA        ASTExprDA;
typedef struct ASTCaseDA        ASTCaseDA;
typedef struct SourceFileDA     SourceFileDA;

// AST Structs
typedef struct ModulePath       ModulePath;
typedef struct ASTExprAAccess   ASTExprAAccess;
typedef struct ASTExprBinary    ASTExprBinary;
typedef struct ASTExprCall      ASTExprCall;
typedef struct ASTExprCast      ASTExprCast;
typedef union  ConstantValue    ConstantValue;
typedef struct ASTExprConstant  ASTExprConstant;
typedef struct InitListEntry    InitListEntry;
typedef struct InitList         InitList;
typedef struct ASTExprMAccess   ASTExprMAccess;
typedef struct ASTExprPreIdent  ASTExprPreIdent;
typedef struct ASTExprTernary   ASTExprTernary;
typedef struct ASTExprUnary     ASTExprUnary;
typedef struct ASTExprUAccess   ASTExprUAccess;
typedef struct ASTExprCTOffset  ASTExprCTOffset;
typedef struct ASTExpr          ASTExpr;
typedef struct ASTBlock         ASTBlock;
typedef struct ASTCase          ASTCase;
typedef struct ASTFor           ASTFor;
typedef struct ASTIf            ASTIf;
typedef struct ASTReturn        ASTReturn;
typedef struct ASTSwap          ASTSwap;
typedef struct ASTSwitch        ASTSwitch;
typedef struct ASTWhile         ASTWhile;
typedef struct ASTCtAssert      ASTCtAssert;
typedef struct ASTStmt          ASTStmt;

// Object Structs (defined symbols)
typedef struct FuncSignature    FuncSignature;
typedef struct Object           Object;
typedef struct ObjEnum          ObjEnum;
typedef struct ObjEnumValue     ObjEnumValue;
typedef struct ObjFunc          ObjFunc;
typedef struct ObjImport        ObjImport;
typedef struct ObjModule        ObjModule;
typedef struct ObjStruct        ObjStruct;
typedef struct ObjTypedef       ObjTypedef;
typedef struct ObjVar           ObjVar;

// Compiler-wide important structs
typedef struct SourceFile       SourceFile;
typedef struct CompilerContext  CompilerContext;

struct HashEntry
{
    Symbol     key;
    uint64_t   hash;
    Object*    value;
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
            char    val[4];
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
    ObjModule*  module;
    const char* line_start;
    const char* cur_pos;
    uint32_t    cur_line;
    LookAhead   la_buf;
};

struct SymbolLoc
{
    Symbol    sym;
    SourceLoc loc;
};

struct ModulePath
{
    SymbolLoc* data;
    uint32_t   size;
    uint32_t   capacity;
};

struct TypeArray
{
    Type* elem_type;
    union
    {
        ASTExpr* size_expr;
        uint64_t static_len;
    };
};

struct TypeBuiltin
{
    BitSize  bit_size;
    ByteSize byte_size;
};

struct Type
{
    TypeKind      kind;
    ResolveStatus status;
    Visibility    visibility;
    void*         llvm_ref;
    Type*         ptr_cache;
    Type*         canonical;

    union
    {
        TypeArray       array;
        TypeBuiltin     builtin;
        FuncSignature*  func_ptr;
        Type*           pointer_base;
        ASTExpr*        type_of;
        ModulePath      unresolved;
        ObjEnum*        enum_;
        ObjStruct*      struct_;
        ObjTypedef*     typedef_;
    };
};

struct TypeLoc
{
    Type*     type;
    SourceLoc loc;
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

struct ObjEnumValueDA
{
    ObjEnumValue** data;
    uint32_t       capacity;
    uint32_t       size;
};

struct ObjFuncDA
{
    ObjFunc** data;
    uint32_t  capacity;
    uint32_t  size;
};

struct ObjImportDA
{
    ObjImport** data;
    uint32_t    capacity;
    uint32_t    size;
};

struct ObjModuleDA
{
    ObjModule** data;
    uint32_t    capacity;
    uint32_t    size;
};

struct ObjVarDA
{
    ObjVar** data;
    uint32_t  capacity;
    uint32_t  size;
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

struct SourceFileDA
{
    SourceFile* data;
    uint32_t    capacity;
    uint32_t    size;
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
    union
    {
        ASTExpr* arr_index;
        ASTExpr* member;
        uint64_t const_index;
    };
    ASTExpr* init_value;
};

struct InitList
{
    InitListEntry* data;
    uint32_t       capacity;
    uint32_t       size;
    uint64_t       max;
};

union ConstantValue
{
    uint64_t   i;
    double     f;
    struct
    {
        char*  str;
        size_t str_len;
    };
};

struct ASTExprConstant
{
    ConstantKind  kind;
    ConstantValue val;
};

struct ASTExprMAccess
{
    ASTExpr* parent_expr;
    ObjVar*  member;
    uint32_t member_idx;
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
    SymbolLoc member;
};

struct ASTExprCTOffset
{
    TypeLoc   struct_;
    SymbolLoc member;
};

struct ASTExpr
{
    Type*     type;
    SourceLoc loc;
    ExprKind  kind;
    bool      evaluated : 1;
    bool      const_eval : 1;

    union
    {
        ASTExprAAccess  array_access;
        ASTExprBinary   binary;
        ASTExprCall     call;
        ASTExprCast     cast;
        ASTExprConstant constant;
        Object*         ident;
        InitList        init_list;
        ASTExprMAccess  member_access;
        ModulePath      pre_sema_ident;
        ASTExprTernary  ternary;
        ASTExprUnary    unary;
        ASTExprUAccess  unresolved_access;

        TypeLoc         ct_alignof;
        TypeLoc         ct_sizeof;
        ASTExprCTOffset ct_offsetof;
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

struct ASTCtAssert
{
    ASTExpr* cond;
    ASTExpr* err_msg;
};

struct ASTStmt
{
    StmtKind  kind;
    bool      always_returns : 1;
    ASTStmt*  next;
    SourceLoc loc;

    union
    {
        ASTBlock  block;
        ASTExpr*  expr;
        ASTFor    for_;
        ASTIf     if_;
        ObjVarDA  multi_decl;
        ASTReturn return_;
        ObjVar*   single_decl;
        ASTSwap   swap;
        ASTSwitch switch_;
        ASTWhile  while_;

        ASTCtAssert ct_assert;
    } stmt;
};

struct FuncSignature
{
    TypeLoc  ret_type;
    ObjVarDA params;
    void*    llvm_ref;
    bool     is_var_arg;
};

struct Object
{
    Symbol        symbol;
    SourceLoc     loc;
    ObjKind       kind;
    Visibility    visibility;
    ResolveStatus status;
    void*         llvm_ref;
};

struct ObjEnum
{
    Object         header;
    ObjEnumValueDA values;
    Type*          type_ref;
    TypeLoc        underlying;
    uint32_t       min_idx;
    uint32_t       max_idx;
};

struct ObjEnumValue
{
    Object   header;
    Type*    enum_type;
    union
    {
        ASTExpr* raw_value;
        uint64_t const_value;
    };
};

struct ObjFunc
{
    Object         header;
    FuncSignature  signature;
    Type*          func_type;
    ASTStmt*       body;
    uint32_t       swap_stmt_align;
    uint32_t       swap_stmt_size;
};

struct ObjImport
{
    Object  header;
    union
    {
        Object*    resolved;
        ModulePath unresolved;
    };
};

struct ObjModule
{
    Object      header;
    bool        is_inline;
    ObjModule*  parent;
    ObjModuleDA submodules;
    ObjFuncDA   funcs;
    ObjImportDA imports;
    ObjectDA    types;
    ObjVarDA    vars;
    ASTStmt*    ct_asserts;
    HashMap     module_ns; // Namespace of modules
    HashMap     symbol_ns; // Namespace of types, functions, variables
};

struct ObjStruct
{
    Object   header;
    Type*    type_ref;
    ObjVarDA members;
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

struct ObjTypedef
{
    Object  header;
    Type*   type_ref;
    TypeLoc alias;
};

struct ObjVar
{
    Object    header;
    TypeLoc   type_loc;
    ASTExpr*  initial_val;
    VarKind   kind;
};



// TODO: Optimization should be possible to make the average Object instance smaller.
//       If it is possible, it would be done by making the shared resources a separate
//       header struct called Object, then the other union kinds would be their own structs
//       with the header to start.
// struct Object
// {
//     union
//     {
//         ObjEnum      enum_;      // Components of enum typedef
//         ObjEnumValue enum_val;   // Components of value in enum
//         ObjFunc      func;       // Components of function
//         Object*      import;     // Imported object
//         Module*      module;     // Module reference
//         ObjStruct    struct_;    // Components of bitfield, struct, or union
//         Type*        type_alias; // Typedef alias
//         ModulePath   unresolved_import;
//         ObjVar       var;        // Components of variable
//     };
//
// };

struct SourceFile
{
    const char* abs_path;
    const char* rel_path;
    const char* src;
    ObjModule*  module;
};

struct CompilerContext
{
    SourceFileDA  sources;
    StringDA      linker_inputs;
    ObjModule     top_module;

    // Compiler CLI arguments/flags
    FileId        input_file;
    const char*   out_name;
    const char*   out_dir;

    CompileTarget target;
    IRTarget      ir_kind;

    bool          emit_link : 1;
    bool          emit_obj : 1;
    bool          emit_ir : 1;
    bool          emit_bc : 1;
    bool          emit_asm : 1;
    bool          werror : 1;
#ifdef SI_DEBUG
    DebugOutput   debug_output;
#endif
};

#define LOC_NULL (SourceLoc){0}
