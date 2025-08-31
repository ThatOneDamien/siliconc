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
typedef struct Type*            TypeCache[__TYPE_CACHE_CNT];

// Dynamic Array Structs
typedef struct StringDA         StringDA;
typedef struct ObjectDA         ObjectDA;
typedef struct ASTExprDA        ASTExprDA;
typedef struct ASTCaseDA        ASTCaseDA;
typedef struct ASTDeclDA        ASTDeclDA;
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
typedef struct InitListEntry    InitListEntry;
typedef struct ASTExprInitList  ASTExprInitList;
typedef struct ASTExprMAccess   ASTExprMAccess;
typedef Symbol                  ASTExprPSIdent; // Pre-semantic Identifier
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
typedef struct ASTExpr*         ObjAliasExpr;
typedef struct Type*            ObjTypeAlias;
typedef struct ObjEnum          ObjEnum;
typedef struct ObjEnumValue     ObjEnumValue;
typedef struct ObjFunc          ObjFunc;
typedef struct ObjStruct        ObjStruct;
typedef struct ObjVar           ObjVar;
typedef struct Object           Object;

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
    Visibility    visibility;
    void*         llvm_ref;
    TypeCache     cache;

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
    CompilationUnit** data;
    uint32_t          capacity;
    uint32_t          size;
};

struct InputFileDA
{
    InputFile* data;
    uint32_t   capacity;
    uint32_t   size;
};

struct ModulePTRDA
{
    Module** data;
    uint32_t capacity;
    uint32_t size;
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

struct ASTExprConstant
{
    ConstantKind kind;
    union
    {
        uint64_t        i;
        double          f;
        char*           s;
        ASTExprInitList list;
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
    bool      evaluated;

    union
    {
        ASTExprAAccess  array_access;
        ASTExprBinary   binary;
        ASTExprCall     call;
        ASTExprCast     cast;
        ASTExprConstant constant;
        ASTExprIdent    ident;
        ASTExprInitList init_list;
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
        ASTExpr* global_initializer;
        union
        {
            uint64_t i;
            double   f;
        } default_val;
    };
    uint32_t member_idx;
    VarKind  kind;

};

struct Object
{
    Symbol        symbol;
    SourceLoc     loc;
    ObjKind       kind;
    Visibility    visibility;
    ObjAttr       attribs;
    ResolveStatus status;
    void*         llvm_ref;
    Type*         type;
    union
    {
        ObjAliasExpr alias_expr;
        ObjEnum      enum_;    // Components of enum typedef
        ObjEnumValue enum_val; // Components of value in enum
        ObjFunc      func;     // Components of function
        ObjStruct    struct_;  // Components of bitfield, struct, or union
        ObjTypeAlias type_alias;
        ObjVar       var;      // Components of variable
    };

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
    ObjectDA aliases;
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
    StringDA    linker_inputs;
    Module      top_module;
    ModulePTRDA modules_to_compile;
    Object*     main_function;
};
