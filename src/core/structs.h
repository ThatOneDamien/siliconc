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
typedef uint16_t    FileId;
typedef uint32_t    BitSize;
typedef uint32_t    ByteSize;

// Compiler-Wide Data Structures
typedef struct Int128           Int128;
typedef struct ConstString      ConstString;
typedef struct HashEntry        HashEntry;
typedef struct HashMap          HashMap;
typedef struct MemArena         MemArena;
typedef struct ScratchBuffer    ScratchBuffer;

// Lexing Stage Structs
typedef struct SourceLoc        SourceLoc;
typedef struct LineCol          LineCol;
typedef struct Token            Token;
typedef struct LookAhead        LookAhead;
typedef struct Lexer            Lexer;
typedef struct SymbolLoc        SymbolLoc;
typedef struct ModulePath       ModulePath;

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
typedef struct AttrDA           AttrDA;

// AST Structs
typedef struct Attr             Attr;
typedef struct ASTExprAAccess   ASTExprAAccess;
typedef struct ASTExprBinary    ASTExprBinary;
typedef struct ASTExprCall      ASTExprCall;
typedef struct ASTExprCast      ASTExprCast;
typedef struct ASTExprConstant  ASTExprConstant;
typedef struct ArrInitEntry     ArrInitEntry;
typedef struct ArrInitList      ArrInitList;
typedef struct StructInitEntry  StructInitEntry;
typedef struct StructInitList   StructInitList;
typedef struct ASTExprMAccess   ASTExprMAccess;
typedef struct ASTExprRange     ASTExprRange;
typedef struct ASTExprPreIdent  ASTExprPreIdent;
typedef struct ASTExprTernary   ASTExprTernary;
typedef struct ASTExprTuple     ASTExprTuple;
typedef struct ASTExprUnary     ASTExprUnary;
typedef struct ASTExprUAccess   ASTExprUAccess;
typedef struct ASTExprCTOffset  ASTExprCTOffset;
typedef struct ASTExpr          ASTExpr;
typedef struct ASTBlock         ASTBlock;
typedef struct ASTBreakCont     ASTBreakCont;
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

struct Int128
{
    uint64_t hi;
    uint64_t lo;
};

struct ConstString
{
    char*    val;
    uint32_t len;
    TypeKind kind;
};

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
#ifdef SI_DEBUG

#endif
};

struct SourceLoc
{
    FileId   file;
    uint16_t len;
    uint32_t start;
};
static_assert(sizeof(SourceLoc) == 8, "Check sizes");

struct LineCol
{
    uint32_t line;
    uint32_t col;
};

struct Token
{
    SourceLoc   loc;
    LineCol     line_col;
    TokenKind   kind;
    union
    {
        ConstString str;

        struct
        {
            uint32_t val;
            TypeKind kind;
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
    const char* src;
    uint32_t    line_num;
    uint32_t    line_start;
    uint32_t    pos;
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
    TypeKind       kind;
    ResolveStatus  status;
    Visibility     visibility;
    TypeQualifiers qualifiers;
    void*          llvm_ref;
    Type*          cache;
    Type*          canonical;

    union
    {
        TypeArray       array;
        TypeBuiltin     builtin;
        FuncSignature*  func_ptr;
        Type*           pointer_base; // Used for both pointers and slices
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

struct AttrDA
{
    Attr*    data;
    uint32_t capacity;
    uint32_t size;
};

struct Attr
{
    Symbol    symbol;
    SourceLoc loc;
    AttrKind  kind;
    ASTExprDA args;
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

struct ArrInitEntry
{
    union
    {
        ASTExpr* arr_index;
        uint64_t const_index;
    };
    ASTExpr* init_value;
};

struct ArrInitList
{
    ArrInitEntry* data;
    uint32_t      capacity;
    uint32_t      size;
    uint64_t      max;
};

struct StructInitEntry
{
    union
    {
        SymbolLoc unresolved_member;
        // ObjVar*   member;
    };
    ASTExpr*  init_value;
};

struct StructInitList
{
    StructInitEntry* data;
    uint32_t         capacity;
    uint32_t         size;
    uint64_t         max;
};


struct ASTExprConstant
{
    ConstantKind  kind;
    union
    {
        Int128        i; // Integer
        double        f; // Float
        bool          b; // Bool
        uint32_t      c; // Char
        ConstString   str; // String
        ObjEnumValue* enum_; // Enum Value
    };
};

struct ASTExprMAccess
{
    ASTExpr* parent_expr;
    ObjVar*  member;
    uint32_t member_idx;
};

struct ASTExprRange
{
    ASTExpr* from;
    ASTExpr* to;
    bool     inclusive;
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
    bool      pure : 1;

    union
    {
        ASTExprAAccess  array_access;
        ASTExprBinary   binary;
        ASTExprCall     call;
        ASTExprCast     cast;
        ASTExprConstant constant;
        Object*         ident;
        ArrInitList     array_init;
        StructInitList  struct_init;
        ASTExprMAccess  member_access;
        ASTExprRange    range;
        ModulePath      pre_sema_ident;
        ASTExprTernary  ternary;
        ASTExprDA       tuple;
        ASTExprUnary    unary;
        ASTExprUAccess  unresolved_access;

        TypeLoc         ct_alignof;
        TypeLoc         ct_sizeof;
        ASTExprCTOffset ct_offsetof;
    } expr;
};

struct ASTBreakCont
{
    union
    {
        SymbolLoc label;
        ASTStmt*  target;
    };
};

struct ASTCase
{
    ASTExpr* expr;
    ASTStmt* body;
    void*    llvm_block_ref;
};

struct ASTFor
{
    SymbolLoc label;
    union
    {
        struct
        {
            ObjVar*  loop_var;
            ASTExpr* collection;
            ASTStmt* body;
        };

        struct
        {
            void* continue_block;
            void* break_block;
        } backend;
    };
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
    SymbolLoc label;
    union
    {
        struct
        {
            ASTExpr*  expr;
            ASTCaseDA cases;
        };

        struct
        {
            void* break_block;
            // TODO: Add retry for use in goto case
        } backend;
    };
};

struct ASTWhile
{
    SymbolLoc label;
    union
    {
        struct
        {
            ASTExpr* cond;
            ASTStmt* body;
        };

        struct
        {
            void* continue_block;
            void* break_block;
        } backend;
    };
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
        ASTStmt*  block;
        ASTBreakCont break_cont;
        ObjVar*   declaration;
        ASTExpr*  expr;
        ASTFor    for_;
        ASTIf     if_;
        ASTReturn return_;
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
    Symbol        link_name;
    SourceLoc     loc;
    ObjKind       kind;
    Visibility    visibility;
    ResolveStatus status;
    AttrDA        attrs;
    ObjModule*    module;
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
        Int128   const_value;
    };
};

struct ObjFunc
{
    Object         header;
    FuncSignature  signature;
    Type*          func_type;
    ASTStmt*       body;
    ByteSize       swap_stmt_align;
    ByteSize       swap_stmt_size;
    bool           is_extern;
    bool           used;
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
            ByteSize size;
            ByteSize align;
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
    bool      uninitialized; // If this is true we have something like int a = void; By default, variables are initialized
    bool      is_const_binding;
    bool      written;
    bool      read;
    bool      is_extern;
    VarKind   kind;
};

struct SourceFile
{
    const char* abs_path;
    const char* rel_path;
    const char* src;
    struct
    {
        uint32_t* data;
        uint32_t  size;
        uint32_t  capacity;
    } line_starts;
    ObjModule*  module;
};

struct CompilerContext
{
    SourceFileDA  sources;
    StringDA      linker_inputs;
    ObjModule     top_module;
    const char*   compiler_name;

    // Compiler CLI arguments/flags
    FileId        input_file;
    const char*   out_name;
    const char*   out_dir;

    struct
    {
        CompileTarget kind;
        size_t        ptr_size;
    } target;
    IRTarget      ir_kind;

    bool          stderr_is_tty : 1;
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
