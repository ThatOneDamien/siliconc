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
typedef uint64_t    ArrayLength;

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
typedef struct TypeOptional     TypeOptional;
typedef struct TypePointer      TypePointer;
typedef struct TypeSlice        TypeSlice;
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
typedef struct ExprDA           ExprDA;
typedef struct StmtCaseDA       StmtCaseDA;
typedef struct SourceFileDA     SourceFileDA;
typedef struct AttrDA           AttrDA;

// AST Structs
// Switch and If are special because they can be expressions+stmts
typedef struct ASTIf            ASTIf;
typedef struct ASTSwitch        ASTSwitch;
typedef struct Attr             Attr;
typedef struct ExprAAccess      ExprArrayAccess;
typedef struct ExprBinary       ExprBinary;
typedef struct ExprCall         ExprCall;
typedef struct ExprCast         ExprCast;
typedef struct ExprCond         ExprCond;
typedef struct ExprConstant     ExprConstant;
typedef struct ExprIf           ExprIf;
typedef struct ArrInitEntry     ArrInitEntry;
typedef struct ArrInitList      ArrInitList;
typedef struct StructInitEntry  StructInitEntry;
typedef struct StructInitList   StructInitList;
typedef struct ExprMAccess      ExprMemberAccess;
typedef struct ExprMBuiltin     ExprMemberBuiltin;
typedef struct ExprMethod       ExprMethod;
typedef struct ExprPtrOff       ExprPtrOffset;
typedef struct ExprRange        ExprRange;
typedef struct ExprTuple        ExprTuple;
typedef struct ExprUnary        ExprUnary;
typedef struct ExprUAccess      ExprUnresAccess;
typedef struct ExprCTOffset     ExprCTOffset;
typedef struct ExprCTTypeEqual  ExprCTTypeEqual;
typedef struct Expr             Expr;
typedef struct StmtBreakCont    StmtBreakCont;
typedef struct StmtCase         StmtCase;
typedef struct StmtFor          StmtFor;
typedef struct StmtResult       StmtResult;
typedef struct StmtSwap         StmtSwap;
typedef struct StmtWhile        StmtWhile;
typedef struct StmtCtAssert     StmtCTAssert;
typedef struct Stmt             Stmt;

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
    size_t saved;
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
    LookAhead   la_buf;
    uint32_t    line_num;
    uint32_t    line_start;
    uint32_t    pos;
    bool        allow_struct_expr;
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
        Expr*    size_expr;
        ArrayLength static_len;
    };
};

struct TypeBuiltin
{
    BitSize  bit_size;
    ByteSize byte_size;
};

struct TypeOptional 
{
    Type* base;
};

struct TypePointer
{
    Type* base;
    union
    {
        Expr*    size_expr;
        ArrayLength static_len;
    };
};

struct TypeSlice
{
    Type* base;
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
        TypeArray      array;
        TypeBuiltin    builtin;
        FuncSignature* func_ptr;
        TypeOptional   optional;
        TypePointer    pointer;
        TypeSlice      slice;
        Expr*          type_of;
        ModulePath     unresolved;
        Object*        user_def;
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

struct ExprDA
{
    Expr** data;
    uint32_t  capacity;
    uint32_t  size;
};

struct StmtCaseDA
{
    StmtCase* data;
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

struct ASTIf
{
    Expr* cond;
    Stmt* then_stmt;
    Stmt* else_stmt;
};

struct ASTSwitch
{
    SymbolLoc label;
    union
    {
        struct
        {
            Expr*  expr;
            StmtCaseDA cases;
        };

        struct
        {
            void* break_block;
            // TODO: Add retry for use in goto case
        } backend;
    };
};

struct Attr
{
    Symbol    symbol;
    SourceLoc loc;
    AttrKind  kind;
    ExprDA args;
};

struct ExprAAccess
{
    Expr* array_expr;
    Expr* index_expr;
};

struct ExprBinary
{
    Expr*     lhs;
    Expr*     rhs;
    BinaryOpKind kind;
};

struct ExprCall
{
    Expr*  func_expr;
    ExprDA args;
};

struct ExprCast
{
    Expr* inner;
    CastKind kind;
};

struct ArrInitEntry
{
    union
    {
        Expr* arr_index;
        uint64_t const_index;
    };
    Expr* init_value;
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
        uint32_t  member_idx;
    };
    Expr*  init_value;
};

struct StructInitList
{
    ModulePath       struct_path;
    StructInitEntry* data;
    uint32_t         capacity;
    uint32_t         size;
    uint64_t         max;
};


struct ExprConstant
{
    ConstantKind  kind;
    bool          is_bit_int; // For integer. I put it here to conserve space. This is true for binary, octal, and hex literals
    union
    {
        Int128        i; // Integer
        double        f; // Float
        bool          b; // Bool
        ConstString   str; // String
    };
};

struct ExprIf
{
    union
    {
        ASTIf data;
        struct
        {
            void* exit_block;
            void* phi_node;
        } backend;
    };
};

struct ExprCond
{
    Expr* cond_expr;
    Expr* then_expr;
    Expr* else_expr;
};


struct ExprMAccess
{
    Expr* parent_expr;
    ObjVar*  member;
    uint32_t member_idx;
};

struct ExprMBuiltin
{
    Expr* parent_expr;
    Symbol   symbol;
    uint32_t member_idx;
};

struct ExprMethod
{
    Expr* parent_expr;
    ObjFunc* method;
};

struct ExprPtrOff
{
    Expr* pointer;
    Expr* offset;
};

struct ExprRange
{
    Expr* from;
    Expr* to;
    // bool     inclusive;
};

struct ExprUnary
{
    Expr*    inner;
    UnaryOpKind kind;
};

struct ExprUAccess
{
    Expr*  parent_expr;
    SymbolLoc member;
};

struct ExprCTOffset
{
    TypeLoc   struct_;
    SymbolLoc member;
};

struct ExprCTTypeEqual
{
    TypeLoc first;
    TypeLoc second;
};

struct Expr
{
    Type*     type;
    SourceLoc loc;
    ExprKind  kind;
    bool      is_evaluated : 1;
    bool      is_const_eval : 1;

    union
    {
        ExprArrayAccess   array_access;
        ArrInitList       array_init;
        ExprBinary        binary;
        ExprCall          call;
        ExprCast          cast;
        ExprCond          conditional;
        ExprConstant      constant;
        ObjFunc*          function;
        ExprIf            if_;
        ExprMemberAccess  member_access;
        ExprMemberBuiltin member_builtin;
        ExprMethod        method_access;
        ExprPtrOffset     pointer_offset;
        ExprRange         range;
        StructInitList    struct_init;
        ASTSwitch         switch_;
        ExprDA            tuple;
        Object*           type_ident;
        ExprUnary         unary;
        ExprUnresAccess   unresolved_access;
        ModulePath        unresolved_ident;
        Expr*             unwrap;
        ObjVar*           var;

        ExprCTOffset      ct_offsetof;
        TypeLoc           ct_typearg;
        ExprCTTypeEqual   ct_type_equal;
    } expr;
};

struct StmtBreakCont
{
    union
    {
        SymbolLoc label;
        Stmt*  target;
    };
};

struct StmtCase
{
    Expr* expr;
    Stmt* body;
    void*    llvm_block_ref;
};

struct StmtFor
{
    SymbolLoc label;
    union
    {
        struct
        {
            ObjVar*  loop_var;
            Expr* collection;
            Stmt* body;
        };

        struct
        {
            void* continue_block;
            void* break_block;
        } backend;
    };
};

struct StmtResult
{
    Expr* val;
    Expr* target;
};

struct StmtSwap
{
    Expr* left;
    Expr* right;
};

struct StmtWhile
{
    SymbolLoc label;
    union
    {
        struct
        {
            Expr* cond;
            Stmt* body;
        };

        struct
        {
            void* continue_block;
            void* break_block;
        } backend;
    };
};

struct StmtCtAssert
{
    Expr* cond;
    Expr* err_msg;
};

struct Stmt
{
    StmtKind  kind;
    bool      always_returns : 1; // Does the block always return?
    Stmt*     next;
    SourceLoc loc;

    union
    {
        Stmt*      block;
        StmtBreakCont break_cont;
        ObjVar*    declaration;
        Expr*      expr;
        StmtFor    for_;
        ASTIf      if_;
        StmtResult result;
        Expr*      return_val;
        StmtSwap   swap;
        ASTSwitch  switch_;
        StmtWhile  while_;

        StmtCTAssert ct_assert;
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
    Symbol        sym;
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
    union
    {
        Expr* raw_value;
        Int128   const_value;
    };
};

struct ObjFunc
{
    Object         header;
    Symbol         link_name;
    FuncSignature  signature;
    SymbolLoc      method_parent;
    Type*          func_type;
    Stmt*       body;
    ByteSize       swap_stmt_align;
    ByteSize       swap_stmt_size;
    bool           is_extern;
    bool           used;
    bool           is_method;
    bool           is_static;
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
    ObjFuncDA   methods;
    ObjImportDA imports;
    ObjectDA    types;
    ObjVarDA    vars;
    Stmt*    ct_asserts;
    HashMap     module_ns; // Namespace of modules
    HashMap     symbol_ns; // Namespace of types, functions, variables
};

struct ObjStruct
{
    Object    header;
    Type*     type_ref;
    ObjVarDA  members;
    ObjFuncDA methods;
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
    Object          header;
    Symbol          link_name;
    TypeLoc         type_loc;
    Expr*        initial_val;
    bool            uninitialized; // If this is true we have something like int a = void; By default, variables are initialized
    bool            written;
    bool            read;
    bool            is_extern;
    VarKind         kind;
    VarBindingKind  binding_kind;
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
    StringDA      extra_linker_flags;
    ObjModule     top_module;
    ObjFunc*      main_function;
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
