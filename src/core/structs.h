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
typedef struct TypeArray        TypeArray;
typedef struct TypeBuiltin      TypeBuiltin;
typedef struct FuncSignature*   TypeFuncPtr;
typedef struct Type*            TypePointer;
typedef struct SourceLoc        TypeUnresolved;
typedef struct Object*          TypeUserdef;
typedef struct Type             Type;

// Dynamic Array Structs
typedef struct ObjectDA         ObjectDA;
typedef struct ASTExprDA        ASTExprDA;
typedef struct ASTDeclDA        ASTDeclDA;
typedef struct CompUnitDA       CompUnitDA;
typedef struct SIFileDA         SIFileDA;
typedef struct ModuleDA         ModuleDA;
typedef struct ModulePTRDA      ModulePTRDA;

// AST Structs
typedef struct Type*            ASTAmbiguous;
typedef struct ASTBlock         ASTBlock;
typedef struct ASTDeclaration   ASTDeclaration;
typedef struct ASTExprAAccess   ASTExprAAccess;
typedef struct ASTExprBinary    ASTExprBinary;
typedef struct ASTExprCall      ASTExprCall;
typedef struct ASTExprCast      ASTExprCast;
typedef struct ASTExprConstant  ASTExprConstant;
typedef struct Object*          ASTExprIdent;
typedef struct ASTExprMAccess   ASTExprMAccess;
typedef struct ASTExprUnary     ASTExprUnary;
typedef struct ASTExprUAccess   ASTExprUAccess;
typedef struct ASTExpr          ASTExpr;
typedef struct ASTIf            ASTIf;
typedef struct ASTReturn        ASTReturn;
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
typedef struct CompilationUnit  CompilationUnit;
typedef struct Module           Module;
typedef struct Cmdline          Cmdline;
typedef struct CompilerContext  CompilerContext;

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
    };
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
    ASTStmt* body;
};

struct ASTDeclaration
{
    Object*  obj;
    ASTExpr* init_expr;
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
    ASTExpr* expr_to_cast;
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

struct ASTExprUnary
{
    ASTExpr*    child;
    UnaryOpKind kind;
};

struct ASTExprUAccess
{
    ASTExpr* parent_expr;
    ASTExpr* member_expr;
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
        ASTExprUnary    unary;
        ASTExprUAccess  unresolved_access;
    } expr;
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
        ASTAmbiguous    ambiguous;
        ASTBlock        block;
        ASTDeclaration  single_decl;
        ASTDeclDA       multi_decl;
        ASTExpr*        expr;
        ASTIf           if_;
        ASTReturn       return_;
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
    ObjectDA       local_objs;
};

struct ObjStruct
{
    ObjectDA      members;
    ResolveStatus status;
    uint32_t      size;
    uint32_t      align;
};

struct ObjVar
{
    Type*    type;
    uint32_t member_idx;
};

struct Object
{
    SourceLoc symbol;
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
    Scope*  parent;
    HashMap vars;
    HashMap types;
};

struct CompilationUnit
{
    SIFile   file;
    ObjectDA funcs;
    ObjectDA types;
    ObjectDA vars;
};

struct CompUnitDA
{
    CompilationUnit** data;
    size_t            capacity;
    size_t            size;
};

struct SIFileDA
{
    SIFile* data;
    size_t  capacity;
    size_t  size;
};

struct ModuleDA
{
    Module* data;
    size_t  capacity;
    size_t  size;
};

struct ModulePTRDA
{
    Module** data;
    size_t   capacity;
    size_t   size;
};

// For now this definition is really basic, later on I will add a proper
// hierarchy of modules.
struct Module
{
    const char* name;
    CompUnitDA  units;
    HashMap     symbols;
};

struct Cmdline
{
    SIFileDA        input_files;
    char*           output_file;

    CompileMode     mode;
    CompileTarget   target;
    IRTarget        ir_kind;

    bool            emit_ir;
    bool            emit_asm;
    bool            hash_hash_hash;
#ifdef SI_DEBUG
    bool            emit_debug_output;
#endif
};

struct CompilerContext
{
    SIFileDA    linker_inputs;
    Module      top_module;
    ModulePTRDA modules_to_compile;
};
