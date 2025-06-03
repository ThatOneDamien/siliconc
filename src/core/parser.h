#pragma once
#include "lexer.h"

typedef struct FuncComps FuncComps;
typedef struct VarComps  VarComps;
typedef struct Object    Object;
typedef struct ASTNode   ASTNode;
typedef enum
{
    NODE_NONE = 0,
    NODE_NOP,
    NODE_BLOCK,
    NODE_VAR,
    NODE_NUM,
    NODE_RETURN,

    NODE_ASSIGN,
    NODE_TERNARY,
    NODE_LOG_OR,
    NODE_LOG_AND,
    NODE_BIT_OR,
    NODE_BIT_XOR,
    NODE_BIT_AND,
    NODE_EQ,
    NODE_NE,
    NODE_LT,
    NODE_LE,
    NODE_SHL,
    NODE_SHR,
    NODE_ADD,
    NODE_SUB,
    NODE_MUL,
    NODE_DIV,
    NODE_MOD,
    NODE_CAST,
    NODE_INC,
    NODE_DEC,
    NODE_NEG,
    NODE_LOG_NOT,
    NODE_BIT_NOT,
    NODE_ADDR_OF,
    NODE_FUNC_CALL,
    NODE_DEREF,
} NodeType;

struct FuncComps
{
    Type*    ret_type;
    Object*  local_objs;
    ASTNode* body;
    Object*  params;
    size_t   param_cnt;
    int      stack_size;
};

struct VarComps
{
    Type* type;
    int   offset;
};

struct Object
{
    Object*  next;
    Token*   symbol;
    bool     is_function;

    union
    {
        FuncComps func; // Components of function
        VarComps  var;  // Components of variable
    } comps;

};

struct ASTNode
{
    NodeType type;
    Token*   token;
    Object*  var;
    ASTNode* children;
    ASTNode* next;
};

void    init_parser(void);
Object* parse_tokens(Token* tokens);
