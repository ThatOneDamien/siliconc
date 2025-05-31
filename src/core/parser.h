#pragma once
#include "lexer.h"

typedef struct FuncComps FuncComps;
typedef struct VarComps  VarComps;
typedef struct Object    Object;
typedef struct ASTNode   ASTNode;
typedef enum
{
    NODE_NONE = 0,
    NODE_ADD,
    NODE_ASSIGN,
    NODE_BLOCK,
    NODE_VAR,
    NODE_NUM,
} NodeType;

struct FuncComps
{
    Type*    ret_type;
    Object*  params;
    size_t   param_cnt;
    Object*  local_objs;
    ASTNode* body;
};

struct VarComps
{
    Type* type;
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
