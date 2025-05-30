#pragma once
#include "lexer.h"

typedef struct Object  Object;
typedef struct ASTNode ASTNode;
typedef enum
{
    NODE_NONE = 0,
    NODE_ADD,
    NODE_ASSIGN,
    NODE_BLOCK,
} NodeType;

struct Object
{
    Object*  next;
    char*    symbol;

    // Function body
    ASTNode* body;
};

struct ASTNode
{
    NodeType type;
    Token*   token;
};

Object* parse_tokens(Token* tokens);
