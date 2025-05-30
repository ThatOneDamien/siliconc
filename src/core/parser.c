#include "parser.h"
#include "core.h"

typedef struct Scope Scope;

// struct Scope
// {
//
// };

// Global check and add functions
// static bool add_global_function(Token** token);

// Grammar definitions
// static ASTNode* declaration(Token** token);

// Scope and variable/function creation

// Token Helpers
static bool consume(Token** token, char* expected);

// static Object* gobjs;
// static Object* lobjs;


// Object* parse_tokens(Token* tokens)
// {
//     SIC_ASSERT(tokens != NULL);
//     globals = NULL;
//     while(tokens->type != TOKEN_EOF)
//     {
//          if(add_global_function(&tokens))
//              continue;
//         tokens = tokens->next;
//     }
//     return globals;
// }

// static bool add_global_function(Token** token)
// {
//     Token* t = *token;
//     if(t->type != TOKEN_IDNT || 
//        tok_equal(t->next, "{"))
//         return false;
//     while()
//
//
//     return false;
// }
//
// static ASTNode* declaration(Token** token)
// {
// }
//
static bool UNUSED consume(Token** token, char* expected)
{
    Token* t = *token;
    if(!tok_equal(t, expected))
        return false;
    *token = t->next;
    return true;
}
