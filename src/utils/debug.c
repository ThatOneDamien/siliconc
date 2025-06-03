#ifdef SI_DEBUG

#include "debug.h"
#include "core/core.h"
#include "core/lexer.h"
#include "core/parser.h"

static const char* tok_type_names[] = {
    [TOKEN_NONE]    = "NULL",
    [TOKEN_IDNT]    = "Identifier",
    [TOKEN_SEP]     = "Separator",
    [TOKEN_KEYWORD] = "Keyword",
    [TOKEN_STR]     = "String Literal",
    [TOKEN_NUM]     = "Numeric Literal",
    [TOKEN_EOF]     = "End Of File",
};

static const char* type_strings[] = {
    [TYPE_NONE] = "NULL", 
    [TYPE_VOID] = "void", 
    [TYPE_U8]   = "u8", 
    [TYPE_S8]   = "s8", 
    [TYPE_U16]  = "u16", 
    [TYPE_S16]  = "s16",
    [TYPE_U32]  = "u32", 
    [TYPE_S32]  = "s32", 
    [TYPE_U64]  = "u64", 
    [TYPE_S64]  = "s64", 
    [TYPE_F32]  = "f32", 
    [TYPE_F64]  = "f64", 
    [TYPE_F128] = "f128", 
};
static const char* node_type_names[] = {
    [NODE_NONE]      = "NULL",
    [NODE_BLOCK]     = "Block",
    [NODE_VAR]       = "Variable",
    [NODE_NUM]       = "Number Literal",
    [NODE_RETURN]    = "Return",
    [NODE_ASSIGN]    = "Assign",
    [NODE_TERNARY]   = "Ternary",
    [NODE_LOG_OR]    = "Logical Or",
    [NODE_LOG_AND]   = "Logical And",
    [NODE_BIT_OR]    = "Bitwise Or",
    [NODE_BIT_XOR]   = "Bitwise Exclusive Or",
    [NODE_BIT_AND]   = "Bitwise And",
    [NODE_EQ]        = "Equal",
    [NODE_NE]        = "Not Equal",
    [NODE_LT]        = "Less Than",
    [NODE_LE]        = "Less Than Or Equal To",
    [NODE_SHL]       = "Shift Left",
    [NODE_SHR]       = "Shift Right",
    [NODE_ADD]       = "Add",
    [NODE_SUB]       = "Subtract",
    [NODE_MUL]       = "Multiply",
    [NODE_DIV]       = "Divide",
    [NODE_MOD]       = "Mod",
    [NODE_CAST]      = "Cast",
    [NODE_INC]       = "Increment",
    [NODE_DEC]       = "Decrement",
    [NODE_NEG]       = "Negate",
    [NODE_LOG_NOT]   = "Logical Not",
    [NODE_BIT_NOT]   = "Bitwise Not",
    [NODE_ADDR_OF]   = "Address Of",
    [NODE_FUNC_CALL] = "Function Call",
    [NODE_DEREF]     = "Dereference"
};

void print_all_tokens(const Token* tok)
{
    SIC_ASSERT(tok != NULL);
    while(tok != NULL)
    {
        printf("%-15s: Len: %-4lu   %.*s\n", 
               tok_type_names[tok->type],
               tok->len,
               (int)tok->len, 
               tok->ref);
        tok = tok->next;
    }
}

static void print_node(const ASTNode* node, int depth)
{
    if(node == NULL)
        return;

    size_t child_cnt = 0;
    ASTNode* child = node->children;
    while(child != NULL)
    {
        child_cnt++;
        child = child->next;
    }

    child = node->children;
    for(size_t i = 0; i < (child_cnt + 1) / 2; ++i, child = child->next)
        ;
    for(size_t i = 0; i < child_cnt / 2; ++i, child = child->next)
        print_node(child, depth + 1);

    for(int i = 0; i < depth; ++i)
        printf("  ");

    printf("(%.*s Type: %s)\n", 
           (int)node->token->len, 
           node->token->ref, 
           node_type_names[node->type]);

    child = node->children;
    for(size_t i = 0; i < (child_cnt + 1) / 2; ++i, child = child->next)
        print_node(child, depth + 1);

}

static void print_func(const Object* func)
{
    const FuncComps* comps = &func->comps.func;
    printf("Function \'%.*s\' (returns %s):\n", 
           (int)func->symbol->len,
           func->symbol->ref,
           type_strings[comps->ret_type->kind]);
    printf("  Params (count: %lu):\n", comps->param_cnt);
    Object* param = comps->params;
    for(size_t i = 0; i < comps->param_cnt; ++i)
    {
        printf("    %.*s (type %s)\n", 
               (int)param->symbol->len, 
               param->symbol->ref,
               type_strings[param->comps.var.type->kind]);
        param = param->next;
    }

    printf("  Local Variables:\n");
    Object* local = comps->local_objs;
    while(local != NULL)
    {
        printf("    %.*s (type %s)\n", 
               (int)local->symbol->len, 
               local->symbol->ref,
               type_strings[local->comps.var.type->kind]);
        local = local->next;
    }

    printf("  Body:\n");
    ASTNode* cur_node = comps->body;
    while(cur_node != NULL)
    {
        print_node(cur_node, 2);
        cur_node = cur_node->next;
    }
    printf("\n");
}

void print_program(const Object* program)
{
    SIC_ASSERT(program != NULL);
    while(program != NULL)
    {
        if(program->is_function)
            print_func(program);
        program = program->next;
    }
}

#endif // SI_DEBUG
