#ifdef SI_DEBUG

#include "debug.h"
#include "core/core.h"
#include "core/internal.h"

static const char* s_tok_names[] = {
    [TOKEN_INVALID]         = "Invalid",
    [TOKEN_IDENT]           = "Identifier",
    [TOKEN_STR]             = "String Literal",
    [TOKEN_NUM]             = "Numeric Literal",
    [TOKEN_AMP]             = "Ampersand",
    [TOKEN_ASTERISK]        = "Asterisk",
    [TOKEN_LOG_NOT]         = "Logical Not",
    [TOKEN_BIT_NOT]         = "Bitwise Not",
    [TOKEN_BIT_OR]          = "Bitwise Or",
    [TOKEN_BIT_XOR]         = "Bitwise Xor",
    [TOKEN_COLON]           = "Colon",
    [TOKEN_SEMI]            = "Semicolon",
    [TOKEN_ASSIGN]          = "Assign",
    [TOKEN_LT]              = "Less Than",
    [TOKEN_GT]              = "Greater Than",
    [TOKEN_DIV]             = "Divide",
    [TOKEN_PERIOD]          = "Period",
    [TOKEN_COMMA]           = "Comma",
    [TOKEN_LBRACE]          = "Left Brace",
    [TOKEN_LBRACKET]        = "Left Bracket",
    [TOKEN_LPAREN]          = "Left Paren",
    [TOKEN_RPAREN]          = "Right Paren",
    [TOKEN_RBRACKET]        = "Right Bracket",
    [TOKEN_RBRACE]          = "Right Brace",
    [TOKEN_ADD]             = "Add",
    [TOKEN_SUB]             = "Subtract",
    [TOKEN_MOD]             = "Modulo",
    [TOKEN_QUESTION]        = "Question Mark",
    [TOKEN_SHR]             = "Shift Right",
    [TOKEN_SHL]             = "Shift Left",
    [TOKEN_LOG_AND]         = "Logical And",
    [TOKEN_LOG_OR]          = "Logical Or",
    [TOKEN_LOG_EQUIV]       = "Equivalent",
    [TOKEN_LOG_NOT_EQUIV]   = "Not Equivalent",
    [TOKEN_LE]              = "Less Than Or Equal",
    [TOKEN_GE]              = "Greater Than Or Equal",
    [TOKEN_BIT_AND_ASSIGN]  = "Bitwise And Assign",
    [TOKEN_BIT_OR_ASSIGN]   = "Bitwise Or Assign",
    [TOKEN_BIT_XOR_ASSIGN]  = "Bitwise Xor Assign",
    [TOKEN_ADD_ASSIGN]      = "Add Assign",
    [TOKEN_SUB_ASSIGN]      = "Subtract Assign",
    [TOKEN_MUL_ASSIGN]      = "Multiply Assign",
    [TOKEN_DIV_ASSIGN]      = "Divide Assign",
    [TOKEN_MOD_ASSIGN]      = "Modulo Assign",
    [TOKEN_SHR_ASSIGN]      = "Shift Right Assign",
    [TOKEN_SHL_ASSIGN]      = "Shift Left Assign",
    [TOKEN_INCREM]          = "Increment",
    [TOKEN_DECREM]          = "Decrement",
    [TOKEN_RETURN]          = "return",
    [TOKEN_VOID]            = "void",
    [TOKEN_U8]              = "u8",
    [TOKEN_S8]              = "s8",
    [TOKEN_U16]             = "u16",
    [TOKEN_S16]             = "s16",
    [TOKEN_U32]             = "u32",
    [TOKEN_S32]             = "s32",
    [TOKEN_U64]             = "u64",
    [TOKEN_S64]             = "s64",
    [TOKEN_F32]             = "f32",
    [TOKEN_F64]             = "f64",
    [TOKEN_EOF]             = "End Of File",
};

static const char* type_strings[] = {
    [TYPE_INVALID]  = "Invalid", 
    [TYPE_VOID]     = "void", 
    [TYPE_U8]       = "u8", 
    [TYPE_S8]       = "s8", 
    [TYPE_U16]      = "u16", 
    [TYPE_S16]      = "s16",
    [TYPE_U32]      = "u32", 
    [TYPE_S32]      = "s32", 
    [TYPE_U64]      = "u64", 
    [TYPE_S64]      = "s64", 
    [TYPE_F32]      = "f32", 
    [TYPE_F64]      = "f64", 
};
static const char* node_type_names[] = {
    [NODE_INVALID]   = "Invalid",
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

void print_all_tokens(Lexer lexer)
{
    while(lexer_advance(&lexer))
    {
        Token* tok = lexer.la_buf.buf + lexer.la_buf.head;
        printf("%-15s: Len: %-4u   %.*s\n", 
               s_tok_names[tok->kind],
               tok->len,
               (int)tok->len, 
               tok->loc);
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
           (int)node->token.len, 
           node->token.loc, 
           node_type_names[node->kind]);

    child = node->children;
    for(size_t i = 0; i < (child_cnt + 1) / 2; ++i, child = child->next)
        print_node(child, depth + 1);

}

static void print_func(const Object* func)
{
    const ObjFunc* comps = &func->func;
    printf("Function \'%.*s\' (returns %s):\n", 
           (int)func->symbol.len,
           func->symbol.loc,
           type_strings[comps->ret_type->kind]);
    printf("  Params (count: %lu):\n", comps->param_cnt);
    Object* param = comps->params;
    for(size_t i = 0; i < comps->param_cnt; ++i)
    {
        printf("    %.*s (type %s)\n", 
               (int)param->symbol.len, 
               param->symbol.loc,
               type_strings[param->var.type->kind]);
        param = param->next;
    }

    printf("  Local Variables:\n");
    Object* local = comps->local_objs;
    while(local != NULL)
    {
        printf("    %.*s (type %s)\n", 
               (int)local->symbol.len, 
               local->symbol.loc,
               type_strings[local->var.type->kind]);
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
    while(program != NULL)
    {
        if(program->kind == OBJ_FUNC)
            print_func(program);
        program = program->next;
    }
}

#endif // SI_DEBUG
