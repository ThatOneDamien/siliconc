#ifdef SI_DEBUG

#include "debug.h"
#include "core/core.h"

static const char* tok_type_names[] = {
    "NULL           ",
    "Identifier     ",
    "Separator      ",
    "Keyword        ",
    "String Literal ",
    "Numeric Literal",
    "End Of File    "
};

static const char* type_strings[] = {
    "NULL", "void", "u8", "s8", "u16", "s16",
    "u32", "s32", "u64", "s64", "f32", "f64", 
    "f128", 
};

static const char* node_type_names[] = {
    "NULL",
    "Add",
    "Assign",
    "Block",
    "Variable",
    "Numeric Literal",
    "Return"
};

void print_all_tokens(const Token* tok)
{
    SIC_ASSERT(tok != NULL);
    while(tok != NULL)
    {
        printf("%s: Len: %-4lu   %.*s\n", 
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
