#include "codegen.h"
#include "core/core.h"
#include "utils/error.h"

#include <stdarg.h>

#define align_up(x, alignment) (((x) + (alignment) - 1) / (alignment) * (alignment))

static FILE*   s_output;
static Object* s_cur_func;

__attribute__((format(printf, 1, 2)))
static void add_line(const char* restrict format, ...);
static void generate_func(Object* program);
static void generate_statement(ASTNode* node);
static void generate_expr(ASTNode* node);
static void assign_offsets(Object* func);

void gasx86_64_codegen(Object* program, char* input_path, FILE* out_file)
{
    SIC_ASSERT(program != NULL);
    SIC_ASSERT(input_path != NULL);
    SIC_ASSERT(out_file != NULL);


    char* input_filename = strrchr(input_path, '/');

    if(input_filename == NULL)
        input_filename = input_path;
    else
        input_filename++;


    s_output = out_file;
    add_line("\t.file\t\"%s\"", input_filename);

    while(program != NULL)
    {
        if(program->is_function)
            generate_func(program);
        program = program->next;
    }

    return;
}

void gasx86_64_assemble(char* input_path, char* out_path)
{
    SIC_ASSERT(out_path != NULL);
    SIC_ASSERT(input_path != NULL);
    char *cmd[] = { "as", "--64", "-c", input_path, "-o", out_path, NULL };
    run_subprocess(cmd);
}

static void add_line(const char* restrict format, ...)
{
    va_list va;
    va_start(va, format);
    vfprintf(s_output, format, va);
    putc('\n', s_output);
    va_end(va);
}

static void generate_func(Object* func)
{
    s_cur_func = func;
    assign_offsets(func);
    int sym_len = (int)func->symbol->len;
    char* sym = func->symbol->ref;
    FuncComps* comps = &func->comps.func;
    // Metadata + header
    add_line("\t.text");
    add_line("\t.global\t%.*s", sym_len, sym);
    add_line("\t.type\t%.*s, @function", sym_len, sym);
    add_line("%.*s:", sym_len, sym);

    // Function Prologue
    add_line(".L.return.%.*s:", sym_len, sym);
    add_line("\tpushq\t%%rbp");
    add_line("\tmovq\t%%rsp, %%rbp");
    if(comps->stack_size > 0)
        add_line("\tsubq\t$%d, %%rsp", comps->stack_size);

    // size_t gpr = 0;
    // size_t fpr = 0;
    // for(Object* param = comps->params; param != NULL; param = param->next)
    // {
    //
    // }

    // Function Epilogue

    if(comps->stack_size > 0)
        add_line("\tmovq\t%%rbp, %%rsp");
    add_line("\tpopq\t%%rbp");
    add_line("\tret");
    add_line("\t.size\t%.*s, .-%.*s", sym_len, sym, sym_len, sym);
}

static UNUSED void generate_statement(ASTNode* node)
{
    switch(node->type)
    {
    case NODE_RETURN: {
        if(node->children)
            generate_expr(node->children);
        add_line("\tjmp\t.L.return.%.*s", (int)s_cur_func->symbol->len, s_cur_func->symbol->ref);
        break;
    }
    default:
        sic_error_fatal("Unimplemented node type.");
    }
}

static void generate_expr(ASTNode* node)
{
    switch(node->type)
    {
    case NODE_NUM: {
        // switch(node->)
        break;
    }
    case NODE_ASSIGN:
        break;
    default:
        sic_error_fatal("Unimplemented node type.");
    }
}

static void assign_offsets(Object* func)
{
    FuncComps* comps = &func->comps.func;
    // TODO: Assign offsets to parameters passed on stack.
    // for(Object* param = comps->params; param != NULL; param = param->next)
    // {
    //
    // }

    int stack_size = 0;
    for(Object* lvar = comps->params; lvar != NULL; lvar = lvar->next)
    {
        VarComps* vcomp = &lvar->comps.var;
        stack_size = align_up(stack_size + vcomp->type->size, vcomp->type->alignment);
        vcomp->offset = -stack_size;
    }
    comps->stack_size = align_up(stack_size, 16);
}

