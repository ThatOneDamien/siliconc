#include "gasx86_64.h"
#include "x86_globals.h"
#include "core/core.h"
#include "utils/error.h"

#include <stdarg.h>

#define align_up(x, alignment) (((x) + (alignment) - 1) / (alignment) * (alignment))

static FILE*    s_output;
static Object*  s_cur_func;
static ASTNode* s_last_node;

__attribute__((format(printf, 1, 2)))
static void add_line(const char* restrict format, ...);
static void generate_func(Object* program);
static void generate_statement(ASTNode* node);
// static void generate_expr(ASTNode* node, x86Reg out_reg);
static void assign_offsets(Object* func);

void gasx86_64_codegen(const CompilationUnit* unit, FILE* out_file)
{
    SIC_ASSERT(out_file != NULL);

    s_output = out_file;
    add_line("\t.file\t\"%s\"", unit->file.file_name);

    // TODO: Fix this, it is incorrect, i am temporarily disabling codegen
    Object* program = unit->funcs.data[0];
    while(program != NULL)
    {
        if(program->kind == OBJ_FUNC)
            generate_func(program);
        program = program->next;
    }

    return;
}

void gasx86_64_assemble(const char* input_path, const char* out_path)
{
    SIC_ASSERT(out_path != NULL);
    SIC_ASSERT(input_path != NULL);
    char *cmd[] = { "as", "--64", "-c", (char*)input_path, "-o", (char*)out_path, NULL };
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
    int sym_len = (int)func->symbol.len;
    const char* sym = func->symbol.loc;
    ObjFunc* comps = &func->func;
    // Metadata + header
    add_line("\t.text");
    add_line("\t.global\t%.*s", sym_len, sym);
    add_line("\t.type\t%.*s, @function", sym_len, sym);
    add_line("%.*s:", sym_len, sym);

    // Function Prologue
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

    for(ASTNode* b = comps->body; b != NULL; b = b->next)
        generate_statement(b);

    add_line(".L.return.%.*s:", sym_len, sym);
    if(comps->stack_size > 0)
        add_line("\tmovq\t%%rbp, %%rsp");
    add_line("\tpopq\t%%rbp");
    add_line("\tret");
    add_line("\t.size\t%.*s, .-%.*s", sym_len, sym, sym_len, sym);
}

static void generate_statement(ASTNode* node)
{
    s_last_node = node;
    // switch(node->kind)
    // {
    // case NODE_RETURN: {
    //     if(node->)
    //         generate_expr(node->children, x86_RAX);
    //     add_line("\tjmp\t.L.return.%.*s", (int)s_cur_func->symbol.len, s_cur_func->symbol.loc);
    //     break;
    // }
    // default:
    //     generate_expr(node, x86_RAX);
    // }
}

// static void generate_expr(ASTNode* node, x86Reg out_reg)
// {
//     s_last_node = node;
//     switch(node->kind)
//     {
//     case NODE_NUM: {
//         add_line("\tmovq\t$%.*s, %%%s", (int)node->token.len, node->token.loc, x86_64_64bitreg[out_reg]);
//         break;
//     }
//     case NODE_ASSIGN: {
//         generate_expr(node->children->next, x86_RAX);
//         add_line("\tmovq\t%%rax, %d(%%rbp)", node->children->var->var.offset);
//         break;
//     }
//     case NODE_VAR: {
//         add_line("\tmovq\t%d(%%rbp), %%%s", node->var->var.offset, x86_64_64bitreg[out_reg]);
//         break;
//     }
//     default:
//         sic_error_fatal("Unimplemented node type.");
//     }
// }

static void assign_offsets(Object* func)
{
    ObjFunc* comps = &func->func;
    // TODO: Assign offsets to parameters passed on stack.
    // for(Object* param = comps->params; param != NULL; param = param->next)
    // {
    //
    // }

    int stack_size = 0;
    for(Object* lvar = comps->local_objs; lvar != NULL; lvar = lvar->next)
    {
        ObjVar* vcomp = &lvar->var;
        uint32_t size = type_size(vcomp->type);
        stack_size = align_up(stack_size + size, size);
        vcomp->offset = -stack_size;
    }
    comps->stack_size = align_up(stack_size, 16);
}

