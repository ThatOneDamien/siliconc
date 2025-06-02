#include "codegen.h"
#include "core/core.h"

#include <stdarg.h>

static FILE* s_output;

__attribute__((format(printf, 1, 2)))
static void add_line(const char* restrict format, ...);
static void generate_func(Object* program);

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

    for(; program != NULL; program = program->next)
    {
        if(program->is_function)
            generate_func(program);
        else
            continue;
    }

    return;
}

void gasx86_64_assemble(char* input_path, char* out_path)
{
    SIC_ASSERT(out_path != NULL);
    SIC_ASSERT(input_path != NULL);
    char *cmd[] = { "as", "-o", out_path, "-c", input_path, NULL };
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
    int sym_len = (int)func->symbol->len;
    char* sym = func->symbol->ref;
    // Metadata + header
    add_line("\t.text");
    add_line("\t.global\t%.*s", sym_len, sym);
    add_line("\t.type\t%.*s, @function", sym_len, sym);
    add_line("%.*s:", sym_len, sym);

    // Function Prologue
    add_line("\tpushq\t%%rbp");
    add_line("\tmovq\t%%rsp, %%rbp");

    // FUnction Epilogue
    add_line("\tmovl\t$0, %%eax");
    add_line("\tpopq\t%%rbp");
    add_line("\tret");
    add_line("\t.size\t%.*s, .-%.*s", sym_len, sym, sym_len, sym);
}

