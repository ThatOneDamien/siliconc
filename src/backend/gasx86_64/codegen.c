#include "codegen.h"
#include "core/core.h"

#include <stdarg.h>

static FILE* s_output;

__attribute__((format(printf, 1, 2)))
static void add_line(const char* restrict format, ...);
static void generate_func(Object* program);

void gasx86_64_codegen(FILE* out_file, Object* program, char* filename)
{
    SIC_ASSERT(out_file != NULL);
    SIC_ASSERT(program != NULL);
    s_output = out_file;
    add_line("\t.file\t\"%s\"", filename);

    for(; program != NULL; program = program->next)
    {
        if(program->is_function)
            generate_func(program);
        else
            continue;
    }

    return;
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
