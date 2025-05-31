#include "codegen.h"
#include "core/core.h"

#include <stdarg.h>

static StringBuilder* output;

static void generate_func(Object* program);

void gasx86_64_codegen(StringBuilder* out, Object* program, char* filename)
{
    SIC_ASSERT(out != NULL);
    SIC_ASSERT(program != NULL);
    output = out;
    sb_appendf(output, "\t.file\t\"%s\"\n", filename);

    for(; program != NULL; program = program->next)
    {
        if(program->is_function)
            generate_func(program);
        else
            continue;
    }

    sb_append_null(output);
    return;
}

static void generate_func(Object* func)
{
    // Metadata + header
    sb_appendf(output, "\t.text\n");
    sb_appendf(output, "\t.global\t%.*s\n", (int)func->symbol->len, func->symbol->ref);
    sb_appendf(output, "\t.type\t%.*s, @function\n", (int)func->symbol->len, func->symbol->ref);
    sb_appendf(output, "%.*s:\n", (int)func->symbol->len, func->symbol->ref);

    // Object* param = func->comps.func.params;
    // for(size_t i = 0; i < func->comps.func.param_cnt; ++i)
    // {
    //     printf("PARAM %lu: TYPE: %u SYMBOL: %.*s\n", 
    //            i,
    //            param->comps.var.type->kind,
    //            (int)param->symbol->len,
    //            param->symbol->ref);
    //     param = param->next;
    // }

    // Function Prologue
    sb_appendf(output, "\tpushq\t%%rbp\n");
    sb_appendf(output, "\tmovq\t%%rsp, %%rbp\n");
}
