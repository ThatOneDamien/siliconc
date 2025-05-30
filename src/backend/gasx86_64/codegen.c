#include "codegen.h"
#include "core/core.h"

void gasx86_64_codegen(StringBuilder* out, Object* program)
{
    sb_appendf(out, "Poopy %i\n", 1);
    return;
    SIC_ASSERT(out != NULL);
    SIC_ASSERT(program != NULL);
}
