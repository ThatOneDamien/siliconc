#include "codegen.h"
#include "codegen-internal.h"

#define INITIAL_CAP (1 << 12)

void gen_ir()
{
    switch(g_args.ir_kind)
    {
    case IR_LLVM: {
        llvm_codegen();
        return;
    }
    case IR_NONE:
        break;
    }
    SIC_UNREACHABLE();
}
