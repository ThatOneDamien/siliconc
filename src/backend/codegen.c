#include "codegen.h"
#include "codegen-internal.h"

#define INITIAL_CAP (1 << 12)

void gen_ir(ModulePTRDA* modules)
{
    switch(g_args.ir_kind)
    {
    case IR_LLVM: {
        llvm_codegen(modules);
        return;
    }
    case IR_NONE:
        break;
    }
    SIC_UNREACHABLE();
}
