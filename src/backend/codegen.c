#include "codegen.h"
#include "codegen-internal.h"
#include "utils/error.h"
#include "utils/file_utils.h"

#define INITIAL_CAP (1 << 12)

void gen_ir(ModulePTRDA* modules)
{
    switch(g_args.ir_kind)
    {
    case IR_LLVM: {
        llvm_codegen(modules);
        break;
    }
    default:
        SIC_TODO();
    }
}

// void assemble_intermediate(const char* input_path, const char* out_path)
// {
//     SIC_ASSERT(out_path != NULL);
//     SIC_ASSERT(input_path != NULL);
//
//     switch(args.inter_rep)
//     {
//     case INTER_GAS_ASM:
//         gasx86_64_assemble(input_path, out_path);
//         break;
//     case INTER_NONE:
//     default:
//         sic_error_fatal("Unknown or unimplemented intermediate representation.");
//     }
// }
