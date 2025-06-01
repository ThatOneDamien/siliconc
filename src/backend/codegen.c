#include "codegen.h"
#include "core/cmdline.h"
#include "utils/error.h"

#include "gasx86_64/codegen.h"

#define INITIAL_CAP (1 << 12)

void gen_intermediate_rep(FILE* out_file, Object* program, char* filename)
{
    StringBuilder res;
    da_init(&res, INITIAL_CAP);
    switch(args.inter_rep)
    {
    case INTER_GAS_ASM: {
        gasx86_64_codegen(out_file, program, filename);
        break;
    }
    case INTER_NONE:
    default:
        sic_error_fatal("Unknown or unimplemented intermediate representation.");
    }
}
