#include "codegen.h"
#include "core/cmdline.h"
#include "utils/error.h"

#include "gasx86_64/codegen.h"

#define INITIAL_CAP (1 << 12)

StringBuilder gen_intermediate_rep(Object* program, char* filename)
{
    StringBuilder res;
    da_init(&res, INITIAL_CAP);
    switch(args.inter_rep)
    {
    case INTER_GAS_ASM: {
        gasx86_64_codegen(&res, program, filename);
        break;
    }
    case INTER_NONE:
    default:
        sic_error_fatal("Unknown or unimplemented intermediate representation.");
    }
    return res;
}
