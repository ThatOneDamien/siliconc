#include "codegen.h"
#include "core/cmdline.h"
#include "utils/error.h"
#include "utils/file_utils.h"

#include "gasx86_64.h"

#define INITIAL_CAP (1 << 12)

void gen_intermediate_rep(const CompilationUnit* unit, const SIFile* output)
{
    SIC_ASSERT(output != NULL);
    SIC_ASSERT(output->full_path != NULL);

    FILE* out_file = open_out_file(output);

    switch(args.inter_rep)
    {
    case INTER_GAS_ASM: {
        gasx86_64_codegen(unit, out_file);
        break;
    }
    case INTER_NONE:
    default:
        sic_error_fatal("Unknown or unimplemented intermediate representation.");
    }
    fclose(out_file);
}

void assemble_intermediate(const char* input_path, const char* out_path)
{
    SIC_ASSERT(out_path != NULL);
    SIC_ASSERT(input_path != NULL);

    switch(args.inter_rep)
    {
    case INTER_GAS_ASM:
        gasx86_64_assemble(input_path, out_path);
        break;
    case INTER_NONE:
    default:
        sic_error_fatal("Unknown or unimplemented intermediate representation.");
    }
}
