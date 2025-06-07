#include "codegen.h"
#include "core/cmdline.h"
#include "utils/error.h"
#include "utils/file_utils.h"

#include "gasx86_64.h"

#define INITIAL_CAP (1 << 12)

void gen_intermediate_rep(Object* program, const char* input_path, const char* out_path)
{
    SIC_ASSERT(program != NULL);
    SIC_ASSERT(input_path != NULL);
    SIC_ASSERT(out_path != NULL);

    FILE* out_file = open_out_file(out_path);
    if(out_file == NULL)
        sic_error_fatal("Unable to open/create file \'%s\'.", out_path);

    switch(args.inter_rep)
    {
    case INTER_GAS_ASM: {
        gasx86_64_codegen(program, input_path, out_file);
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
