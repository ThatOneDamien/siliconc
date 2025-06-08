#include "cmdline.h"
#include "utils/error.h"

#include <string.h>

Cmdline args;

void process_cmdln_args(int argc, char* argv[])
{
    memset(&args, 0, sizeof(Cmdline));
    da_init(&args.input_files, 16);
    for(int i = 1; i < argc; i++)
    {
        if(argv[i][0] != '-') // Not an argument, only an input file.
            da_append(&args.input_files, sifile_new(argv[i]));

        if(argv[i][1] == '\0') // Gcc accepts the argument '-' to mean
            continue;          // take the input from stdin, but sic won't use that.

        char* arg = argv[i] + 1;
        if(strcmp(arg, "p") == 0)
        {
            if(args.mode != MODE_NONE)
                sic_error_fatal("Provided multiple mode arguments.");
            args.mode = MODE_PREPROCESS;
        }
        else if(strcmp(arg, "c") == 0)
        {
            if(args.mode != MODE_NONE)
                sic_error_fatal("Provided multiple mode arguments.");
            args.mode = MODE_COMPILE;
        }
        else if(strcmp(arg, "s") == 0)
        {
            if(args.mode != MODE_NONE)
                sic_error_fatal("Provided multiple mode arguments.");
            args.mode = MODE_ASSEMBLE;
        }
        else if(strcmp(arg, "o") == 0)
        {
            if(i == argc - 1)
                sic_error_fatal("Missing filename after -o.");
            i++;
            args.output_file = argv[i];
        }
        else if(strcmp(arg, "###") == 0)
            args.hash_hash_hash = true;
    }

    if(args.mode == MODE_NONE)
        args.mode = MODE_LINK;

    if(args.input_files.size == 0)
        sic_error_fatal("No input files provided.");

    if(args.mode != MODE_LINK && args.output_file != NULL &&
       args.input_files.size > 1)
        sic_error_fatal("Cannot provide multiple input files to '-p', '-c', or '-s' when '-o' is used.");

    args.target = TARGET_x86_64;
    args.inter_rep = INTER_GAS_ASM;
}
