#include "cmdline.h"

#include <string.h>

Cmdline args;

void process_cmdln_args(int argc, char* argv[])
{
    memset(&args, 0, sizeof(Cmdline));
    da_init(&args.input_files, 16);
    for(int i = 1; i < argc; i++)
    {
        if(argv[i][0] != '-') // Not an argument, only an input file.
            da_append(&args.input_files, argv[i]);
    }
}
