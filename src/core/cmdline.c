#include "internal.h"
#include "utils/error.h"

#include <string.h>

Cmdline g_args;

static void print_help(void);

void process_cmdln_args(int argc, char* argv[])
{
    memset(&g_args, 0, sizeof(Cmdline));
    da_init(&g_args.input_files, 16);
    for(int i = 1; i < argc; i++)
    {
        if(argv[i][0] != '-') // Not an argument, only an input file.
        {
            da_append(&g_args.input_files, sifile_new(argv[i]));
            continue;
        }

        if(argv[i][1] == '\0') // Gcc accepts the argument '-' to mean
            continue;          // take the input from stdin, but sic won't use that.

        char* arg = argv[i] + 1;
        if(strcmp(arg, "c") == 0)
        {
            if(g_args.mode != MODE_NONE)
                sic_error_fatal("Provided multiple mode arguments.");
            g_args.mode = MODE_COMPILE;
        }
        else if(strcmp(arg, "s") == 0)
        {
            if(g_args.mode != MODE_NONE)
                sic_error_fatal("Provided multiple mode arguments.");
            g_args.mode = MODE_ASSEMBLE;
        }
        else if(strcmp(arg, "o") == 0)
        {
            if(i == argc - 1)
                sic_error_fatal("Missing filename after -o.");
            i++;
            g_args.output_file = argv[i];
        }
        else if(strcmp(arg, "emit-ir") == 0)
            g_args.emit_ir = true;
        else if(strcmp(arg, "emit-asm") == 0)
            g_args.emit_asm = true;
        else if(strcmp(arg, "-help") == 0)
            print_help();
        else if(strcmp(arg, "###") == 0)
            g_args.hash_hash_hash = true;
#ifdef SI_DEBUG
        else if(strcmp(arg, "v") == 0)
            g_args.emit_debug_output = true;
#endif
        else
            sic_error_weak("Unknown argument \'-%s\', ignoring.", arg);
    }


    if(g_args.input_files.size == 0)
        sic_error_fatal("No input files provided.");

    if(g_args.mode == MODE_NONE)
        g_args.mode = MODE_LINK;
    else if(g_args.mode != MODE_LINK && g_args.output_file != NULL && g_args.input_files.size > 1)
        sic_error_fatal("Cannot provide multiple input files to '-c' or '-s' when '-o' is used.");


    // TODO: Make this customizable through options
    g_args.target = TARGET_x86_64;
    g_args.ir_kind = IR_LLVM;
}

static void print_help(void)
{
    // Temporary, need to make this a better system so that adding new options is easier. Most likely using a table
    // for the descriptions and options
    printf("OVERVIEW: siliconc(a.k.a sic), a compiler for the silicon language.\n\n");
    printf("USAGE: sic [options] input_file...\n\n");
    printf("OPTIONS:\n");
    printf("  Compilation Mode: (Default is compile, assemble, and link)\n");
    printf("    -c          Compile only. (Output is an assembly file)\n");
    printf("    -s          Compile and assemble only. (Output is an object file)\n\n");
    printf("  Intermediate Emittion:\n");
    printf("    -emit-ir    Emits IR(default LLVM-IR) for all compiled modules, this will be done regardless of compilation mode.\n");
    printf("    -emit-asm   Emits assembly code for all compiled modules, this will be done regardless of the compilation mode.\n");
    printf("  Other:\n");
    printf("    --help      Print this message.\n");
    printf("    -###        Output subcommands executed to stdout.\n");
    exit(0);
}
