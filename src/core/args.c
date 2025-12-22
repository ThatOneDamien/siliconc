#include "internal.h"
#include "utils/file_utils.h"

#include <string.h>

static void print_help(void);

void process_args(int argc, char* argv[])
{
    for(int i = 1; i < argc; i++)
    {
        char* arg = argv[i];
        if(arg[0] != '-') // Not an argument, only an input file.
        {
            switch(get_filetype(arg))
            {
            case FT_SI:
                if(g_compiler.input_file < g_compiler.sources.size)
                    sic_fatal_error("Only 1 entry file may be provided.");
                g_compiler.input_file = source_file_add_or_get(argv[i], &g_compiler.top_module);
                continue;
            case FT_OBJ:
                da_append(&g_compiler.linker_inputs, arg);
                continue;
            case FT_LLVM_IR:
            case FT_ASM:
            case FT_SHARED:
            case FT_STATIC:
                SIC_TODO();
            case FT_UNKNOWN:
            default:
                sic_fatal_error("Input file \'%s\' has invalid extension.", arg);
            }
            continue;
        }

        arg++;

        if(arg[0] == '\0') // Gcc accepts the argument '-' to mean
            continue;          // take the input from stdin, but sic won't use that.

        if(strcmp(arg, "c") == 0)
        {
            if(g_compiler.mode != MODE_NONE)
                sic_fatal_error("Provided multiple mode arguments.");
            g_compiler.mode = MODE_COMPILE;
        }
        else if(strcmp(arg, "s") == 0)
        {
            if(g_compiler.mode != MODE_NONE)
                sic_fatal_error("Provided multiple mode arguments.");
            g_compiler.mode = MODE_ASSEMBLE;
        }
        else if(strcmp(arg, "o") == 0)
        {
            if(i == argc - 1 || argv[i + 1][0] == '-')
                sic_fatal_error("Missing filename after -o.");
            i++;
            g_compiler.output_file_name = argv[i];
        }
        else if(strcmp(arg, "Werror") == 0)
            g_compiler.werror = true;
        else if(strcmp(arg, "emit-ir") == 0)
            g_compiler.emit_ir = true;
        else if(strcmp(arg, "emit-asm") == 0)
            g_compiler.emit_asm = true;
        else if(strcmp(arg, "-help") == 0)
            print_help();
        else if(strcmp(arg, "###") == 0)
            g_compiler.hash_hash_hash = true;
#ifdef SI_DEBUG
        else if(arg[0] == 'v')
        {
            while(*(++arg) != '\0')
            {
                if(arg[0] == 'l')
                    g_compiler.debug_output |= DEBUG_LEXER;
                else if(arg[0] == 'p')
                    g_compiler.debug_output |= DEBUG_PARSER;
                else if(arg[0] == 's')
                    g_compiler.debug_output |= DEBUG_SEMA;
                else if(arg[0] == 'c')
                    g_compiler.debug_output |= DEBUG_CODEGEN;
                else if(arg[0] == 'm')
                    g_compiler.debug_output |= DEBUG_MEMORY;
            }
        }
#endif
        else
            sic_diagnostic(DIAG_NOTE, "Unknown argument \'-%s\', ignoring.", arg);
    }


    if(g_compiler.mode == MODE_NONE)
        g_compiler.mode = MODE_LINK;


    // TODO: Make this customizable through options
    g_compiler.target = TARGET_x86_64;
    g_compiler.ir_kind = IR_LLVM;
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
