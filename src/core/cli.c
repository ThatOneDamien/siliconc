#include "internal.h"
#include "utils/file_utils.h"

#include <string.h>

static NORETURN void print_help(void);

typedef struct
{
    char        short_name;
    const char* long_name;
    const char* input_args;
    const char* description;
} Flag;

typedef enum
{
    FLAG_HELP,
    FLAG_OPTIMIZE,
    FLAG_OUT_DIR,
    FLAG_OUT_NAME,
    __FLAG_COUNT,
} FlagKind;

static const Flag s_flags[__FLAG_COUNT];

void process_args(int argc, char* argv[])
{
    bool had_emit = false;
    for(int i = 1; i < argc; i++)
    {
        char* arg = argv[i];
        if(arg[0] != '-') // Not an argument, only an input file.
        {
            switch(get_filetype(arg))
            {
            case FT_SI:
                if(g_compiler.input_file != FILE_NULL)
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

        FlagKind kind = -1;

        if(arg[0] != '-')
        {
#ifdef SI_DEBUG
            if(arg[0] == 'v')
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
                continue;
            }
#endif
            for(FlagKind j = 0; j < __FLAG_COUNT; ++j)
            {
                if(s_flags[j].short_name != 0 && arg[0] == s_flags[j].short_name)
                {
                    kind = j;
                    goto HANDLE_FLAG;
                }
            }
            sic_fatal_error("Unknown flag \'-%s\'.", arg);
        }

        arg++;
        for(size_t j = 0; j < __FLAG_COUNT; ++j)
        {
            if(s_flags[j].long_name != NULL && strcmp(arg, s_flags[j].long_name) == 0)
            {
                kind = j;
                goto HANDLE_FLAG;
            }
        }
        sic_fatal_error("Bad argument.");

HANDLE_FLAG:
        switch(kind)
        {
        case FLAG_HELP:
            print_help();
        case FLAG_OPTIMIZE:
        case FLAG_OUT_DIR:
        case FLAG_OUT_NAME:
            if(i == argc - 1 || argv[i + 1][0] == '-')
                sic_fatal_error("Missing filename after --oname.");
            i++;
            g_compiler.link_name = argv[i];
            break;
        default:
            SIC_UNREACHABLE();
        }
    }


    // TODO: Make this customizable through options
    if(!had_emit)
        g_compiler.emit_link = true;
    g_compiler.target = TARGET_x86_64;
    g_compiler.ir_kind = IR_LLVM;
}


static NORETURN void print_help(void)
{
    printf("About: siliconc(a.k.a sic), a compiler for the silicon language.\n\n");
    printf("Usage: sic [<OPTIONS>] INPUT\n\n");
    printf("Options:\n");
    SIC_TODO();
    // for(size_t i = 0; i < __FLAG_COUNT; ++i)
    // {
    //     printf("    ");
    //     if(!s_flags[i].short_name)
    //         printf("    ");
    //     else
    //     {
    //         printf("-%c", s_flags[i].short_name);
    //         if()
    //     }
    //
    // }
    exit(0);
}

static const Flag s_flags[__FLAG_COUNT] = {
    [FLAG_HELP]     = { 'h', "help", NULL, "Print this help message." },
    [FLAG_OPTIMIZE] = { 'O', NULL, "<OPT-LEVEL>", "Optimize build to OPT-LEVEL, which can be 0,1,2,3,s,z."},
    [FLAG_OUT_DIR]  = { 0, "out-dir", "<DIR>", "Place build components in the directory DIR."},
    [FLAG_OUT_NAME] = { 0, "oname", "<NAME>", "Set the name of the final build output (executable or library) to NAME."},
};
