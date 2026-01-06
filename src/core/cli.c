#include "internal.h"
#include "utils/file_utils.h"

#include <string.h>

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
    FLAG_EMIT,
    FLAG_OUT_NAME,
    FLAG_OUT_DIR,
    __FLAG_COUNT,
} FlagKind;

static void process_emit(char* arg);
static NORETURN void print_help(void);

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
            case FT_LLVM_IR:
            case FT_ASM:
            case FT_SHARED:
            case FT_STATIC:
            case FT_UNKNOWN:
            default:
                sic_fatal_error("Unrecognized input file \'%s\'. Please only provide a silicon source file (.si).", arg);
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
        for(FlagKind j = 0; j < __FLAG_COUNT; ++j)
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
            SIC_TODO();
        case FLAG_OUT_DIR:
            if(i == argc - 1 || argv[i + 1][0] == '-')
                sic_fatal_error("Missing directory path after --out-dir.");
            i++;
            g_compiler.out_dir = argv[i];
            break;
        case FLAG_EMIT:
            if(i == argc - 1 || argv[i + 1][0] == '-')
                sic_fatal_error("Missing file kind after --emit.");
            had_emit = true;
            i++;
            process_emit(argv[i]);
            break;
        case FLAG_OUT_NAME:
            if(i == argc - 1 || argv[i + 1][0] == '-')
                sic_fatal_error("Missing filename after --out-name.");
            i++;
            g_compiler.out_name = argv[i];
            break;
        default:
            SIC_UNREACHABLE();
        }
    }

    if(g_compiler.input_file == FILE_NULL)
        sic_fatal_error("No input file provided.");

    // TODO: Make this customizable through options
    if(!had_emit)
        g_compiler.emit_link = true;

    if(!g_compiler.out_name)
    {
        SourceFile* main_file = file_from_id(g_compiler.input_file);
        scratch_clear();
        const char* file_name = strrchr(main_file->rel_path, '/');
        file_name = file_name == NULL ? main_file->rel_path : file_name + 1;
        while(file_name[0] != '\0' && file_name[0] != '.')
        {
            scratch_appendc(file_name[0]);
            file_name++;
        }
        g_compiler.out_name = str_dupn(scratch_string(), g_scratch.len);
    }
    g_compiler.target = TARGET_x86_64;
    g_compiler.ir_kind = IR_LLVM;
}

static void process_emit(char* arg)
{
    char* kind = strtok(arg, ",");
    while(kind != NULL)
    {
        if(strcmp(kind, "link") == 0)     g_compiler.emit_link = true;
        else if(strcmp(kind, "obj") == 0) g_compiler.emit_obj = true;
        else if(strcmp(kind, "asm") == 0) g_compiler.emit_asm = true;
        else if(strcmp(kind, "ir") == 0)  g_compiler.emit_ir = true;
        else
            sic_fatal_error("Unknown emit kind '%s', possible options are link,obj,asm,ir.", kind);
        kind = strtok(NULL, ",");
    }

}

static NORETURN void print_help(void)
{
    printf("About: siliconc(a.k.a sic), a compiler for the silicon language.\n\n");
    printf("Usage: sic [<OPTIONS>] INPUT\n\n");
    printf("Options:\n");

    for(size_t i = 0; i < __FLAG_COUNT; ++i)
    {
        printf("    ");
        if(s_flags[i].short_name)
        {
            printf("-%c", s_flags[i].short_name);
            if(s_flags[i].long_name)
                printf(", --%s ", s_flags[i].long_name);
            else
                putc(' ', stdout);
        }
        else
            printf("    --%s ", s_flags[i].long_name);

        if(s_flags[i].input_args)
            printf("%s ", s_flags[i].input_args);
        printf("%s\n", s_flags[i].description);
    }
    exit(EXIT_SUCCESS);
}

static const Flag s_flags[__FLAG_COUNT] = {
    [FLAG_HELP]     = { 'h', "help", "", "Print this help message." },
    [FLAG_OPTIMIZE] = { 'O', NULL, "<OPT-LEVEL>", "Optimize build to OPT-LEVEL, which can be 0,1,2,3,s,z."},
    [FLAG_EMIT]     = { 0, "emit", "<KIND>[,<KIND>]", "Emit all file types in list. KIND can be link,obj,asm,ir."},
    [FLAG_OUT_NAME] = { 0, "out-name", "<NAME>", "Set the name of the final build output (executable or library) to NAME."},
    [FLAG_OUT_DIR]  = { 0, "out-dir", "<DIR>", "Place build components in the directory DIR."},
};
