#pragma once
#include "utils/da.h"

typedef enum
{
    MODE_NONE = 0,   // NULL Mode
    MODE_PREPROCESS, // Preprocess only
    MODE_COMPILE,    // Preprocess + Compile
    MODE_ASSEMBLE,   // Preprocess + Compile + Assemble
    MODE_LINK        // Preprocess + Compile + Assemble + Link
} CompileMode;

// For now only GAS is going to be used, in the future nasm
// and llvm intermediate representation will be usable.
typedef enum
{
    INTER_NONE = 0,
    INTER_GAS_ASM,
    // INTER_NASM_ASM,
    // INTER_LLVM,    
} IntermediateRep;

// Could be expanded later to support multiple architectures.
typedef enum
{
    TARGET_NONE = 0,
    TARGET_x86_64
} TargetArch;

typedef struct Cmdline Cmdline;
struct Cmdline
{
    StringArray     input_files;
    char*           output_file;

    CompileMode     mode;
    TargetArch      target;
    IntermediateRep inter_rep;
};

extern Cmdline args;

void process_cmdln_args(int argc, char* argv[]);
