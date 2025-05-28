#pragma once
#include "da.h"

typedef enum
{
    MODE_NONE = 0,
    MODE_PREPROCESS,
    MODE_COMPILE,
    MODE_ASSEMBLE,
    MODE_LINK
} CompileMode;

typedef struct Cmdline Cmdline;
struct Cmdline
{
    StringArray input_files;
    const char* output_file;
    CompileMode mode;
};

extern Cmdline args;

void process_cmdln_args(int argc, char* argv[]);
