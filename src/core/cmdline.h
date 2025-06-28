#pragma once
#include "internal.h"
#include "utils/da.h"
#include "utils/file_utils.h"

typedef struct Cmdline  Cmdline;

struct Cmdline
{
    SIFileDA        input_files;
    char*           output_file;

    CompileMode     mode;
    CompileTarget   target;
    IRTarget        ir_kind;

    bool            emit_ir;
    bool            emit_asm;
    bool            hash_hash_hash;
};

extern Cmdline g_args;

void process_cmdln_args(int argc, char* argv[]);
