#pragma once
#include "core/internal.h"
#include "utils/da.h"
#include "utils/file_utils.h"

void gen_intermediate_rep(const CompilationUnit* unit, const SIFile* output);
void assemble_intermediate(const char* input_path, const char* out_path);
