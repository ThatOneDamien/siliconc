#pragma once
#include "core/internal.h"
#include "utils/da.h"
#include "utils/file_utils.h"

void gasx86_64_codegen(const CompilationUnit* unit, FILE* out_file);
void gasx86_64_assemble(const char* input_path, const char* out_path);

void llvm_codegen(const CompilationUnit* unit);
