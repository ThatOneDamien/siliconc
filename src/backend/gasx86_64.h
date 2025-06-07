#pragma once
#include "core/internal.h"
#include "utils/da.h"

void gasx86_64_codegen(Object* program, const char* input_path, FILE* out_file);
void gasx86_64_assemble(const char* input_path, const char* out_path);
