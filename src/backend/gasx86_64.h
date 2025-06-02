#pragma once
#include "core/parser.h"
#include "utils/da.h"

void gasx86_64_codegen(Object* program, char* input_path, FILE* out_file);
void gasx86_64_assemble(char* input_path, char* out_path);
