#pragma once
#include "core/parser.h"
#include "utils/da.h"

void gen_intermediate_rep(Object* program, char* input_path, char* out_path);
void assemble_intermediate(char* input_path, char* out_path);
