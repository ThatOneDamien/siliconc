#pragma once
#include "core/internal.h"
#include "utils/da.h"

void gen_intermediate_rep(Object* program, const char* input_path, const char* out_path);
void assemble_intermediate(const char* input_path, const char* out_path);
