#pragma once

#ifdef SI_DEBUG

#include "core/internal.h"

void print_all_tokens(Lexer lexer);
void print_unit(const CompilationUnit* unit);

#endif // SI_DEBUG
