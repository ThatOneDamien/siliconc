#pragma once

#ifdef SI_DEBUG

#include "core/lexer.h"
#include "core/parser.h"

void print_all_tokens(const Token* tok);
void print_program(const Object* program);

#endif // SI_DEBUG
