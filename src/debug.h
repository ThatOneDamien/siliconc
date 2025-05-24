#pragma once

#ifdef SI_DEBUG

#include "lexer.h"
void print_token(const Token* tok);
void print_all_tokens(const Token* tok);

#endif // SI_DEBUG
