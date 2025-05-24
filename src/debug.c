#ifdef SI_DEBUG

#include "debug.h"
#include "core.h"

static const char* tok_type_names[] = {
    "Identifier     ",
    "Separator      ",
    "Keyword        ",
    "String Literal ",
    "Numeric Literal",
    "End Of File    "
};

void print_token(const Token* tok)
{
    SIC_ASSERT(tok != NULL);
    // printf("Token \'%s\': Type - %s\n", tok->ref);
}

void print_all_tokens(const Token* tok)
{
    SIC_ASSERT(tok != NULL);
    for(size_t i = 0; tok != NULL; i++)
    {
        printf("Token %-3lu %s: %.*s\n", 
               i, 
               tok_type_names[tok->type],
               (int)tok->len, 
               tok->ref);
        tok = tok->next;
    }
}

#endif // SI_DEBUG
