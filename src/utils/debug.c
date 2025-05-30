#ifdef SI_DEBUG

#include "debug.h"
#include "core/core.h"

static const char* tok_type_names[] = {
    "NULL           ",
    "Identifier     ",
    "Separator      ",
    "Keyword        ",
    "String Literal ",
    "Numeric Literal",
    "End Of File    "
};

void print_all_tokens(const Token* tok)
{
    SIC_ASSERT(tok != NULL);
    for(size_t i = 0; tok != NULL; i++)
    {
        printf("Token %-3lu %s: Len: %-4lu   %.*s\n", 
               i, 
               tok_type_names[tok->type],
               tok->len,
               (int)tok->len, 
               tok->ref);
        tok = tok->next;
    }
}

#endif // SI_DEBUG
