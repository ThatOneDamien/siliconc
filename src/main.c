#include "core.h"
#include "debug.h"
#include "file_utils.h"
#include "lexer.h"

static void print_usage(void)
{
    printf("Usage: sic <filename>\n");
}

int main(int argc, char* argv[])
{
    (void)argv;
    if(argc < 2)
    {
        print_usage();
        return EXIT_FAILURE;
    }

    // This means we have a file to open
    Token* tokens = lex_file(argv[1]);
    print_all_tokens(tokens);

    return EXIT_SUCCESS;
}
