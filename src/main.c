#include "core.h"
#include "debug.h"
#include "file_utils.h"
#include "lexer.h"

int main(int argc, char* argv[])
{
    if(argc < 2)
    {
        fprintf(stderr, "sic: \033[31mfatal error\033[0m");
        return EXIT_FAILURE;
    }

    // This means we have a file to open
    Token* tokens = lex_file(argv[1]);
    print_all_tokens(tokens);

    return EXIT_SUCCESS;
}
