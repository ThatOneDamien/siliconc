#include "cmdline.h"
#include "core.h"
#include "lexer.h"
#include "parser.h"
#include "backend/codegen.h"
#include "utils/debug.h"
#include "utils/error.h"
#include "utils/file_utils.h"
#include "utils/hash.h"

int main(int argc, char* argv[])
{
    hashmap_test();
    return 0;
    if(argc < 1)
        sic_error_fatal("Bad arguments.");
    
    process_cmdln_args(argc, argv);
    
    for(size_t i = 0; i < args.input_files.size; ++i)
        if(!file_exists(args.input_files.data[i]))
            sic_error_fatal("File named '%s' not found.", args.input_files.data[i]);


    for(size_t i = 0; i < args.input_files.size; ++i)
    {
        FileType ft = get_filetype(args.input_files.data[i]);
        (void)ft;
        Token* tokens = lex_file(args.input_files.data[i]);
        print_all_tokens(tokens);

        if(args.mode < MODE_COMPILE)
            continue;
        Object* program = parse_tokens(tokens);
        StringBuilder intermediate = gen_intermediate_rep(program);
        sb_append_null(&intermediate);
        printf("%s\n", intermediate.data);
    }

    return EXIT_SUCCESS;
}
