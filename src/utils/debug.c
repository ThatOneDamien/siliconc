#ifdef SI_DEBUG

#include "debug.h"
#include "core/core.h"
#include "core/internal.h"

#define PRINT_DEPTH(depth) do { for(int i = 0; i < (int)(depth); ++i) printf("  "); } while(0)

static const char* s_tok_names[] = {
    [TOKEN_INVALID]         = "Invalid",
    [TOKEN_IDENT]           = "Identifier",
    [TOKEN_INT_LITERAL]     = "Integer Literal",
    [TOKEN_CHAR_LITERAL]    = "Char Literal",
    [TOKEN_FLOAT_LITERAL]   = "Float Literal",
    [TOKEN_STRING_LITERAL]  = "String Literal",
    [TOKEN_AMP]             = "Ampersand",
    [TOKEN_ASTERISK]        = "Asterisk",
    [TOKEN_LOG_NOT]         = "Logical Not",
    [TOKEN_BIT_NOT]         = "Bitwise Not",
    [TOKEN_BIT_OR]          = "Bitwise Or",
    [TOKEN_BIT_XOR]         = "Bitwise Xor",
    [TOKEN_COLON]           = "Colon",
    [TOKEN_SEMI]            = "Semicolon",
    [TOKEN_ASSIGN]          = "Assign",
    [TOKEN_LT]              = "Less Than",
    [TOKEN_GT]              = "Greater Than",
    [TOKEN_DIV]             = "Divide",
    [TOKEN_PERIOD]          = "Period",
    [TOKEN_COMMA]           = "Comma",
    [TOKEN_LBRACE]          = "Left Brace",
    [TOKEN_LBRACKET]        = "Left Bracket",
    [TOKEN_LPAREN]          = "Left Paren",
    [TOKEN_RPAREN]          = "Right Paren",
    [TOKEN_RBRACKET]        = "Right Bracket",
    [TOKEN_RBRACE]          = "Right Brace",
    [TOKEN_ADD]             = "Add",
    [TOKEN_SUB]             = "Subtract",
    [TOKEN_MOD]             = "Modulo",
    [TOKEN_QUESTION]        = "Question Mark",
    [TOKEN_SHR]             = "Shift Right",
    [TOKEN_SHL]             = "Shift Left",
    [TOKEN_LOG_AND]         = "Logical And",
    [TOKEN_LOG_OR]          = "Logical Or",
    [TOKEN_EQ]              = "Equal",
    [TOKEN_NE]              = "Not Equal",
    [TOKEN_LE]              = "Less Than Or Equal",
    [TOKEN_GE]              = "Greater Than Or Equal",
    [TOKEN_BIT_AND_ASSIGN]  = "Bitwise And Assign",
    [TOKEN_BIT_OR_ASSIGN]   = "Bitwise Or Assign",
    [TOKEN_BIT_XOR_ASSIGN]  = "Bitwise Xor Assign",
    [TOKEN_ADD_ASSIGN]      = "Add Assign",
    [TOKEN_SUB_ASSIGN]      = "Subtract Assign",
    [TOKEN_MUL_ASSIGN]      = "Multiply Assign",
    [TOKEN_DIV_ASSIGN]      = "Divide Assign",
    [TOKEN_MOD_ASSIGN]      = "Modulo Assign",
    [TOKEN_SHR_ASSIGN]      = "Shift Right Assign",
    [TOKEN_SHL_ASSIGN]      = "Shift Left Assign",
    [TOKEN_INCREM]          = "Increment",
    [TOKEN_DECREM]          = "Decrement",
    [TOKEN_EXTERN]          = "extern",
    [TOKEN_PUB]             = "pub",
    [TOKEN_PRIV]            = "priv",
    [TOKEN_PROT]            = "prot",
    [TOKEN_RETURN]          = "return",
    [TOKEN_VOID]            = "void",
    [TOKEN_U8]              = "u8",
    [TOKEN_S8]              = "s8",
    [TOKEN_U16]             = "u16",
    [TOKEN_S16]             = "s16",
    [TOKEN_U32]             = "u32",
    [TOKEN_S32]             = "s32",
    [TOKEN_U64]             = "u64",
    [TOKEN_S64]             = "s64",
    [TOKEN_F32]             = "f32",
    [TOKEN_F64]             = "f64",
    [TOKEN_EOF]             = "End Of File",
};

static const char* s_type_strings[] = {
    [TYPE_INVALID]  = "Invalid", 
    [TYPE_VOID]     = "void", 
    [TYPE_U8]       = "u8", 
    [TYPE_S8]       = "s8", 
    [TYPE_U16]      = "u16", 
    [TYPE_S16]      = "s16",
    [TYPE_U32]      = "u32", 
    [TYPE_S32]      = "s32", 
    [TYPE_U64]      = "u64", 
    [TYPE_S64]      = "s64", 
    [TYPE_F32]      = "f32", 
    [TYPE_F64]      = "f64", 
    [TYPE_POINTER]  = "pointer",
};


static const char* s_node_type_names[] = {
    [NODE_INVALID]     = "Invalid",
    [NODE_BLOCK]       = "Block",
    [NODE_SINGLE_DECL] = "Single Declaration",
    [NODE_MULTI_DECL]  = "Multi Declaration",
    [NODE_EXPR_STMT]   = "Expression Statement",
    [NODE_RETURN]      = "Return Statement",
};

static const char* s_access_strs[] = {
    [ACCESS_PUBLIC]     = "public",
    [ACCESS_PROTECTED]  = "protected",
    [ACCESS_PRIVATE]    = "private",
};

void print_all_tokens(Lexer lexer)
{
    while(lexer_advance(&lexer))
    {
        Token* tok = lexer.la_buf.buf + lexer.la_buf.head;
        printf("%-15s: Len: %-4u   %.*s\n", 
               s_tok_names[tok->kind],
               tok->loc.len,
               (int)tok->loc.len, 
               tok->loc.start);
    }
}

static void print_expr(const ASTExpr* expr, int depth)
{
    if(expr == NULL)
        return;
    PRINT_DEPTH(depth);
    switch(expr->kind)
    {
    case EXPR_BINARY:
        printf("[ BINARY \'%.*s\' ]\n", expr->loc.len, expr->loc.start);
        print_expr(expr->expr.binary.lhs, depth + 1);
        print_expr(expr->expr.binary.rhs, depth + 1);
        break;
    case EXPR_CAST:
        printf("[ Cast ]\n");
        print_expr(expr->expr.cast.expr_to_cast, depth + 1);
        break;
    case EXPR_CONSTANT:
        printf("[ Constant \'%.*s\' val: %lu]\n", expr->loc.len, expr->loc.start, expr->expr.constant.val.i);
        break;
    case EXPR_FUNC_CALL:
        printf("[ Call ]\n");
        print_expr(expr->expr.call.func_expr, depth + 1);
        for(size_t i = 0; i < expr->expr.call.args.size; ++i)
            print_expr(expr->expr.call.args.data[i], depth + 1);
        break;
    case EXPR_INVALID:
        printf("[ Invalid ]\n");
        break;
    case EXPR_NOP:
        printf("[ Nop ]\n");
        break;
    case EXPR_PRE_SEMANTIC_IDENT: {
        const SourceLoc* loc = &expr->loc;
        printf("[ Identifier \'%.*s\' ]\n", loc->len, loc->start);
        break;
    }
    case EXPR_UNARY:
        printf("[ Unary \'%.*s\' ]\n", expr->loc.len, expr->loc.start);
        print_expr(expr->expr.unary.child, depth + 1);
        break;
    default:
        printf("[ %.*s ]\n", expr->loc.len, expr->loc.start);
        break;
    }
}

static void print_node(const ASTNode* node, int depth)
{
    if(node == NULL)
        return;

    PRINT_DEPTH(depth);
    printf("( %s )\n", s_node_type_names[node->kind]);
    switch(node->kind)
    {
    case NODE_BLOCK: {
        ASTNode* cur = node->stmt.block.body;
        while(cur != NULL)
        {
            print_node(cur, depth + 1);
            cur = cur->next;
        }
        break;
    }
    case NODE_EXPR_STMT:
        print_expr(node->stmt.expr, depth + 1);
        break;
    case NODE_RETURN:
        print_expr(node->stmt.return_.ret_expr, depth + 1);
        break;
    case NODE_SINGLE_DECL:
        print_expr(node->stmt.single_decl.init_expr, depth + 1);
        break;
    case NODE_MULTI_DECL:
        for(size_t i = 0; i < node->stmt.multi_decl.size; ++i)
            print_expr(node->stmt.multi_decl.data[i].init_expr, depth + 1);
        break;
    default:
        break;
    }

}

static void print_func(const Object* func)
{
    const ObjFunc* comps = &func->func;
    printf("Function \'%.*s\' %s (returns %s):\n", 
           (int)func->symbol.len,
           func->symbol.start,
           s_access_strs[func->access],
           s_type_strings[comps->ret_type->kind]);
    printf("  Params (count: %lu):\n", comps->params.size);
    for(size_t i = 0; i < comps->params.size; ++i)
    {
        Object* param = comps->params.data[i];
        printf("    %.*s (type %s)\n", 
               (int)param->symbol.len, 
               param->symbol.start,
               s_type_strings[param->var.type->kind]);
    }

    printf("  Local Variables:\n");
    for(size_t i = 0; i < comps->local_objs.size; ++i)
    {
        Object* local = comps->local_objs.data[i];
        printf("    %.*s (type %s)\n", 
               (int)local->symbol.len, 
               local->symbol.start,
               s_type_strings[local->var.type->kind]);
    }

    printf("  Body:\n");
    ASTNode* cur_node = comps->body;
    while(cur_node != NULL)
    {
        print_node(cur_node, 2);
        cur_node = cur_node->next;
    }
    printf("\n");
}

void print_unit(const CompilationUnit* unit)
{
    SIC_ASSERT(unit != NULL);
    printf("Compilation Unit: \'%s\' (%zu Funcs, %zu Global Vars)\n", unit->file.full_path, unit->funcs.size, unit->vars.size);
    for(size_t i = 0; i < unit->funcs.size; ++i)
        print_func(unit->funcs.data[i]);
}

#endif // SI_DEBUG
