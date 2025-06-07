#pragma once

#include "internal.h"
#include "utils/error.h"
#include "utils/lib.h"

// Global check and add functions
bool     function_definition(Lexer* l, Type* ret_type, ObjAttr* func_attr);

// Grammar parsing
void     parse_type_prefix(Lexer* l, Type** type, ObjAttr* attr);
void     parse_func_params(Lexer* l, Object** params, size_t* count);
ASTNode* parse_stmt_block(Lexer* l);
ASTNode* parse_stmt(Lexer* l);
ASTNode* parse_declaration(Lexer* l);
ASTNode* parse_assignment(Lexer* l);
ASTNode* parse_ternary(Lexer* l);
ASTNode* parse_logical_or(Lexer* l);
ASTNode* parse_logical_and(Lexer* l);
ASTNode* parse_bitwise_or(Lexer* l);
ASTNode* parse_bitwise_xor(Lexer* l);
ASTNode* parse_bitwise_and(Lexer* l);
ASTNode* parse_logical_equality(Lexer* l);
ASTNode* parse_relational(Lexer* l);
ASTNode* parse_bitwise_shift(Lexer* l);
ASTNode* parse_add_and_sub(Lexer* l);
ASTNode* parse_mul_div_and_mod(Lexer* l);
ASTNode* parse_cast(Lexer* l);
ASTNode* parse_unary(Lexer* l);
ASTNode* parse_postfix(Lexer* l);
ASTNode* parse_func_call(Lexer* l);
ASTNode* parse_primary_expr(Lexer* l);

// Scope and symbol defining/finding functions
void     enter_scope(void);
void     exit_scope(void);
Object*  create_obj_in_scope(Token* symbol, bool global);
Object*  get_var(Token* symbol);

// Inline helpers
static inline Token* tok_forw(Lexer* lexer, uint32_t count)
{
    SIC_ASSERT(count < LOOK_AHEAD_SIZE);
    return lexer->la_buf.buf + ((lexer->la_buf.head + count) % LOOK_AHEAD_SIZE);
}

// static inline bool tok_equal(Lexer* lexer, TokenKind kind)
// {
//     return lexer->tok.kind == kind;
// }

// static inline bool tok_expect(Lexer* lexer, TokenKind kind)
// {
//     if(tok_equal(lexer, kind))
//         return true;
//
//     sic_error_in_src(lexer->file_name, lexer->cur_line, 
//                      lexer->line_start, lexer->cur_pos, 
//                      "Expected \'%s\'", tok_kind_to_str(kind));
//     return false;
// }
//
// static inline TokenKind peek_kind(Lexer* lexer)
// {
//     return lexer->
// }

static inline bool tok_is_keyword(TokenKind kind)
{
    return kind >= TOKEN_KEYWORD_START && kind <= TOKEN_KEYWORD_END;
}

static inline bool tok_is_builtin_type(TokenKind kind)
{
    return kind >= TOKEN_TYPENAME_START && kind <= TOKEN_TYPENAME_END;
}
