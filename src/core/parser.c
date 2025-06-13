#include "internal.h"
#include "utils/da.h"
#include "utils/error.h"
#include "utils/lib.h"

typedef ASTExpr* (*ExprPrefixFunc)(Lexer*);
typedef ASTExpr* (*ExprInfixFunc)(Lexer*, ASTExpr*);
typedef struct ExprParseRule ExprParseRule;
struct ExprParseRule
{
    ExprPrefixFunc prefix;
    ExprInfixFunc  infix;
    OpPrecedence  precedence;
};


// Global check and add functions
static bool      function_declaration(Lexer* l, ObjAccess access, Type* ret_type, ObjAttr attribs);
static ObjAccess parse_access(Lexer* l);

// Grammar parsing
static bool     parse_attribute(Lexer* l, ObjAttr* attribs);
static bool     parse_type_qualifier(Lexer* l, TypeQualifier* qual);
static void     parse_type_prefix(Lexer* l, Type** type, ObjAttr* attribs);
static bool     parse_func_params(Lexer* l, Object** params, size_t* count);
static ASTNode* parse_stmt_block(Lexer* l);
static ASTNode* parse_stmt(Lexer* l);
static ASTNode* parse_declaration(Lexer* l, Type* type, ObjAttr attribs);
static ASTExpr* parse_expr(Lexer* l, OpPrecedence precedence);
static ASTExpr* parse_binary(Lexer* l, ASTExpr* lhs);
static ASTExpr* parse_identifier_expr(Lexer* l);
static ASTExpr* parse_paren_expr(Lexer* l);
static ASTExpr* parse_unary_prefix(Lexer* l);
static ASTExpr* parse_int_literal(Lexer* l);

// Inline helpers

static inline Token* peek(Lexer* lexer)
{
    return lexer->la_buf.buf + lexer->la_buf.head;
}

static inline Token* peek_forw(Lexer* lexer, uint32_t count)
{
    SIC_ASSERT(count < LOOK_AHEAD_SIZE);
    return lexer->la_buf.buf + ((lexer->la_buf.head + count) % LOOK_AHEAD_SIZE);
}

static inline bool tok_equal(Lexer* lexer, TokenKind kind)
{
    return peek(lexer)->kind == kind;
}

static inline bool tok_equal_forw(Lexer* lexer, uint32_t count, TokenKind kind)
{
    return peek_forw(lexer, count)->kind == kind;
}

static inline void parser_error(Lexer* lexer, const char* restrict message, ...)
{
    va_list va;
    va_start(va, message);
    Token* t = peek(lexer);
    sic_error_atv(lexer->unit->file.full_path, t, message, va);
    va_end(va);
}

static inline bool UNUSED expect(Lexer* l, TokenKind kind)
{
    if(tok_equal(l, kind))
        return true;

    parser_error(l, "Expected \'%s\'", tok_kind_to_str(kind));
    return false;
}

static inline void advance(Lexer* l)
{
    if(!lexer_advance(l))
        return;
}

static inline void advance_many(Lexer* l, uint32_t steps)
{
    SIC_ASSERT(steps > 1);
    for(uint32_t i = 0; i < steps; ++i)
    {
        if(!lexer_advance(l))
            return;
    }
}

static inline bool try_consume(Lexer* l, TokenKind kind)
{
    if(tok_equal(l, kind))
    {
        advance(l);
        return true;
    }
    return false;
}

static inline bool consume(Lexer* l, TokenKind kind)
{
    if(try_consume(l, kind))
        return true;

    parser_error(l, "Expected \'%s\'.", tok_kind_to_str(kind));
    return false;
}

static inline ASTNode* new_node(Lexer* l, NodeKind kind)
{
    ASTNode* node = CALLOC_STRUCT(ASTNode);
    node->kind = kind;
    node->token = *peek(l);
    return node;
}

static ASTNode s_badnode = {0};
static ASTExpr s_badexpr = {0};
static ExprParseRule expr_rules[__TOKEN_COUNT];

#define ERROR_AND_RET(ret_val, l, ...)   do { parser_error(l, __VA_ARGS__); return ret_val; } while(0)
#define CONSUME_OR_RET(ret_val, l, kind) do { if(!consume(l, kind)) return ret_val; } while(0)
#define EXPECT_OR_RET(ret_val, l, kind)  do { if(!expect(l, kind)) return ret_val; } while(0)
#define BAD_NODE (&s_badnode)
#define BAD_EXPR (&s_badexpr)


void parser_init(void)
{
}

void parse_unit(CompilationUnit* unit)
{
    SIC_ASSERT(unit != NULL);
    Lexer l;
    lexer_init_unit(&l, unit);
    da_init(&unit->funcs, 0);
    da_init(&unit->vars, 0);

    while(lexer_advance(&l))
    {
        ObjAccess access = parse_access(&l);
        Type* type;
        ObjAttr attribs = ATTR_NONE;
        parse_type_prefix(&l, &type, &attribs);
        
        bool recovery_needed = type == NULL;

        if(type == NULL)
            parser_error(&l, "Missing type specifier.");
        else if(function_declaration(&l, access, type, attribs))
             continue;
        else
            recovery_needed = true;
        
        if(recovery_needed)
        {
            printf("recovering.\n");
            while(true)
            {
                Token* t = peek_forw(&l, 1);
                if(t->kind == TOKEN_EOF)
                {
                    advance(&l);
                    break;
                }
                if(t->loc == t->line_start &&
                    (t->kind == TOKEN_IDENT ||
                    (t->kind >= TOKEN_KEYWORD_START &&
                     t->kind <= TOKEN_KEYWORD_END)))
                    break;
                advance(&l);
            }
        }
    }
}

// function_declaration -> type_prefix identifier "(" func_params (";" | "{" stmt_block)
static bool function_declaration(Lexer* l, ObjAccess access, Type* ret_type, ObjAttr attribs)
{
    if(!tok_equal(l, TOKEN_IDENT)||
       !tok_equal_forw(l, 1, TOKEN_LPAREN))
        return false;

    Object* func = MALLOC_STRUCT(Object);
    ObjFunc* comps = &func->func;
    func->kind = OBJ_FUNC;
    comps->ret_type = ret_type;
    func->symbol = *peek(l);
    func->access = access;
    func->attribs = attribs;

    advance_many(l, 2);
    if(!parse_func_params(l, &comps->params, &comps->param_cnt))
        return false;

    if(try_consume(l, TOKEN_SEMI))
    {
        printf("Function declaration\n");
        return true;
    }

    ASTNode* body_block = parse_stmt_block(l);
    comps->body = body_block->stmt.block.body;

    da_append(&l->unit->funcs, func);
    return true;
}

static ObjAccess parse_access(Lexer* l)
{
    ObjAccess access = ACCESS_PROTECTED;
    switch(peek(l)->kind)
    {
    case TOKEN_PRIV:
        access = ACCESS_PRIVATE;
        break;
    case TOKEN_PROT:
        break;
    case TOKEN_PUB:
        access = ACCESS_PUBLIC;
        break;
    default:
        return access;
    }
    advance(l);
    return access;
}

static bool parse_attribute(Lexer* l, ObjAttr* attribs)
{
    ObjAttr temp;
    switch(peek(l)->kind)
    {
    case TOKEN_EXTERN:
        temp = ATTR_EXTERN;
        break;
    default:
        return false;
    }

    if(attribs == NULL)
        parser_error(l, "Object attributes not allowed in this context.");
    else if(*attribs & temp) {} // This would be where we warn for duplicate attribute
    else
        *attribs |= temp;

    advance(l);
    return true;
}

static bool parse_type_qualifier(Lexer* l, TypeQualifier* qual)
{
    TypeQualifier temp;
    switch(peek(l)->kind)
    {
    case TOKEN_CONST:
        temp = QUALIFIER_CONST;
        break;
    default:
        temp = QUALIFIER_NONE;
        break;
    }

    if(temp != QUALIFIER_NONE)
    {
        // This would be where we warn for duplicate type qualifier
        if(*qual & temp) {}
        *qual |= temp;
        advance(l);
        return true;
    }
    return false;

}

// type_qualifier -> "const" TODO: NOT IMPLEMENTED YET!!!
// type_name -> "void" | "u8" | "s8" | "u16" | "s16" |
//              "u32" | "s32" | "u64" | "s64" | "f32" | 
//              "f64" | identifier
// type_suffix -> "(" func_params type_qualifier* |
//                "[" array_dims | TODO: NOT IMPLEMENTED YET!!!
//                "*" type_qualifier* |
// type_prefix -> (type_qualifier | type_name)* type_suffix*
// NOTE: You are only allowed to specify one storage class and one type name
//       per type prefix. You can have multiple qualifiers, but if any one
//       qualifier is duplicated a warning will be raised.
static void parse_type_prefix(Lexer* l, Type** type, ObjAttr* attribs)
{
    Type* ty = NULL;
    TypeQualifier qual = QUALIFIER_NONE;

    while(true)
    {
        if(parse_attribute(l, attribs))
            continue;
        if(parse_type_qualifier(l, &qual))
            continue;
        Type* t = is_builtin_type(peek(l)->kind) ? builtin_type(peek(l)->kind) : NULL; // TODO: Get type
        if(t != NULL)
        {
            if(ty != NULL)
                parser_error(l, "Two or more data types in type prefix");
            ty = t;
            advance(l);
            continue;
        }

        break;
    }

    if(qual != QUALIFIER_NONE)
    {
        ty = type_copy(ty);
        ty->qualifiers = qual;
    }

    while(true)
    {

        if(tok_equal(l, TOKEN_LBRACKET))
            sic_error_fatal("Array types not implemented yet.");


        if(tok_equal(l, TOKEN_LPAREN))
        {
            sic_error_fatal("Function pointer types not implemented yet.");
            // Object* params;
            // size_t count;
            // if(!parse_func_params(l, &params, &count))
            //     return;
        }
        else if(tok_equal(l, TOKEN_ASTERISK))
        {
            ty = pointer_to(ty);
            advance(l);
        }
        else
            break;

        ty->qualifiers = QUALIFIER_NONE;
        while(parse_type_qualifier(l, &ty->qualifiers)) {}
    }

    *type = ty;
}

static bool parse_func_params(Lexer* l, Object** params, size_t* count)
{
    Object head;
    head.next = NULL;
    Object* cur_param = &head;
    size_t cnt = 0;

    while(!tok_equal(l, TOKEN_RPAREN))
    {
        if(tok_equal(l, TOKEN_EOF))
            ERROR_AND_RET(false, l, "No closing parentheses.");
        
        cnt++;
        if(cur_param != &head)
            consume(l, TOKEN_COMMA);


        Type* type;
        parse_type_prefix(l, &type, NULL);
        if(type == NULL)
            ERROR_AND_RET(false, l, "Missing type specifier.");

        EXPECT_OR_RET(false, l, TOKEN_IDENT);

        cur_param->next = MALLOC(sizeof(Object));
        cur_param = cur_param->next;
        cur_param->kind = OBJ_VAR;
        cur_param->next = NULL;
        cur_param->var.type = type;
        cur_param->symbol = *peek(l);

        advance(l);
    }
    
    advance(l);
    *params = head.next;
    *count = cnt;
    return true;
}

static ASTNode* parse_stmt_block(Lexer* l)
{
    CONSUME_OR_RET(BAD_NODE, l, TOKEN_LBRACE);
    ASTNode* block = new_node(l, NODE_BLOCK);
    ASTNode head;
    head.next = NULL;
    ASTNode* cur_expr = &head;

    while(!try_consume(l, TOKEN_RBRACE))
    {
        if(tok_equal(l, TOKEN_EOF))
            parser_error(l, "No closing }.");
        Type* type;
        ObjAttr attribs = ATTR_NONE;
        parse_type_prefix(l, &type, &attribs);

        if(type == NULL)
            cur_expr->next = parse_stmt(l);
        else
        {
            cur_expr->next = parse_declaration(l, type, attribs);
            if(cur_expr->next == NULL)
                continue;
        }
        cur_expr = cur_expr->next;
    }

    block->stmt.block.body = head.next;
    return block;
}

static ASTNode* parse_stmt(Lexer* l)
{
    if(tok_equal(l, TOKEN_SEMI))
    {
        ASTNode* node = new_node(l, NODE_EXPR_STMT);
        node->stmt.expr = MALLOC_STRUCT(ASTExpr);
        node->stmt.expr->kind = EXPR_NOP;
        advance(l);
        return node;
    }
    //
    // if(tok_equal(l, TOKEN_RETURN))
    // {
    //     ASTNode* new_node = create_node(NODE_RETURN, peek(l));
    //     Type* ret_type = s_curfunc->func.ret_type;
    //     advance(l);
    //     if(try_consume(l, TOKEN_SEMI))
    //     {
    //         if(ret_type != g_type_void)
    //             sic_error_fatal("Non-void function should return a value.");
    //         return new_node;
    //     }
    //
    //     ASTNode* ret_expr = parse_assignment(l);
    //     consume(l, TOKEN_SEMI);
    //
    //     new_node->children = ret_expr;
    //     return new_node;
    // }

    ASTNode* expr_stmt = new_node(l, NODE_EXPR_STMT);
    expr_stmt->stmt.expr = parse_expr(l, PREC_ASSIGN);
    if(expr_stmt->stmt.expr == NULL || expr_stmt->stmt.expr->kind == EXPR_INVALID)
    {
        advance(l);
        return BAD_NODE;
    }
    consume(l, TOKEN_SEMI);
    return expr_stmt;
}

static ASTNode* parse_declaration(Lexer* l, Type* type, ObjAttr attribs)
{
    (void)l;
    (void)attribs;
    ASTNode head;
    head.next = NULL;
    // ASTNode* cur = &head;

    if(type == g_type_void)
        sic_error_fatal("Variables cannot be declared as type void.");

    // while(expect(l, TOKEN_IDENT))
    // {
    //     Object* new_var = scope_def_obj(l, peek(l), storage);
    //     new_var->kind = OBJ_VAR;
    //     new_var->var.type = type;
    //     advance(l);
    //     if(tok_equal(l, TOKEN_ASSIGN))
    //     {
    //         ASTNode* assign = create_node(NODE_ASSIGN, peek(l));
    //         assign->children = create_node(NODE_VAR, &new_var->symbol);
    //         assign->children->var = new_var;
    //         advance(l);
    //         assign->children->next = parse_assignment(l);
    //         cur->next = assign;
    //         cur = cur->next;
    //     }
    //     if(try_consume(l, TOKEN_COMMA))
    //         continue;
    //     consume(l, TOKEN_SEMI);
    //     break;
    // }
    return head.next;
}

static ASTExpr* parse_expr(Lexer* l, OpPrecedence precedence)
{
    ExprPrefixFunc prefix = expr_rules[peek(l)->kind].prefix;
    if(prefix == NULL)
        ERROR_AND_RET(BAD_EXPR, l, "Expected an expression.");

    ASTExpr* left = prefix(l);
    while(true)
    {
        TokenKind kind = peek(l)->kind;
        if(expr_rules[kind].precedence < precedence)
            break;
        if(left == NULL || left->kind == EXPR_INVALID)
            return left;

        ExprInfixFunc infix = expr_rules[kind].infix;
        if(infix == NULL)
            ERROR_AND_RET(BAD_EXPR, l, "Left side of operator is invalid.");
        left = infix(l, left);
    }
    return left;
}

static ASTExpr* parse_binary(Lexer* l, ASTExpr* lhs)
{
    Token tok = *peek(l);
    advance(l);

    ASTExpr* rhs;
    OpPrecedence rhs_pref = expr_rules[tok.kind].precedence;
    if(rhs_pref != PREC_ASSIGN)
        rhs_pref++;

    rhs = parse_expr(l, rhs_pref);
    if(rhs == NULL || rhs->kind == EXPR_INVALID)
        return BAD_EXPR;

    ASTExpr* binary = MALLOC_STRUCT(ASTExpr);
    binary->kind = EXPR_BINARY;
    binary->token = tok;
    binary->expr.binary.lhs = lhs;
    binary->expr.binary.rhs = rhs;
    binary->expr.binary.kind = tok_to_binary_op(tok.kind);
    return binary;
}

static ASTExpr* parse_identifier_expr(Lexer* l)
{
    ASTExpr* expr = MALLOC_STRUCT(ASTExpr);
    expr->kind = EXPR_PRE_SEMANTIC_IDENT;
    expr->token = *peek(l);
    advance(l);
    return expr;
}

static ASTExpr* parse_paren_expr(Lexer* l)
{
    SIC_ASSERT(peek(l)->kind == TOKEN_LPAREN);
    advance(l);
    Type* cast_type;
    parse_type_prefix(l, &cast_type, NULL);
    if(cast_type != NULL)
    {
        ASTExpr* cast = MALLOC_STRUCT(ASTExpr);
        cast->kind = EXPR_CAST;
        cast->token = *peek(l);
        CONSUME_OR_RET(BAD_EXPR, l, TOKEN_RPAREN);
        cast->expr.cast.expr_to_cast = parse_expr(l, PREC_PRIMARY_POSTFIX);
        if(cast->expr.cast.expr_to_cast == NULL || cast->expr.cast.expr_to_cast->kind == EXPR_INVALID)
            return BAD_EXPR;
        return cast;
    }

    ASTExpr* inside_expr = parse_expr(l, PREC_ASSIGN);
    CONSUME_OR_RET(BAD_EXPR, l, TOKEN_RPAREN);
    return inside_expr;
}

static ASTExpr* parse_unary_prefix(Lexer* l)
{
    ASTExpr* expr = MALLOC_STRUCT(ASTExpr);
    expr->kind = EXPR_UNARY;
    expr->token = *peek(l);
    expr->expr.unary.child = parse_expr(l, PREC_UNARY_PREFIX);
    if(expr->expr.unary.child == NULL || expr->expr.unary.child->kind == EXPR_INVALID)
        return BAD_EXPR;
    return expr;
}

static ASTExpr* parse_int_literal(Lexer* l)
{
    ASTExpr* expr = MALLOC_STRUCT(ASTExpr);
    expr->kind = EXPR_CONSTANT;
    expr->token = *peek(l);
    advance(l);
    return expr;
}

static ExprParseRule UNUSED expr_rules[__TOKEN_COUNT] = {
    [TOKEN_IDENT]           = { parse_identifier_expr, NULL, PREC_NONE },
    [TOKEN_INT_LITERAL]     = { parse_int_literal, NULL, PREC_NONE },
    [TOKEN_CHAR_LITERAL]    = { NULL, NULL, PREC_NONE },
    [TOKEN_FLOAT_LITERAL]   = { NULL, NULL, PREC_NONE },
    [TOKEN_STRING_LITERAL]  = { NULL, NULL, PREC_NONE },
    [TOKEN_AMP]             = { parse_unary_prefix, parse_binary, PREC_BIT_AND },
    [TOKEN_ASTERISK]        = { parse_unary_prefix, parse_binary, PREC_MUL_DIV_MOD },
    [TOKEN_LOG_NOT]         = { parse_unary_prefix, NULL, PREC_UNARY_PREFIX },
    [TOKEN_BIT_NOT]         = { parse_unary_prefix, NULL, PREC_UNARY_PREFIX },
    [TOKEN_BIT_OR]          = { NULL, parse_binary, PREC_BIT_OR },
    [TOKEN_BIT_XOR]         = { NULL, parse_binary, PREC_BIT_XOR },
    [TOKEN_ASSIGN]          = { NULL, parse_binary, PREC_ASSIGN },
    [TOKEN_LT]              = { NULL, parse_binary, PREC_RELATIONAL },
    [TOKEN_GT]              = { NULL, parse_binary, PREC_RELATIONAL },
    [TOKEN_DIV]             = { NULL, parse_binary, PREC_MUL_DIV_MOD },
    // [TOKEN_PERIOD]          = { NULL, NULL, PREC_PRIMARY_POSTFIX },
    // [TOKEN_LBRACE]          = { NULL, NULL, PREC_NONE },
    // [TOKEN_LBRACKET]        = { NULL, NULL, PREC_NONE },
    [TOKEN_LPAREN]          = { parse_paren_expr, NULL, PREC_NONE },
    [TOKEN_ADD]             = { NULL, parse_binary, PREC_ADD_SUB },
    [TOKEN_SUB]             = { parse_unary_prefix, parse_binary, PREC_ADD_SUB },
    [TOKEN_MOD]             = { NULL, parse_binary, PREC_MUL_DIV_MOD },
    [TOKEN_QUESTION]        = { NULL, NULL, PREC_TERNARY },
    [TOKEN_SHR]             = { NULL, parse_binary, PREC_SHIFTS },
    [TOKEN_SHL]             = { NULL, parse_binary, PREC_SHIFTS },
    [TOKEN_LOG_AND]         = { NULL, parse_binary, PREC_LOG_AND },
    [TOKEN_LOG_OR]          = { NULL, parse_binary, PREC_LOG_OR },
    [TOKEN_EQ]              = { NULL, parse_binary, PREC_RELATIONAL },
    [TOKEN_NE]              = { NULL, parse_binary, PREC_RELATIONAL },
    [TOKEN_LE]              = { NULL, parse_binary, PREC_RELATIONAL },
    [TOKEN_GE]              = { NULL, parse_binary, PREC_RELATIONAL },
    [TOKEN_BIT_AND_ASSIGN]  = { NULL, parse_binary, PREC_ASSIGN },
    [TOKEN_BIT_OR_ASSIGN]   = { NULL, parse_binary, PREC_ASSIGN },
    [TOKEN_BIT_XOR_ASSIGN]  = { NULL, parse_binary, PREC_ASSIGN },
    [TOKEN_ADD_ASSIGN]      = { NULL, parse_binary, PREC_ASSIGN },
    [TOKEN_SUB_ASSIGN]      = { NULL, parse_binary, PREC_ASSIGN },
    [TOKEN_MUL_ASSIGN]      = { NULL, parse_binary, PREC_ASSIGN },
    [TOKEN_DIV_ASSIGN]      = { NULL, parse_binary, PREC_ASSIGN },
    [TOKEN_MOD_ASSIGN]      = { NULL, parse_binary, PREC_ASSIGN },
    [TOKEN_SHR_ASSIGN]      = { NULL, parse_binary, PREC_ASSIGN },
    [TOKEN_SHL_ASSIGN]      = { NULL, parse_binary, PREC_ASSIGN },
    [TOKEN_INCREM]          = { parse_unary_prefix, NULL, PREC_PRIMARY_POSTFIX },
    [TOKEN_DECREM]          = { parse_unary_prefix, NULL, PREC_PRIMARY_POSTFIX },
};
