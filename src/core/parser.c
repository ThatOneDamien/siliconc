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
static bool      parse_top_level(Lexer* l);
static bool      function_declaration(Lexer* l, ObjAccess access, Type* ret_type, ObjAttr attribs);
static ObjAccess parse_access(Lexer* l);

// Grammar parsing
static bool     parse_attribute(Lexer* l, ObjAttr* attribs);
static bool     parse_type_qualifier(Lexer* l, TypeQualifier* qual);
static void     parse_type_prefix(Lexer* l, Type** type, ObjAttr* attribs);
static bool     parse_func_params(Lexer* l, ObjectDA* params);
static ASTNode* parse_stmt_block(Lexer* l);
static ASTNode* parse_stmt(Lexer* l);
static ASTNode* parse_declaration(Lexer* l, Type* type, ObjAttr attribs);
static ASTExpr* parse_expr(Lexer* l, OpPrecedence precedence);
static ASTExpr* parse_binary(Lexer* l, ASTExpr* lhs);
static ASTExpr* parse_identifier_expr(Lexer* l);
static ASTExpr* parse_call(Lexer* l, ASTExpr* func_expr);
static ASTExpr* parse_cast(Lexer* l, ASTExpr* expr_to_cast);
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
    sic_error_atv(lexer->unit->file.full_path, &t->loc, message, va);
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

static inline ASTExpr* new_expr(Lexer* l, ExprKind kind)
{
    ASTExpr* expr = CALLOC_STRUCT(ASTExpr);
    expr->kind = kind;
    expr->loc = peek(l)->loc;
    return expr;
}

static inline void recover_to(Lexer* l, const TokenKind stopping_kinds[], size_t count)
{
    while(true)
    {
        TokenKind kind = peek(l)->kind;
        SIC_ASSERT(kind != TOKEN_INVALID);
        if(kind == TOKEN_EOF)
            return;
        for(size_t i = 0; i < count; ++i)
            if(kind == stopping_kinds[i])
                return;
        advance(l);
    }
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
    if(try_consume(&l, TOKEN_MODULE))
    {
        printf("here\n");
    }
    da_append(&g_compiler.top_module.units, unit);

    while(!tok_equal(&l, TOKEN_EOF))
    {
        if(!parse_top_level(&l))
        {
            while(true)
            {
                Token* t = peek(&l);
                if(t->kind == TOKEN_EOF)
                {
                    advance(&l);
                    break;
                }
                if(t->loc.start == t->loc.line_start &&
                    (t->kind == TOKEN_IDENT ||
                    (t->kind >= TOKEN_KEYWORD_START &&
                     t->kind <= TOKEN_KEYWORD_END)))
                    break;
                advance(&l);
            }
        }
    }
}

static bool parse_top_level(Lexer* l)
{
    switch(peek(l)->kind)
    {
    case TOKEN_MODULE:
        SIC_TODO();
    case TOKEN_PRIV:
    case TOKEN_PROT:
    case TOKEN_PUB:
    default: {
        ObjAccess access = parse_access(l);
        Type* type;
        ObjAttr attribs = ATTR_NONE;
        parse_type_prefix(l, &type, &attribs);

        if(type == NULL)
        {
            parser_error(l, "Missing type specifier.");
            return false;
        }

        return function_declaration(l, access, type, attribs);
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
    func->symbol = peek(l)->loc;
    func->access = access;
    func->attribs = attribs;
    da_init(&func->func.params, 8);

    advance_many(l, 2);
    if(!parse_func_params(l, &comps->params))
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

// type_qualifier -> "const"
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

static bool parse_func_params(Lexer* l, ObjectDA* params)
{
    while(!tok_equal(l, TOKEN_RPAREN))
    {
        if(tok_equal(l, TOKEN_EOF))
            ERROR_AND_RET(false, l, "No closing parentheses.");
        
        if(params->size > 0)
            consume(l, TOKEN_COMMA);


        Type* type;
        parse_type_prefix(l, &type, NULL);
        if(type == NULL)
            ERROR_AND_RET(false, l, "Missing type specifier.");

        EXPECT_OR_RET(false, l, TOKEN_IDENT);

        da_resize(params, params->size + 1);
        params->data[params->size - 1] = CALLOC_STRUCT(Object);
        params->data[params->size - 1]->kind = OBJ_VAR;
        params->data[params->size - 1]->var.type = type;
        params->data[params->size - 1]->symbol = peek(l)->loc;

        advance(l);
    }
    
    advance(l);
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
            ERROR_AND_RET(BAD_NODE, l, "No closing }.");
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

static const TokenKind s_stmt_recover_list[] = { TOKEN_SEMI, TOKEN_RBRACE };

static ASTNode* parse_stmt(Lexer* l)
{
    ASTNode* node;
    bool recover = false;
    switch(peek(l)->kind)
    {
    case TOKEN_LBRACE:
        node = parse_stmt_block(l);
        return node;
    case TOKEN_RETURN:
        node = new_node(l, NODE_RETURN);
        advance(l);
        if(!tok_equal(l, TOKEN_SEMI))
        {
            node->stmt.return_.ret_expr = parse_expr(l, PREC_ASSIGN);
            recover = node->stmt.return_.ret_expr->kind == EXPR_INVALID || 
                      !consume(l, TOKEN_SEMI);
        }
        break;
    case TOKEN_SEMI:
        node = new_node(l, NODE_EXPR_STMT);
        node->stmt.expr = new_expr(l, EXPR_NOP);
        advance(l);
        return node;
    default:
        node = new_node(l, NODE_EXPR_STMT);
        node->stmt.expr = parse_expr(l, PREC_ASSIGN);
        recover = node->stmt.expr->kind == EXPR_INVALID || 
                  !consume(l, TOKEN_SEMI);
        break;
    }

    if(recover)
    {
        recover_to(l, s_stmt_recover_list, sizeof(s_stmt_recover_list) / sizeof(s_stmt_recover_list[0]));
        return BAD_NODE;
    }

    return node;
}

static ASTNode* parse_declaration(Lexer* l, Type* type, ObjAttr attribs)
{
    ASTNode* decl_node = new_node(l, NODE_SINGLE_DECL);
    Object* var = CALLOC_STRUCT(Object);
    var->kind = OBJ_VAR;
    var->attribs = attribs;
    var->var.type = type;
    var->symbol = peek(l)->loc;
    decl_node->stmt.single_decl.obj = var;
    ASTExpr* expr = NULL;
    advance(l);
    if(try_consume(l, TOKEN_ASSIGN))
    {
        expr = parse_expr(l, PREC_ASSIGN);
        if(expr->kind == EXPR_INVALID)
            goto ERR;
        decl_node->stmt.single_decl.init_expr = expr;
    }
    if(try_consume(l, TOKEN_SEMI))
        return decl_node;

    decl_node->kind = NODE_MULTI_DECL;
    ASTDeclDA* decl_list = &decl_node->stmt.multi_decl;
    da_init(decl_list, 8);
    decl_list->data[0].obj       = var;
    decl_list->data[0].init_expr = expr;
    decl_list->size = 1;

    while(try_consume(l, TOKEN_COMMA))
    {
        da_resize(decl_list, decl_list->size + 1);
        var = CALLOC_STRUCT(Object);
        var->kind = OBJ_VAR;
        var->attribs = attribs;
        var->var.type = type;
        var->symbol = peek(l)->loc;
        advance(l);
        if(try_consume(l, TOKEN_ASSIGN))
        {
            expr = parse_expr(l, PREC_ASSIGN);
            if(expr->kind == EXPR_INVALID)
                goto ERR;
            decl_list->data[decl_list->size - 1].init_expr = expr;
        }
        decl_list->data[decl_list->size - 1].obj = var;
    }

    if(consume(l, TOKEN_SEMI))
        return decl_node;
ERR:
    recover_to(l, s_stmt_recover_list, sizeof(s_stmt_recover_list) / sizeof(s_stmt_recover_list[0]));
    return BAD_NODE;
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
        if(left->kind == EXPR_INVALID)
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
    ASTExpr* binary = new_expr(l, EXPR_BINARY);
    Token tok = *peek(l);
    advance(l);

    ASTExpr* rhs;
    OpPrecedence rhs_pref = expr_rules[tok.kind].precedence;
    if(rhs_pref != PREC_ASSIGN)
        rhs_pref++;

    rhs = parse_expr(l, rhs_pref);
    if(rhs->kind == EXPR_INVALID)
        return BAD_EXPR;

    binary->expr.binary.lhs = lhs;
    binary->expr.binary.rhs = rhs;
    binary->expr.binary.kind = tok_to_binary_op(tok.kind);
    return binary;
}

static ASTExpr* parse_identifier_expr(Lexer* l)
{
    ASTExpr* expr = new_expr(l, EXPR_PRE_SEMANTIC_IDENT);
    advance(l);
    return expr;
}

static ASTExpr* parse_call(Lexer* l, ASTExpr* func_expr)
{
    ASTExpr* call = new_expr(l, EXPR_FUNC_CALL);
    advance(l);
    call->expr.call.func_expr = func_expr;
    if(try_consume(l, TOKEN_RPAREN)) // void return
        return call;
    
    ASTExprDA* args = &call->expr.call.args;
    da_init(args, 8);
    da_append(args, parse_expr(l, PREC_ASSIGN));
    if(args->data[0]->kind == EXPR_INVALID)
        return BAD_EXPR;

    while(!try_consume(l, TOKEN_RPAREN))
    {
        CONSUME_OR_RET(BAD_EXPR, l, TOKEN_COMMA);

        da_append(args, parse_expr(l, PREC_ASSIGN));
        if(args->data[args->size - 1]->kind == EXPR_INVALID)
            return BAD_EXPR;
    }

    return call;
}

static ASTExpr* parse_cast(Lexer* l, ASTExpr* expr_to_cast)
{
    ASTExpr* cast = new_expr(l, EXPR_CAST);
    advance(l);
    
    Type* ty;
    parse_type_prefix(l, &ty, NULL);
    if(ty == NULL)
        ERROR_AND_RET(BAD_EXPR, l, "Expected a type after keyword \'as\'.");
    cast->expr.cast.expr_to_cast = expr_to_cast;
    cast->expr.cast.cast_type = ty;

    return cast;
}

static ASTExpr* parse_paren_expr(Lexer* l)
{
    SIC_ASSERT(peek(l)->kind == TOKEN_LPAREN);
    advance(l);
    ASTExpr* inside_expr = parse_expr(l, PREC_ASSIGN);
    CONSUME_OR_RET(BAD_EXPR, l, TOKEN_RPAREN);
    return inside_expr;
}

static ASTExpr* parse_unary_prefix(Lexer* l)
{
    ASTExpr* expr = new_expr(l, EXPR_UNARY);
    TokenKind kind = peek(l)->kind;
    advance(l);
    expr->expr.unary.child = parse_expr(l, PREC_UNARY_PREFIX);
    if(expr->expr.unary.child->kind == EXPR_INVALID)
        return BAD_EXPR;
    expr->expr.unary.kind = tok_to_unary_op(kind);
    return expr;
}

static ASTExpr* parse_int_literal(Lexer* l)
{
    ASTExpr* expr = new_expr(l, EXPR_CONSTANT);
    expr->expr.constant.kind = CONSTANT_INTEGER;

    // TODO: Deal with the prefix for hex, octal, binary

    uint64_t val = 0;
    for(uint32_t i = 0; i < expr->loc.len; ++i)
    {
        uint64_t prev = val;
        val *= 10;
        val += expr->loc.start[i] - '0';
        if(prev > val)
        {
            parser_error(l, "Integer value exceeds maximum possible 64 bit value.");
            advance(l);
            return BAD_EXPR;
        }
    }

    expr->expr.constant.val.i = val;
    expr->type = val > 0xFFFFFFFF ? g_type_u64 : g_type_u32;

    // TODO: Deal with the suffix.
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
    [TOKEN_LPAREN]          = { parse_paren_expr, parse_call, PREC_PRIMARY_POSTFIX },
    [TOKEN_ADD]             = { NULL, parse_binary, PREC_ADD_SUB },
    [TOKEN_SUB]             = { parse_unary_prefix, parse_binary, PREC_ADD_SUB },
    [TOKEN_MODULO]          = { NULL, parse_binary, PREC_MUL_DIV_MOD },
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
    [TOKEN_AS]              = { NULL, parse_cast, PREC_UNARY_PREFIX },
};
