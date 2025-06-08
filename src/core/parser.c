#include "internal.h"
#include "utils/error.h"
#include "utils/lib.h"

// Global check and add functions
static bool     function_definition(Lexer* l, Type* ret_type, ObjAttr* func_attr);

// Grammar parsing
static void     parse_type_prefix(Lexer* l, Type** type, ObjAttr* attr);
static bool     parse_func_params(Lexer* l, Object** params, size_t* count);
static ASTNode* parse_stmt_block(Lexer* l);
static ASTNode* parse_stmt(Lexer* l);
static ASTNode* parse_declaration(Lexer* l);
static ASTNode* parse_assignment(Lexer* l);
static ASTNode* parse_ternary(Lexer* l);
static ASTNode* parse_logical_or(Lexer* l);
static ASTNode* parse_logical_and(Lexer* l);
static ASTNode* parse_bitwise_or(Lexer* l);
static ASTNode* parse_bitwise_xor(Lexer* l);
static ASTNode* parse_bitwise_and(Lexer* l);
static ASTNode* parse_logical_equality(Lexer* l);
static ASTNode* parse_relational(Lexer* l);
static ASTNode* parse_bitwise_shift(Lexer* l);
static ASTNode* parse_add_and_sub(Lexer* l);
static ASTNode* parse_mul_div_and_mod(Lexer* l);
static ASTNode* parse_cast(Lexer* l);
static ASTNode* parse_unary(Lexer* l);
static ASTNode* parse_postfix(Lexer* l);
static ASTNode* parse_func_call(Lexer* l);
static ASTNode* parse_primary_expr(Lexer* l);

// Scope and symbol defining/finding functions
static void     enter_scope(void);
static void     exit_scope(void);
static Object*  create_obj_in_scope(Token* symbol, bool global);
static Object*  get_var(Token* symbol);
static ASTNode* create_node(NodeKind kind, Token* token);

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
    sic_error_atv(lexer->file_name, t, message, va);
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

#define ERROR_AND_RET(ret_val, l, ...)   do { parser_error(l, __VA_ARGS__); return ret_val; } while(0)
#define CONSUME_OR_RET(ret_val, l, kind) do { if(!consume(l, kind)) return ret_val; } while(0)
#define EXPECT_OR_RET(ret_val, l, kind)  do { if(!expect(l, kind)) return ret_val; } while(0)

static Object* s_globals;
static Object* s_locals;
static Scope*  s_curscope;
static Scope   s_file_scope;
static Object* s_curfunc;

void parser_init(void)
{
    s_globals   = NULL;
    s_locals    = NULL;
    s_curscope = NULL;
    s_curfunc  = NULL;
    hashmap_init(&s_file_scope.vars);
}

Object* parse_unit(Lexer* lexer)
{
    SIC_ASSERT(lexer != NULL);
    s_globals = NULL;
    s_locals = NULL;
    s_file_scope.parent = NULL;
    s_curscope = &s_file_scope;
    hashmap_clear(&s_curscope->vars);

    while(lexer_advance(lexer))
    {
        Type* type;
        ObjAttr attr;
        parse_type_prefix(lexer, &type, &attr);
        
        bool recovery_needed = type == NULL;

        if(type == NULL)
            parser_error(lexer, "Missing type specifier.");
        else if(function_definition(lexer, type, &attr))
             continue;
        else
            recovery_needed = true;
        
        if(recovery_needed)
        {
            while(true)
            {
                Token* t = peek_forw(lexer, 1);
                if(t->loc == t->line_start &&
                   (t->kind == TOKEN_EOF ||
                    t->kind == TOKEN_IDENT ||
                    (t->kind >= TOKEN_KEYWORD_START &&
                     t->kind <= TOKEN_KEYWORD_END)))
                    break;
                advance(lexer);
            }
        }
    }
    return s_globals;
}

static bool function_definition(Lexer* l, Type* ret_type, ObjAttr* func_attr)
{
    (void)func_attr;
    if(!tok_equal(l, TOKEN_IDENT)||
       !tok_equal_forw(l, 1, TOKEN_LPAREN))
        return false;


    Object* func = create_obj_in_scope(peek(l), true);
    FuncComps* func_comps = &func->comps.func;
    func->is_function = true;
    func_comps->ret_type = ret_type;
    func->symbol = *peek(l);

    advance_many(l, 2);
    if(!parse_func_params(l, &func_comps->params, &func_comps->param_cnt))
        return false;

    s_curfunc = func;
    s_locals = NULL;
    enter_scope();

    s_locals = func_comps->params;
    for(size_t i = 0; i < func_comps->param_cnt; ++i)
    {
        if(s_locals->symbol.loc)
            hashmap_putn(&s_curscope->vars, s_locals->symbol.loc, s_locals->symbol.len, s_locals);
        s_locals = s_locals->next;
    }
    s_locals = func_comps->params;

    consume(l, TOKEN_LBRACE);

    ASTNode* body_block = parse_stmt_block(l);
    func_comps->body = body_block->children;
    free(body_block);

    func_comps->local_objs = s_locals;
    exit_scope();

    return true;
}

static void parse_type_prefix(Lexer* l, Type** type, ObjAttr* attr)
{
    bool seen_type = false;
    Type* ty = NULL;

    // TODO: Make the kw map hold values depending on whether the kw is a typename or not to
    //       make this function better. Furthermore it could be used to remove the string comparisons
    //       making the process quicker.
    while(true)
    {
        if(tok_equal(l, TOKEN_EXTERN))
        {
            *attr |= OBJ_ATTR_EXTERN;
            advance(l);
            continue;
        }

        bool prev = seen_type;

        switch(peek(l)->kind)
        {
        case TOKEN_U8:
            ty = g_type_u8;
            break;
        case TOKEN_S8:
            ty = g_type_u8;
            break;
        case TOKEN_U16:
            ty = g_type_u16;
            break;
        case TOKEN_S16:
            ty = g_type_s16;
            break;
        case TOKEN_U32:
            ty = g_type_u32;
            break;
        case TOKEN_S32:
            ty = g_type_s32;
            break;
        case TOKEN_U64:
            ty = g_type_u64;
            break;
        case TOKEN_S64:
            ty = g_type_s64;
            break;
        case TOKEN_F32:
            ty = g_type_f32;
            break;
        case TOKEN_F64:
            ty = g_type_f64;
            break;
        default:
            *type = ty;
            return;
        }
        seen_type = true;
        if(prev)
            parser_error(l, "Two or more data types in type prefix");
        advance(l);
    }
    

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

        cur_param->next = malloc(sizeof(Object));
        cur_param = cur_param->next;
        cur_param->is_function = false;
        cur_param->next = NULL;
        cur_param->comps.var.type = type;
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
    ASTNode* block = malloc(sizeof(ASTNode));
    block->kind = NODE_BLOCK;
    block->token = *peek(l);
    
    ASTNode head;
    head.next = NULL;
    ASTNode* cur_expr = &head;

    while(!tok_equal(l, TOKEN_RBRACE))
    {
        if(tok_equal(l, TOKEN_EOF))
            parser_error(l, "No closing }.");
        Type* type;
        ObjAttr attr;
        parse_type_prefix(l, &type, &attr);

        if(type == NULL)
        {
            cur_expr->next = parse_stmt(l);
            cur_expr = cur_expr->next;
        }
        else
            advance(l);
    }

    block->children = head.next;
    advance(l);
    return block;
}

static ASTNode* parse_stmt(Lexer* l)
{
    if(tok_equal(l, TOKEN_SEMI))
    {
        ASTNode* new_node = create_node(NODE_NOP, peek(l));
        advance(l);
        new_node->next = NULL;
        new_node->children = NULL;
        return new_node;
    }

    if(tok_equal(l, TOKEN_RETURN))
    {
        ASTNode* new_node = create_node(NODE_RETURN, peek(l));
        Type* ret_type = s_curfunc->comps.func.ret_type;
        advance(l);
        if(try_consume(l, TOKEN_SEMI))
        {
            if(ret_type != g_type_void)
                sic_error_fatal("Non-void function should return a value.");
            return new_node;
        }

        ASTNode* ret_expr = parse_assignment(l);
        consume(l, TOKEN_SEMI);

        new_node->children = ret_expr;
        return new_node;
    }

    ASTNode* res = parse_assignment(l);
    consume(l, TOKEN_SEMI);
    return res;
}

static UNUSED ASTNode* parse_declaration(Lexer* l)
{
    ASTNode head;
    head.next = NULL;
    // ASTNode* cur = &head;

    Type* type;
    ObjAttr attr;
    parse_type_prefix(l, &type, &attr);
    if(type == g_type_void)
        sic_error_fatal("Variables cannot be declared as type void.");

    if(tok_equal(l, TOKEN_IDENT))
    {
        Object* new_var = create_obj_in_scope(peek(l), false);
        new_var->is_function = false;
        new_var->comps.var.type = type;
        if(tok_equal(l, TOKEN_ASSIGN))
        {
            // ASTNode* var_node = create_node(NODE_VAR, new_var->symbol);
            // ASTNode* assign = create_node(NODE_ASSIGN, peek(l));
            // advance(l);
            // ASTNode* rhs = parse_assignment(l);
        }
    }
    else
        sic_error_fatal("Expected identifier.");
    return head.next;
}

static ASTNode* parse_assignment(Lexer* l)
{
    ASTNode* node = parse_ternary(l); // TODO: Change this to look ahead for unary.
    if(tok_equal(l, TOKEN_ASSIGN))
    {
        ASTNode* new_node = create_node(NODE_ASSIGN, peek(l));
        new_node->children = node;
        advance(l);
        node->next = parse_assignment(l);
        return new_node;
    }

    return node;
}

static ASTNode* parse_ternary(Lexer* l)
{
    ASTNode* condition = parse_logical_or(l);

    if(!tok_equal(l, TOKEN_QUESTION))
        return condition;

    ASTNode* tern = create_node(NODE_TERNARY, peek(l));
    advance(l);
    ASTNode* true_node = parse_assignment(l);
    consume(l, TOKEN_COLON);
    ASTNode* false_node = parse_ternary(l);
    condition->next = true_node;
    true_node->next = false_node;
    false_node->next = NULL;
    tern->children = condition;
    return tern;
}

static ASTNode* parse_logical_or(Lexer* l)
{
    ASTNode* node = parse_logical_and(l);
    while(tok_equal(l, TOKEN_LOG_OR))
    {
        ASTNode* new_node = create_node(NODE_LOG_OR, peek(l));
        advance(l);
        ASTNode* rhs = parse_logical_and(l);
        node->next = rhs;
        rhs->next = NULL;
        new_node->children = node;
        node = new_node;
    }
    return node;
}

static ASTNode* parse_logical_and(Lexer* l)
{
    ASTNode* node = parse_bitwise_or(l);
    while(tok_equal(l, TOKEN_LOG_AND))
    {
        ASTNode* new_node = create_node(NODE_LOG_AND, peek(l));
        advance(l);
        ASTNode* rhs = parse_bitwise_or(l);
        node->next = rhs;
        rhs->next = NULL;
        new_node->children = node;
        node = new_node;
    }
    return node;
}

static ASTNode* parse_bitwise_or(Lexer* l)
{
    ASTNode* node = parse_bitwise_xor(l);
    while(tok_equal(l, TOKEN_BIT_OR))
    {
        ASTNode* new_node = create_node(NODE_BIT_OR, peek(l));
        advance(l);
        ASTNode* rhs = parse_bitwise_xor(l);
        node->next = rhs;
        rhs->next = NULL;
        new_node->children = node;
        node = new_node;
    }
    return node;

}

static ASTNode* parse_bitwise_xor(Lexer* l)
{
    ASTNode* node = parse_bitwise_and(l);
    while(tok_equal(l, TOKEN_BIT_XOR))
    {
        ASTNode* new_node = create_node(NODE_BIT_XOR, peek(l));
        advance(l);
        ASTNode* rhs = parse_bitwise_and(l);
        node->next = rhs;
        rhs->next = NULL;
        new_node->children = node;
        node = new_node;
    }
    return node;

}

static ASTNode* parse_bitwise_and(Lexer* l)
{
    ASTNode* node = parse_logical_equality(l);
    while(tok_equal(l, TOKEN_AMP))
    {
        ASTNode* new_node = create_node(NODE_BIT_AND, peek(l));
        advance(l);
        ASTNode* rhs = parse_logical_equality(l);
        node->next = rhs;
        rhs->next = NULL;
        new_node->children = node;
        node = new_node;
    }
    return node;

}

static ASTNode* parse_logical_equality(Lexer* l)
{
    ASTNode* node = parse_relational(l);
    while(true)
    {
        ASTNode* new_node;
        if(tok_equal(l, TOKEN_LOG_EQUIV))
            new_node = create_node(NODE_EQ, peek(l));
        else if(tok_equal(l, TOKEN_LOG_NOT_EQUIV))
            new_node = create_node(NODE_NE, peek(l));
        else
            return node;

        advance(l);
        ASTNode* rhs = parse_relational(l);
        node->next = rhs;
        rhs->next = NULL;
        new_node->children = node;
        node = new_node;
    }
}

static ASTNode* parse_relational(Lexer* l)
{
    ASTNode* node = parse_bitwise_shift(l);
    while(true)
    {
        ASTNode* new_node;
        bool reverse = false;
        if(tok_equal(l, TOKEN_LT))
            new_node = create_node(NODE_LT, peek(l));
        else if(tok_equal(l, TOKEN_LE))
            new_node = create_node(NODE_LE, peek(l));
        else if(tok_equal(l, TOKEN_GT))
        {
            reverse = true;
            new_node = create_node(NODE_LT, peek(l));
        }
        else if(tok_equal(l, TOKEN_GE))
        {
            reverse = true;
            new_node = create_node(NODE_LE, peek(l));
        }
        else
            return node;

        advance(l);
        ASTNode* other = parse_bitwise_shift(l);
        if(reverse)
        {
            other->next = node;
            node->next = NULL;
            new_node->children = other;
        }
        else
        {
            node->next = other;
            other->next = NULL;
            new_node->children = node;
        }
        node = new_node;
    }

}

static ASTNode* parse_bitwise_shift(Lexer* l)
{
    ASTNode* node = parse_add_and_sub(l);
    while(true)
    {
        ASTNode* new_node;
        if(tok_equal(l, TOKEN_SHL))
            new_node = create_node(NODE_SHL, peek(l));
        else if(tok_equal(l, TOKEN_SHR))
            new_node = create_node(NODE_SHR, peek(l));
        else
            return node;

        advance(l);
        ASTNode* rhs = parse_add_and_sub(l);
        node->next = rhs;
        rhs->next = NULL;
        new_node->children = node;
        node = new_node;
    }
}

static ASTNode* parse_add_and_sub(Lexer* l)
{
    ASTNode* node = parse_mul_div_and_mod(l);
    while(true)
    {
        ASTNode* new_node;
        if(tok_equal(l, TOKEN_ADD))
            new_node = create_node(NODE_ADD, peek(l));
        else if(tok_equal(l, TOKEN_SUB))
            new_node = create_node(NODE_SUB, peek(l));
        else
            return node;

        advance(l);
        ASTNode* rhs = parse_mul_div_and_mod(l);
        node->next = rhs;
        rhs->next = NULL;
        new_node->children = node;
        node = new_node;
    }

}

static ASTNode* parse_mul_div_and_mod(Lexer* l)
{
    ASTNode* node = parse_cast(l);
    while(true)
    {
        ASTNode* new_node;
        if(tok_equal(l, TOKEN_ASTERISK))
            new_node = create_node(NODE_MUL, peek(l));
        else if(tok_equal(l, TOKEN_DIV))
            new_node = create_node(NODE_DIV, peek(l));
        else if(tok_equal(l, TOKEN_MOD))
            new_node = create_node(NODE_MOD, peek(l));
        else
            return node;

        advance(l);
        ASTNode* rhs = parse_cast(l);
        node->next = rhs;
        rhs->next = NULL;
        new_node->children = node;
        node = new_node;
    }
}

static ASTNode* parse_cast(Lexer* l)
{
    // if(tok_equal(l, ) && 
    // {
    //     sic_error_fatal("Casts not implemented yet.");
        // Token* cast_tok = s_token;
        // Type* type;
        // parse_type_prefix(&type, NULL);
        // expect(&s_token, ")");
        // ASTNode* node = create_node(NODE_CAST, cast_tok);
        // node->next = NULL;
        // node->children = parse_cast(l);
    // }
    return parse_unary(l);
}

static ASTNode* parse_unary(Lexer* l)
{
    if(tok_equal(l, TOKEN_SUB))
    {
        ASTNode* node = create_node(NODE_NEG, peek(l));
        advance(l);
        node->next = NULL;
        node->children = parse_cast(l);
        return node;
    }
    
    if(tok_equal(l, TOKEN_AMP))
    {
        ASTNode* node = create_node(NODE_ADDR_OF, peek(l));
        advance(l);
        node->next = NULL;
        node->children = parse_cast(l);
        return node;
    }

    if(tok_equal(l, TOKEN_ASTERISK))
    {
        ASTNode* node = create_node(NODE_DEREF, peek(l));
        advance(l);
        node->next = NULL;
        node->children = parse_cast(l);
        return node;
    }

    if(tok_equal(l, TOKEN_LOG_NOT))
    {
        ASTNode* node = create_node(NODE_LOG_NOT, peek(l));
        advance(l);
        node->next = NULL;
        node->children = parse_cast(l);
        return node;
    }

    if(tok_equal(l, TOKEN_BIT_NOT))
    {
        ASTNode* node = create_node(NODE_BIT_NOT, peek(l));
        advance(l);
        node->next = NULL;
        node->children = parse_cast(l);
        return node;
    }
    return parse_postfix(l);
}


static ASTNode* parse_postfix(Lexer* l)
{
    ASTNode* node = parse_primary_expr(l);
    while(true)
    {
        if(tok_equal(l, TOKEN_LPAREN))
        {
            parse_func_call(l);
        }

        return node;
    }
}

static ASTNode* parse_func_call(Lexer* l)
{
    (void)l;
    sic_error_fatal("Function calls are unimplemented.");
}

static ASTNode* parse_primary_expr(Lexer* l)
{
    if(tok_equal(l, TOKEN_IDENT))
    {
        Object* var = get_var(peek(l));
        if(var)
        {
            ASTNode* new_node = create_node(NODE_VAR, peek(l));
            new_node->var = var;
            advance(l);
            return new_node;
        }
        sic_error_fatal("Undefined variable \'%.*s\'.", (int)peek(l)->len, peek(l)->loc);
    }
    else if(tok_equal(l, TOKEN_NUM))
    {
        ASTNode* new_node = create_node(NODE_NUM, peek(l));
        advance(l);
        return new_node;
    }

    sic_error_fatal("Expected an expression.");
    return NULL;
}

static void enter_scope(void)
{
    Scope* new_scope = malloc(sizeof(Scope));
    new_scope->parent = s_curscope;
    hashmap_init(&new_scope->vars);
    s_curscope = new_scope;
}

static void exit_scope(void)
{
    hashmap_free(&s_curscope->vars);
    Scope* prev = s_curscope;
    s_curscope = s_curscope->parent;
    free(prev);
}

static Object* create_obj_in_scope(Token* symbol, bool global)
{
    Object* obj = malloc(sizeof(Object));
    obj->symbol = *symbol;
    if(global)
    {
        obj->next = s_globals;
        s_globals = obj;
    }
    else
    {
        obj->next = s_locals;
        s_locals = obj;
    }
    hashmap_putn(&s_curscope->vars, symbol->loc, symbol->len, obj);
    return obj;
}

static Object* get_var(Token* symbol)
{
    for(Scope* s = s_curscope; s != NULL; s = s->parent)
    {
        Object* var = hashmap_getn(&s->vars, symbol->loc, symbol->len);
        if(var != NULL)
            return var;
    }
    return NULL;
}

static ASTNode* create_node(NodeKind kind, Token* token)
{
    ASTNode* res = malloc(sizeof(ASTNode));
    res->kind = kind;
    res->token = *token;
    res->next = NULL;
    res->children = NULL;
    return res;
}
