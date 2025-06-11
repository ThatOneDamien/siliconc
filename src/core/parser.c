#include "internal.h"
#include "utils/error.h"
#include "utils/lib.h"

typedef struct Scope Scope;
struct Scope
{
    Scope*  parent;

    HashMap vars;
    HashMap types;
};

// Global check and add functions
static bool     function_declaration(Lexer* l, Type* ret_type, StorageClass func_storage);

// Grammar parsing
static bool     parse_storage_class(Lexer* l, StorageClass* storage);
static bool     parse_type_qualifier(Lexer* l, TypeQualifier* qual);
static void     parse_type_prefix(Lexer* l, Type** type, StorageClass* storage);
static bool     parse_func_params(Lexer* l, Object** params, size_t* count);
static ASTNode* parse_stmt_block(Lexer* l);
static ASTNode* parse_stmt(Lexer* l);
static ASTNode* parse_declaration(Lexer* l, Type* type, StorageClass storage);
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
static Object*  scope_def_obj(Lexer* l, Token* symbol, StorageClass storage);
static Object*  get_var(Token* symbol);
static Type*    get_type(Token* tok);
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
static ASTNode s_badnode;

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
        StorageClass storage = STORAGE_DEFAULT;
        parse_type_prefix(lexer, &type, &storage);
        
        bool recovery_needed = type == NULL;

        if(type == NULL)
            parser_error(lexer, "Missing type specifier.");
        else if(function_declaration(lexer, type, storage))
             continue;
        else
            recovery_needed = true;
        
        if(recovery_needed)
        {
            printf("recovering.\n");
            while(true)
            {
                Token* t = peek_forw(lexer, 1);
                if(t->kind == TOKEN_EOF)
                {
                    advance(lexer);
                    break;
                }
                if(t->loc == t->line_start &&
                    (t->kind == TOKEN_IDENT ||
                    (t->kind >= TOKEN_KEYWORD_START &&
                     t->kind <= TOKEN_KEYWORD_END)))
                    break;
                advance(lexer);
            }
        }
    }
    return s_globals;
}

// function_declaration -> type_prefix identifier "(" func_params (";" | "{" stmt_block)
static bool function_declaration(Lexer* l, Type* ret_type, StorageClass func_storage)
{
    (void)func_storage;
    if(!tok_equal(l, TOKEN_IDENT)||
       !tok_equal_forw(l, 1, TOKEN_LPAREN))
        return false;

    if(func_storage == STORAGE_DEFAULT)
        func_storage = STORAGE_GLOBAL;
    Object* func = scope_def_obj(l, peek(l), func_storage);
    ObjFunc* comps = &func->func;
    func->kind = OBJ_FUNC;
    comps->ret_type = ret_type;
    func->symbol = *peek(l);

    advance_many(l, 2);
    if(!parse_func_params(l, &comps->params, &comps->param_cnt))
        return false;

    s_curfunc = func;
    s_locals = NULL;
    enter_scope();

    s_locals = comps->params;
    for(size_t i = 0; i < comps->param_cnt; ++i)
    {
        if(s_locals->symbol.loc)
            hashmap_putn(&s_curscope->vars, s_locals->symbol.loc, s_locals->symbol.len, s_locals);
        s_locals = s_locals->next;
    }
    s_locals = comps->params;

    consume(l, TOKEN_LBRACE);

    ASTNode* body_block = parse_stmt_block(l);
    comps->body = body_block->children;

    comps->local_objs = s_locals;
    exit_scope();

    return true;
}

static bool parse_storage_class(Lexer* l, StorageClass* storage)
{
    StorageClass temp;
    switch(peek(l)->kind)
    {
    case TOKEN_EXTERN:
        temp = STORAGE_EXTERN;
        break;
    default:
        temp = STORAGE_DEFAULT;
        break;
    }

    if(temp != STORAGE_DEFAULT)
    {
        if(storage == NULL)
            parser_error(l, "Storage class not allowed in this context.");
        else if(*storage == temp) {} // This would be where we warn for duplicate storage class
        else if(*storage != STORAGE_DEFAULT)
            parser_error(l, "Multiple storage classes cannot be combined.");
        else
            *storage = temp;
        advance(l);
        return true;
    }
    return false;
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

// storage_class -> "extern"
// type_qualifier -> "const" TODO: NOT IMPLEMENTED YET!!!
// type_name -> "void" | "u8" | "s8" | "u16" | "s16" |
//              "u32" | "s32" | "u64" | "s64" | "f32" | 
//              "f64" | identifier
// type_suffix -> "(" func_params type_qualifier* |
//                "[" array_dims | TODO: NOT IMPLEMENTED YET!!!
//                "*" type_qualifier* |
// type_prefix -> (storage_class | type_qualifier | type_name)* type_suffix*
// NOTE: You are only allowed to specify one storage class and one type name
//       per type prefix. You can have multiple qualifiers, but if any one
//       qualifier is duplicated a warning will be raised.
static void parse_type_prefix(Lexer* l, Type** type, StorageClass* storage)
{
    Type* ty = NULL;
    TypeQualifier qual = QUALIFIER_NONE;
    // TODO: Make the kw map hold values depending on whether the kw is a typename or not to
    //       make this function better. Furthermore it could be used to remove the string comparisons
    //       making the process quicker.
    while(true)
    {
        if(parse_storage_class(l, storage))
            continue;
        if(parse_type_qualifier(l, &qual))
            continue;
        Type* t = get_type(peek(l));
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
    ASTNode* block = MALLOC(sizeof(ASTNode));
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
        StorageClass storage;
        parse_type_prefix(l, &type, &storage);

        if(type == NULL)
            cur_expr->next = parse_stmt(l);
        else
        {
            cur_expr->next = parse_declaration(l, type, storage);
            if(cur_expr->next == NULL)
                continue;
        }
        cur_expr = cur_expr->next;
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
        Type* ret_type = s_curfunc->func.ret_type;
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

static ASTNode* parse_declaration(Lexer* l, Type* type, StorageClass storage)
{
    ASTNode head;
    head.next = NULL;
    ASTNode* cur = &head;

    if(type == g_type_void)
        sic_error_fatal("Variables cannot be declared as type void.");

    while(expect(l, TOKEN_IDENT))
    {
        if(storage == STORAGE_DEFAULT)
            storage = STORAGE_SCOPE;
        Object* new_var = scope_def_obj(l, peek(l), storage);
        new_var->kind = OBJ_VAR;
        new_var->var.type = type;
        advance(l);
        if(tok_equal(l, TOKEN_ASSIGN))
        {
            ASTNode* assign = create_node(NODE_ASSIGN, peek(l));
            assign->children = create_node(NODE_VAR, &new_var->symbol);
            assign->children->var = new_var;
            advance(l);
            assign->children->next = parse_assignment(l);
            cur->next = assign;
            cur = cur->next;
        }
        if(try_consume(l, TOKEN_COMMA))
            continue;
        consume(l, TOKEN_SEMI);
        break;
    }
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
        sic_error("Undefined variable \'%.*s\'.", (int)peek(l)->len, peek(l)->loc);
    }
    else if(tok_equal(l, TOKEN_NUM))
    {
        ASTNode* new_node = create_node(NODE_NUM, peek(l));
        advance(l);
        return new_node;
    }
    else
        sic_error("Expected an expression.");
    advance(l);
    return &s_badnode;
}

static void enter_scope(void)
{
    Scope* new_scope = MALLOC(sizeof(Scope));
    new_scope->parent = s_curscope;
    hashmap_init(&new_scope->vars);
    s_curscope = new_scope;
}

static void exit_scope(void)
{
    hashmap_free(&s_curscope->vars);
    s_curscope = s_curscope->parent;
}

static Object*  scope_def_obj(Lexer* l, Token* symbol, StorageClass storage)
{
    Object* old = hashmap_getn(&s_curscope->vars, symbol->loc, symbol->len);
    if(old != NULL)
    {
        sic_error_at(l->file_name, symbol, "Redefinition of symbol \'%.*s\'", (int)symbol->len, symbol->loc);
        return old;
    }
    Object* obj = MALLOC(sizeof(Object));
    obj->symbol = *symbol;
    if(storage == STORAGE_SCOPE)
    {
        obj->next = s_locals;
        s_locals = obj;
    }
    else
    {
        obj->next = s_globals;
        s_globals = obj;
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

static Type* get_type(Token* tok)
{
    if(tok->kind >= TOKEN_TYPENAME_START && tok->kind <= TOKEN_TYPENAME_END)
        return builtin_type(tok->kind);
    else if(tok->kind == TOKEN_IDENT)
    {
        // Search through scopes for typedef
    }

    return NULL;
}

static ASTNode* create_node(NodeKind kind, Token* token)
{
    ASTNode* res = MALLOC(sizeof(ASTNode));
    res->kind = kind;
    res->token = *token;
    res->next = NULL;
    res->children = NULL;
    return res;
}
