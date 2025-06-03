#include "parser.h"
#include "core.h"
#include "type.h"
#include "utils/error.h"
#include "utils/hash.h"

#define SET_EXISTS ((void*)true)

typedef struct Scope Scope;

struct Scope
{
    Scope*  parent;

    HashMap vars;
    HashMap types;
};

typedef enum
{
    OBJ_ATTR_NONE   = 0,
    OBJ_ATTR_EXTERN = (1 << 0),
} ObjAttr;

// Global check and add functions
static bool function_definition(Type* ret_type, ObjAttr* func_attr);

// Grammar parsing
static void     parse_type_prefix(Type** type, ObjAttr* attr);
static void     parse_func_params(Object** params, size_t* count);
static ASTNode* parse_stmt_block(void);
static ASTNode* parse_stmt(void);
static ASTNode* parse_declaration(void);
static ASTNode* parse_assignment(void);
static ASTNode* parse_ternary(void);
static ASTNode* parse_logical_or(void);
static ASTNode* parse_logical_and(void);
static ASTNode* parse_bitwise_or(void);
static ASTNode* parse_bitwise_xor(void);
static ASTNode* parse_bitwise_and(void);
static ASTNode* parse_logical_equality(void);
static ASTNode* parse_relational(void);
static ASTNode* parse_bitwise_shift(void);
static ASTNode* parse_add_and_sub(void);
static ASTNode* parse_mul_div_and_mod(void);
static ASTNode* parse_cast(void);
static ASTNode* parse_unary(void);
static ASTNode* parse_postfix(void);
static ASTNode* parse_func_call(void);
static ASTNode* parse_primary_expr(void);
// static 

// Scope and symbol defining/finding functions
static void    enter_scope(void);
static void    exit_scope(void);
static Object* create_obj_in_scope(Token* symbol, bool global);
static Object* get_var(Token* symbol);

// Token/Node Helpers
static bool     consume(Token** token, char* expected);
static void     expect(Token** token, char* expected);
static ASTNode* create_node(NodeType type, Token* token);

static Object* s_globals;
static Object* s_locals;
static Scope*  s_curscope;
static Scope   s_file_scope;
static Object* s_curfunc;
static HashMap s_kw_map;
static Token*  s_token;

void init_parser(void)
{
    static const char* keywords[] = {
        "void", "u8", "s8", "i8", "u16", "s16", "i16",
        "u32", "s32", "i32", "u64", "s64", "i64", "f32",
        "f64", "f128", "extern"
    };

    size_t kw_cnt = sizeof(keywords) / sizeof(keywords[0]);
    hashmap_initn(&s_kw_map, kw_cnt);

    for(size_t i = 0; i < kw_cnt; ++i)
        hashmap_put(&s_kw_map, keywords[i], SET_EXISTS);

    hashmap_init(&s_file_scope.vars);
    s_globals   = NULL;
    s_locals    = NULL;
    s_curscope = NULL;
    s_curfunc  = NULL;
}

Object* parse_tokens(Token* tokens)
{
    SIC_ASSERT(tokens != NULL);
    s_globals = NULL;
    s_locals = NULL;
    s_file_scope.parent = NULL;
    s_curscope = &s_file_scope;
    hashmap_clear(&s_curscope->vars);

    s_token = tokens;

    while(s_token->type != TOKEN_EOF)
    {
        Type* type;
        ObjAttr attr;
        parse_type_prefix(&type, &attr);
        if(type == NULL)
            sic_error_fatal("Missing type specifier."); // TODO: Change this to be non fatal error with line printing and such

        if(function_definition(type, &attr))
             continue;
        tokens = tokens->next;
    }
    return s_globals;
}

static bool function_definition(Type* ret_type, ObjAttr* func_attr)
{
    (void)func_attr;
    if(s_token->type != TOKEN_IDNT ||
       !tok_equal(s_token->next, "("))
        return false;


    Object* func = create_obj_in_scope(s_token, true);
    FuncComps* func_comps = &func->comps.func;
    func->is_function = true;
    func_comps->ret_type = ret_type;
    func->symbol = s_token;

    s_token = s_token->next->next;
    parse_func_params(&func_comps->params, &func_comps->param_cnt);

    s_curfunc = func;
    s_locals = NULL;
    enter_scope();

    s_locals = func_comps->params;
    for(size_t i = 0; i < func_comps->param_cnt; ++i)
    {
        if(s_locals->symbol)
            hashmap_putn(&s_curscope->vars, s_locals->symbol->ref, s_locals->symbol->len, s_locals);
        s_locals = s_locals->next;
    }
    s_locals = func_comps->params;

    expect(&s_token, "{");

    ASTNode* body_block = parse_stmt_block();
    func_comps->body = body_block->children;
    free(body_block);

    func_comps->local_objs = s_locals;
    exit_scope();

    return true;
}

static void parse_type_prefix(Type** type, ObjAttr* attr)
{
    const bool storage_class_allowed = attr != NULL;
    bool seen_type = false;
    Type* ty = NULL;

    // TODO: Make the kw map hold values depending on whether the kw is a typename or not to
    //       make this function better. Furthermore it could be used to remove the string comparisons
    //       making the process quicker.
    while(hashmap_getn(&s_kw_map, s_token->ref, s_token->len) != NULL)
    {
        if(tok_equal(s_token, "extern"))
        {
            if(!storage_class_allowed)
                sic_error_fatal("Storage class specification not allowed in this context.");
            *attr |= OBJ_ATTR_EXTERN;
        }
        else if(seen_type)
            sic_error_fatal("Cannot combine multiple types.");
        else if(tok_equal(s_token, "void"))
        {
            ty = t_void;
            seen_type = true;
        }
        else if(tok_equal(s_token, "u8"))
        {
            ty = t_u8;
            seen_type = true;
        }
        else if(tok_equal(s_token, "s8"))
        {
            ty = t_s8;
            seen_type = true;
        }
        else if(tok_equal(s_token, "u16"))
        {
            ty = t_u16;
            seen_type = true;
        }
        else if(tok_equal(s_token, "s16"))
        {
            ty = t_s16;
            seen_type = true;
        }
        else if(tok_equal(s_token, "u32"))
        {
            ty = t_u32;
            seen_type = true;
        }
        else if(tok_equal(s_token, "s32"))
        {
            ty = t_s32;
            seen_type = true;
        }
        else if(tok_equal(s_token, "u64"))
        {
            ty = t_u64;
            seen_type = true;
        }
        else if(tok_equal(s_token, "s64"))
        {
            ty = t_s64;
            seen_type = true;
        }
        s_token = s_token->next;
    }
    

    *type = ty;
}

static void parse_func_params(Object** params, size_t* count)
{
    Object head;
    head.next = NULL;
    Object* cur_param = &head;
    size_t cnt = 0;

    while(!tok_equal(s_token, ")"))
    {
        if(s_token->type == TOKEN_EOF)
            sic_error_fatal("No closing parentheses.");
        
        cnt++;
        if(cur_param != &head)
            expect(&s_token, ",");


        Type* type;
        parse_type_prefix(&type, NULL);
        if(type == NULL)
            sic_error_fatal("Missing type specifier.");

        if(s_token->type != TOKEN_IDNT)
            sic_error_fatal("Paramater does not have name.");

        cur_param->next = malloc(sizeof(Object));
        cur_param = cur_param->next;
        cur_param->is_function = false;
        cur_param->next = NULL;
        cur_param->comps.var.type = type;
        cur_param->symbol = s_token;

        s_token = s_token->next;
    }
    
    s_token = s_token->next;
    *params = head.next;
    *count = cnt;
}

static ASTNode* parse_stmt_block(void)
{
    ASTNode* block = malloc(sizeof(ASTNode));
    block->type = NODE_BLOCK;
    block->token = s_token;
    
    ASTNode head;
    head.next = NULL;
    ASTNode* cur_expr = &head;

    while(!tok_equal(s_token, "}"))
    {
        if(s_token->type == TOKEN_EOF)
            sic_error_fatal("No closing }.");
        Type* type;
        ObjAttr attr;
        parse_type_prefix(&type, &attr);

        if(type == NULL)
        {
            cur_expr->next = parse_stmt();
            cur_expr = cur_expr->next;
        }
        else
            s_token = s_token->next;
    }

    block->children = head.next;
    s_token = s_token->next;
    return block;
}

static ASTNode* parse_stmt(void)
{
    if(tok_equal(s_token, ";"))
    {
        ASTNode* new_node = create_node(NODE_NOP, s_token);
        s_token = s_token->next;
        new_node->next = NULL;
        new_node->children = NULL;
        return new_node;
    }

    if(tok_equal(s_token, "return"))
    {
        ASTNode* new_node = create_node(NODE_RETURN, s_token);
        Type* ret_type = s_curfunc->comps.func.ret_type;
        s_token = s_token->next;
        if(consume(&s_token, ";"))
        {
            if(ret_type != t_void)
                sic_error_fatal("Non-void function should return a value.");
            return new_node;
        }

        ASTNode* ret_expr = parse_assignment();
        expect(&s_token, ";");

        new_node->children = ret_expr;
        return new_node;
    }

    ASTNode* res = parse_assignment();
    expect(&s_token, ";");
    return res;
}

static UNUSED ASTNode* parse_declaration(void)
{
    ASTNode head;
    head.next = NULL;
    // ASTNode* cur = &head;

    Type* type;
    ObjAttr attr;
    parse_type_prefix(&type, &attr);
    if(type == t_void)
        sic_error_fatal("Variables cannot be declared as type void.");

    if(s_token->type == TOKEN_IDNT)
    {
        Object* new_var = create_obj_in_scope(s_token, false);
        new_var->is_function = false;
        new_var->comps.var.type = type;
        if(tok_equal(s_token, "="))
        {
            // ASTNode* var_node = create_node(NODE_VAR, new_var->symbol);
            // ASTNode* assign = create_node(NODE_ASSIGN, s_token);
            // s_token = s_token->next;
            // ASTNode* rhs = parse_assignment();
        }
    }
    else
        sic_error_fatal("Expected identifier.");
    return head.next;
}

static ASTNode* parse_assignment(void)
{
    ASTNode* node = parse_ternary(); // TODO: Change this to look ahead for unary.
    if(tok_equal(s_token, "="))
    {
        ASTNode* new_node = create_node(NODE_ASSIGN, s_token);
        new_node->children = node;
        s_token = s_token->next;
        node->next = parse_assignment();
        return new_node;
    }

    return node;
}

static ASTNode* parse_ternary(void)
{
    ASTNode* condition = parse_logical_or();

    if(!tok_equal(s_token, "?"))
        return condition;

    ASTNode* tern = create_node(NODE_TERNARY, s_token);
    s_token = s_token->next;
    ASTNode* true_node = parse_assignment();
    expect(&s_token, ":");
    ASTNode* false_node = parse_ternary();
    condition->next = true_node;
    true_node->next = false_node;
    false_node->next = NULL;
    tern->children = condition;
    return tern;
}

static ASTNode* parse_logical_or(void)
{
    ASTNode* node = parse_logical_and();
    while(tok_equal(s_token, "||"))
    {
        ASTNode* new_node = create_node(NODE_LOG_OR, s_token);
        s_token = s_token->next;
        ASTNode* rhs = parse_logical_and();
        node->next = rhs;
        rhs->next = NULL;
        new_node->children = node;
        node = new_node;
    }
    return node;
}

static ASTNode* parse_logical_and(void)
{
    ASTNode* node = parse_bitwise_or();
    while(tok_equal(s_token, "&&"))
    {
        ASTNode* new_node = create_node(NODE_LOG_AND, s_token);
        s_token = s_token->next;
        ASTNode* rhs = parse_bitwise_or();
        node->next = rhs;
        rhs->next = NULL;
        new_node->children = node;
        node = new_node;
    }
    return node;
}

static ASTNode* parse_bitwise_or(void)
{
    ASTNode* node = parse_bitwise_xor();
    while(tok_equal(s_token, "|"))
    {
        ASTNode* new_node = create_node(NODE_BIT_OR, s_token);
        s_token = s_token->next;
        ASTNode* rhs = parse_bitwise_xor();
        node->next = rhs;
        rhs->next = NULL;
        new_node->children = node;
        node = new_node;
    }
    return node;

}

static ASTNode* parse_bitwise_xor(void)
{
    ASTNode* node = parse_bitwise_and();
    while(tok_equal(s_token, "^"))
    {
        ASTNode* new_node = create_node(NODE_BIT_XOR, s_token);
        s_token = s_token->next;
        ASTNode* rhs = parse_bitwise_and();
        node->next = rhs;
        rhs->next = NULL;
        new_node->children = node;
        node = new_node;
    }
    return node;

}

static ASTNode* parse_bitwise_and(void)
{
    ASTNode* node = parse_logical_equality();
    while(tok_equal(s_token, "&"))
    {
        ASTNode* new_node = create_node(NODE_BIT_AND, s_token);
        s_token = s_token->next;
        ASTNode* rhs = parse_logical_equality();
        node->next = rhs;
        rhs->next = NULL;
        new_node->children = node;
        node = new_node;
    }
    return node;

}

static ASTNode* parse_logical_equality(void)
{
    ASTNode* node = parse_relational();
    while(true)
    {
        ASTNode* new_node;
        if(tok_equal(s_token, "=="))
            new_node = create_node(NODE_EQ, s_token);
        else if(tok_equal(s_token, "!="))
            new_node = create_node(NODE_NE, s_token);
        else
            return node;

        s_token = s_token->next;
        ASTNode* rhs = parse_relational();
        node->next = rhs;
        rhs->next = NULL;
        new_node->children = node;
        node = new_node;
    }
}

static ASTNode* parse_relational(void)
{
    ASTNode* node = parse_bitwise_shift();
    while(true)
    {
        ASTNode* new_node;
        bool reverse = false;
        if(tok_equal(s_token, "<"))
            new_node = create_node(NODE_LT, s_token);
        else if(tok_equal(s_token, "<="))
            new_node = create_node(NODE_LE, s_token);
        else if(tok_equal(s_token, ">"))
        {
            reverse = true;
            new_node = create_node(NODE_LT, s_token);
        }
        else if(tok_equal(s_token, ">="))
        {
            reverse = true;
            new_node = create_node(NODE_LE, s_token);
        }
        else
            return node;

        s_token = s_token->next;
        ASTNode* other = parse_bitwise_shift();
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

static ASTNode* parse_bitwise_shift(void)
{
    ASTNode* node = parse_add_and_sub();
    while(true)
    {
        ASTNode* new_node;
        if(tok_equal(s_token, "<<"))
            new_node = create_node(NODE_SHL, s_token);
        else if(tok_equal(s_token, ">>"))
            new_node = create_node(NODE_SHR, s_token);
        else
            return node;

        s_token = s_token->next;
        ASTNode* rhs = parse_add_and_sub();
        node->next = rhs;
        rhs->next = NULL;
        new_node->children = node;
        node = new_node;
    }
}

static ASTNode* parse_add_and_sub(void)
{
    ASTNode* node = parse_mul_div_and_mod();
    while(true)
    {
        ASTNode* new_node;
        if(tok_equal(s_token, "+"))
            new_node = create_node(NODE_ADD, s_token);
        else if(tok_equal(s_token, "-"))
            new_node = create_node(NODE_SUB, s_token);
        else
            return node;

        s_token = s_token->next;
        ASTNode* rhs = parse_mul_div_and_mod();
        node->next = rhs;
        rhs->next = NULL;
        new_node->children = node;
        node = new_node;
    }

}

static ASTNode* parse_mul_div_and_mod(void)
{
    ASTNode* node = parse_cast();
    while(true)
    {
        ASTNode* new_node;
        if(tok_equal(s_token, "*"))
            new_node = create_node(NODE_MUL, s_token);
        else if(tok_equal(s_token, "/"))
            new_node = create_node(NODE_DIV, s_token);
        else if(tok_equal(s_token, "%"))
            new_node = create_node(NODE_MOD, s_token);
        else
            return node;

        s_token = s_token->next;
        ASTNode* rhs = parse_cast();
        node->next = rhs;
        rhs->next = NULL;
        new_node->children = node;
        node = new_node;
    }
}

static ASTNode* parse_cast(void)
{
    if(tok_equal(s_token, "(") && hashmap_getn(&s_kw_map, s_token->next->ref, s_token->next->len) != NULL)
    {
        sic_error_fatal("Casts not implemented yet.");
        // Token* cast_tok = s_token;
        // Type* type;
        // parse_type_prefix(&type, NULL);
        // expect(&s_token, ")");
        // ASTNode* node = create_node(NODE_CAST, cast_tok);
        // node->next = NULL;
        // node->children = parse_cast();
    }
    return parse_unary();
}

static ASTNode* parse_unary(void)
{
    if(tok_equal(s_token, "-"))
    {
        ASTNode* node = create_node(NODE_NEG, s_token);
        s_token = s_token->next;
        node->next = NULL;
        node->children = parse_cast();
        return node;
    }
    
    if(tok_equal(s_token, "&"))
    {
        ASTNode* node = create_node(NODE_ADDR_OF, s_token);
        s_token = s_token->next;
        node->next = NULL;
        node->children = parse_cast();
        return node;
    }

    if(tok_equal(s_token, "*"))
    {
        ASTNode* node = create_node(NODE_DEREF, s_token);
        s_token = s_token->next;
        node->next = NULL;
        node->children = parse_cast();
        return node;
    }

    if(tok_equal(s_token, "!"))
    {
        ASTNode* node = create_node(NODE_LOG_NOT, s_token);
        s_token = s_token->next;
        node->next = NULL;
        node->children = parse_cast();
        return node;
    }

    if(tok_equal(s_token, "~"))
    {
        ASTNode* node = create_node(NODE_BIT_NOT, s_token);
        s_token = s_token->next;
        node->next = NULL;
        node->children = parse_cast();
        return node;
    }
    return parse_postfix();
}


static ASTNode* parse_postfix(void)
{
    ASTNode* node = parse_primary_expr();
    while(true)
    {
        if(tok_equal(s_token, "("))
        {
            parse_func_call();
        }

        return node;
    }
}

static ASTNode* parse_func_call(void)
{
    sic_error_fatal("Function calls are unimplemented.");
}

static ASTNode* parse_primary_expr(void)
{
    if(s_token->type == TOKEN_IDNT)
    {
        Object* var = get_var(s_token);
        if(var)
        {
            ASTNode* new_node = create_node(NODE_VAR, s_token);
            new_node->var = var;
            s_token = s_token->next;
            return new_node;
        }
        sic_error_fatal("Undefined variable \'%.*s\'.", (int)s_token->len, s_token->ref);
    }
    else if(s_token->type == TOKEN_NUM)
    {
        ASTNode* new_node = create_node(NODE_NUM, s_token);
        s_token = s_token->next;
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
    obj->symbol = symbol;
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
    hashmap_putn(&s_curscope->vars, symbol->ref, symbol->len, obj);
    return obj;
}

static Object* get_var(Token* symbol)
{
    for(Scope* s = s_curscope; s != NULL; s = s->parent)
    {
        Object* var = hashmap_getn(&s->vars, symbol->ref, symbol->len);
        if(var != NULL)
            return var;
    }
    return NULL;
}

static bool consume(Token** token, char* expected)
{
    Token* t = *token;
    if(!tok_equal(t, expected))
        return false;
    *token = t->next;
    return true;
}

static void expect(Token** token, char* expected)
{
    if(!consume(token, expected))
        sic_error_fatal("Expected token \'%s\'.", expected);
}

static ASTNode* create_node(NodeType type, Token* token)
{
    ASTNode* res = malloc(sizeof(ASTNode));
    res->type = type;
    res->token = token;
    res->next = NULL;
    res->children = NULL;
    return res;
}
