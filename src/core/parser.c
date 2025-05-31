#include "parser.h"
#include "core.h"
#include "type.h"
#include "utils/error.h"
#include "utils/hash.h"

#define SET_EXISTS ((void*)true)

typedef struct Scope         Scope;

struct Scope
{
    Scope*  parent;
    HashMap vars;
};

typedef enum
{
    OBJ_ATTR_NONE   = 0,
    OBJ_ATTR_EXTERN = (1 << 0),
} ObjAttr;

// Global check and add functions
static bool function_definition(Token** token, Type* ret_type, ObjAttr* func_attr);

// Grammar parsing
static void     parse_type_prefix(Token** token, Type** type, ObjAttr* attr);
static void     parse_func_params(Token** token, Object** params, size_t* count);
static ASTNode* parse_expr_statement(Token** token);
static ASTNode* parse_assignment(Token** token);
static ASTNode* parse_identity(Token** token);
// static 

// Scope and symbol defining/finding functions
static void    enter_scope(void);
static void    exit_scope(void);
static Object* create_obj_in_scope(Token* symbol, bool global);
static Object* get_var(Token* symbol);

// Token Helpers
static bool consume(Token** token, char* expected);
static void expect(Token** token, char* expected);

static Object* global_objs;
static Object* local_objs;
static Scope*  current_scope;
static Scope   filewide_scope;
static Object* current_func;
static HashMap type_kw_map;

void init_parser(void)
{
    static const char* keywords[] = {
        "void", "u8", "s8", "i8", "u16", "s16", "i16",
        "u32", "s32", "i32", "u64", "s64", "i64", "f32",
        "f64", "f128", "extern"
    };

    size_t kw_cnt = sizeof(keywords) / sizeof(keywords[0]);
    hashmap_initn(&type_kw_map, kw_cnt);

    for(size_t i = 0; i < kw_cnt; ++i)
        hashmap_put(&type_kw_map, keywords[i], SET_EXISTS);

    hashmap_init(&filewide_scope.vars);
    global_objs   = NULL;
    local_objs    = NULL;
    current_scope = NULL;
    current_func  = NULL;
}

Object* parse_tokens(Token* tokens)
{
    SIC_ASSERT(tokens != NULL);
    global_objs = NULL;
    current_scope = &filewide_scope;
    filewide_scope.parent = NULL;
    hashmap_clear(&current_scope->vars);
    while(tokens->type != TOKEN_EOF)
    {
        Type* type;
        ObjAttr attr;
        parse_type_prefix(&tokens, &type, &attr);
        if(function_definition(&tokens, type, &attr))
             continue;
        tokens = tokens->next;
    }
    return global_objs;
}

static bool function_definition(Token** token, Type* ret_type, ObjAttr* func_attr)
{
    (void)func_attr;
    Token* t = *token;
    if(t->type != TOKEN_IDNT ||
       !tok_equal(t->next, "("))
        return false;


    Object* func = create_obj_in_scope(t, true);
    func->is_function = true;
    func->comps.func.ret_type = ret_type;
    func->symbol = t;

    t = t->next->next;
    parse_func_params(&t, &func->comps.func.params, &func->comps.func.param_cnt);

    current_func = func;
    local_objs = NULL;
    enter_scope();

    expect(&t, "{");

    while(!tok_equal(t, "}"))
    {
        if(t->type == TOKEN_EOF)
            sic_error_fatal("No closing }.");
        printf("IN BODY: %.*s\n", (int)t->len, t->ref);
        t = t->next;
    }

    t = t->next;
    exit_scope();

    *token = t;
    return true;
}

static void parse_type_prefix(Token** token, Type** type, ObjAttr* attr)
{
    const bool storage_class_allowed = attr != NULL;
    bool seen_type = false;
    Type* ty = NULL;
    Token* t = *token;

    while(hashmap_getn(&type_kw_map, t->ref, t->len) != NULL)
    {
        if(tok_equal(t, "extern"))
        {
            if(!storage_class_allowed)
                sic_error_fatal("Storage class specification not allowed in this context.");
            *attr |= OBJ_ATTR_EXTERN;
        }
        else if(seen_type)
            sic_error_fatal("Cannot combine multiple types.");
        else if(tok_equal(t, "void"))
        {
            ty = t_void;
            seen_type = true;
        }
        else if(tok_equal(t, "u8"))
        {
            ty = t_u8;
            seen_type = true;
        }
        t = t->next;
    }
    
    if(ty == NULL)
        sic_error_fatal("Missing type specifier."); // TODO: Change this to be non fatal error with line printing and such

    *type = ty;
    *token = t;
}

static void parse_func_params(Token** token, Object** params, size_t* count)
{
    Token* t = *token;
    Object head;
    head.next = NULL;
    Object* cur_param = &head;
    size_t cnt = 0;

    while(!tok_equal(t, ")"))
    {
        if(t->type == TOKEN_EOF)
            sic_error_fatal("No closing parentheses.");
        
        cnt++;
        if(cur_param != &head)
            expect(&t, ",");


        Type* type;
        parse_type_prefix(&t, &type, NULL);

        if(t->type != TOKEN_IDNT)
            sic_error_fatal("Paramater does not have name.");

        cur_param->next = malloc(sizeof(Object));
        cur_param = cur_param->next;
        cur_param->is_function = false;
        cur_param->next = NULL;
        cur_param->comps.var.type = type;
        cur_param->symbol = t;

        t = t->next;
    }
    
    *token = t->next;
    *params = head.next;
    *count = cnt;
}

static ASTNode* parse_expr_statement(Token** token)
{
    Token* t = *token;
    if(tok_equal(t, ";"))
    {
        *token = t->next;
        return NULL;
    }
}

static ASTNode* parse_assignment(Token** token)
{
    ASTNode* node = parse_identity(token);
    Token* t = *token;

    if(tok_equal(t, "="))
    {
        ASTNode* new_node = malloc(sizeof(ASTNode));
        new_node->type = NODE_ASSIGN;
        new_node->token = t;
        new_node->children = node;
        node->next = parse_identity(token);
    }

    return node;
}

static ASTNode* parse_identity(Token** token)
{
    Token* t = *token;
    if(t->type == TOKEN_IDNT)
    {
        Object* var = get_var(t);
        if(var)
        {
            ASTNode* new_node = malloc(sizeof(ASTNode));
            new_node->type = NODE_VAR;
            new_node->token = t;
            new_node->var = var;
            *token = t->next;
            return new_node;
        }
        sic_error_fatal("Undefined variable \'%.*s\'.", (int)t->len, t->ref);
    }
    else if(t->type == TOKEN_NUM)
    {
        ASTNode* new_node = malloc(sizeof(ASTNode));
        new_node->type = NODE_NUM;
        new_node->token = t;
        *token = t->next;
        return new_node;
    }

    sic_error_fatal("Expected an expression.");
    return NULL;
}

static void enter_scope(void)
{
    Scope* new_scope = malloc(sizeof(Scope));
    new_scope->parent = current_scope;
    hashmap_init(&new_scope->vars);
    current_scope = new_scope;
}

static void exit_scope(void)
{
    hashmap_free(&current_scope->vars);
    Scope* prev = current_scope;
    current_scope = current_scope->parent;
    free(prev);
}

static Object* create_obj_in_scope(Token* symbol, bool global)
{
    Object* obj = malloc(sizeof(Object));
    obj->symbol = symbol;
    if(global)
    {
        obj->next = global_objs;
        global_objs = obj;
    }
    else
    {
        obj->next = local_objs;
        local_objs = obj;
    }
    hashmap_putn(&current_scope->vars, symbol->ref, symbol->len, obj);
    return obj;
}

static Object* get_var(Token* symbol)
{
    for(Scope* s = current_scope; s != NULL; s = s->parent)
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
