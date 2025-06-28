#include "semantics.h"
#include "utils/da.h"

static void    analyze_function(SemaContext* c, Object* function);
static void    analyze_stmt(SemaContext* c, ASTNode* stmt);

static inline void push_scope(SemaContext* c);
static inline void pop_scope(SemaContext* c);


void semantic_analysis(CompilationUnit* unit)
{
    SIC_ASSERT(unit != NULL);
    SemaContext context;
    context.unit = unit;
    context.unit_scope.parent = NULL;
    context.cur_scope = &context.unit_scope;

    ObjectDA* funcs = &context.unit->funcs;
    hashmap_initn(&context.unit_scope.vars, funcs->size);
    for(size_t i = 0; i < funcs->size; ++i)
    {
        // For now, I only allow one declaration/definition of a function
        // In the future I will support multiple declarations as long as the
        // signatures match, or possibly even add function overloading, though
        // I'm not sure I want that.
        Object* func = funcs->data[i];
        if(find_obj(&context, &func->symbol) != NULL)
            sema_error(&context, &func->symbol, "Symbol redefined.");
        hashmap_putn(&context.unit_scope.vars, func->symbol.start, func->symbol.len, func);
    }


    for(size_t i = 0; i < funcs->size; ++i)
        analyze_function(&context, funcs->data[i]);
}

static void analyze_function(SemaContext* c, Object* function)
{
    push_scope(c);
    c->cur_func = function;
    for(size_t i = 0; i < function->func.params.size; ++i)
    {
        Object* param = function->func.params.data[i];
        hashmap_putn(&c->cur_scope->vars, param->symbol.start, param->symbol.len, param);
    }

    ASTNode* stmt = function->func.body;
    while(stmt != NULL)
    {
        analyze_stmt(c, stmt);
        stmt = stmt->next;
    }
    pop_scope(c);
}

static void analyze_stmt(SemaContext* c, ASTNode* stmt)
{
    switch(stmt->kind)
    {
    case NODE_BLOCK: {
        push_scope(c);
        ASTNode* sub_stmt = stmt->stmt.block.body;
        while(sub_stmt != NULL)
        {
            analyze_stmt(c, sub_stmt);
            sub_stmt = sub_stmt->next;
        }
        pop_scope(c);
        break;
    }
    case NODE_EXPR_STMT:
        analyze_expr(c, stmt->stmt.expr);
        break;
    case NODE_INVALID:
        return;
    case NODE_RETURN:
        analyze_expr(c, stmt->stmt.return_.ret_expr);
        
        break;
    case NODE_SINGLE_DECL:
        declare_obj(c, stmt->stmt.single_decl.obj);
        if(stmt->stmt.single_decl.init_expr != NULL)
            analyze_expr(c, stmt->stmt.single_decl.init_expr);
        break;
    case NODE_MULTI_DECL: {
        ASTDeclDA* decl_list = &stmt->stmt.multi_decl;
        for(size_t i = 0; i < decl_list->size; ++i)
        {
            declare_obj(c, decl_list->data[i].obj);
            if(decl_list->data[i].init_expr != NULL)
                analyze_expr(c, decl_list->data[i].init_expr);
        }
        break;
    }
    default:
        break;
    }
}

void declare_obj(SemaContext* c, Object* obj)
{
    if(hashmap_getn(&c->cur_scope->vars, obj->symbol.start, obj->symbol.len) != NULL)
        sema_error(c, &obj->symbol, "Redefinition of symbol \'%.*s\'.", obj->symbol.len, obj->symbol.start);
    hashmap_putn(&c->cur_scope->vars, obj->symbol.start, obj->symbol.len, obj);
    if(c->cur_func == NULL)
    {
        // This is a global variable, declare it in the compilation unit.
    }
    else
        da_append(&c->cur_func->func.local_objs, obj);
}

Object* find_obj(SemaContext* c, SourceLoc* symbol)
{
    for(Scope* sc = c->cur_scope; sc != NULL; sc = sc->parent)
    {
        Object* o = hashmap_getn(&sc->vars, symbol->start, symbol->len);
        if(o != NULL)
            return o;
    }
    return NULL;
}

static void push_scope(SemaContext* c)
{
    Scope* new_scope = CALLOC_STRUCT(Scope);
    new_scope->parent = c->cur_scope;
    hashmap_init(&new_scope->vars);
    c->cur_scope = new_scope;
}

static void pop_scope(SemaContext* c)
{
    c->cur_scope = c->cur_scope->parent;
}
