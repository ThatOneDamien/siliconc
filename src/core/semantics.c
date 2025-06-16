#include "internal.h"

typedef struct SemaContext SemaContext;
struct SemaContext
{
    CompilationUnit* unit;
    Scope            unit_scope;
    Scope*           cur_scope;
};

static void    analyze_function(SemaContext* c, Object* function);
static Object* find_obj(SemaContext* c, SourceLoc* symbol);
static void    push_scope(SemaContext* c);
static void    pop_scope(SemaContext* c);

void semantic_analysis(CompilationUnit* unit)
{
    SIC_ASSERT(unit != NULL);
    SemaContext context;
    context.unit = unit;
    context.unit_scope.parent = NULL;
    context.cur_scope = &context.unit_scope;

    ObjectDA* funcs = &context.unit->funcs;
    hashmap_initn(&context.unit_scope.vars, funcs->size);
    // for(size_t i = 0; i < funcs->size; ++i)
    // {
    //     // For now, I only allow one declaration/definition of a function
    //     // In the future I will support multiple declarations as long as the
    //     // signatures match, or possibly even add function overloading, though
    //     // I'm not sure I want that.
    //     if(find_obj)
    // }
}

static void UNUSED analyze_function(SemaContext* c, Object* function)
{
    (void)c;
    (void)function;
}


static UNUSED Object* find_obj(SemaContext* c, SourceLoc* symbol)
{
    for(Scope* sc = c->cur_scope; sc != NULL; sc = sc->parent)
    {
        Object* o = hashmap_getn(&c->cur_scope->vars, symbol->start, symbol->len);
        if(o != NULL)
            return o;
    }
    return NULL;
}

static void UNUSED push_scope(SemaContext* c)
{
    Scope* new_scope = MALLOC_STRUCT(Scope);
    new_scope->parent = c->cur_scope;
    hashmap_init(&new_scope->vars);
    c->cur_scope = new_scope;
}

static void UNUSED pop_scope(SemaContext* c)
{
    c->cur_scope = c->cur_scope->parent;
}
