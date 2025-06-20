#include "internal.h"
#include "utils/da.h"

typedef struct SemaContext SemaContext;
struct SemaContext
{
    CompilationUnit* unit;
    Object*          cur_func;
    Scope            unit_scope;
    Scope*           cur_scope;
};

static void    analyze_function(SemaContext* c, Object* function);
static void    analyze_stmt(SemaContext* c, ASTNode* stmt);
static void    analyze_expr(SemaContext* c, ASTExpr* expr);
static void    analyze_unary(SemaContext* c, ASTExpr* expr);
// static bool    cast_allowed(SemaContext* c, TypeKind from, TypeKind to, bool explicit);
// static void    try_collapse_unary(SemaContext* c, ASTExpr* unary);
static void    declare_obj(SemaContext* c, Object* obj);
static Object* find_obj(SemaContext* c, SourceLoc* symbol);
static void    push_scope(SemaContext* c);
static void    pop_scope(SemaContext* c);

__attribute__((format(printf, 3, 4)))
static inline void sema_error(const SemaContext* c, const SourceLoc* loc, 
                              const char* restrict message, ...)
{
    va_list va;
    va_start(va, message);
    sic_error_atv(c->unit->file.full_path, loc, message, va);
    va_end(va);
}

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
        declare_obj(c, function->func.params.data[i]);

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

static void analyze_expr(SemaContext* c, ASTExpr* expr)
{
    switch(expr->kind)
    {
    case EXPR_BINARY: {
        ASTExprBinary* bin = &expr->expr.binary;
        analyze_expr(c, bin->lhs);
        analyze_expr(c, bin->rhs);
        if(bin->lhs->kind == EXPR_INVALID ||
           bin->rhs->kind == EXPR_INVALID)
        {
            expr->kind = EXPR_INVALID;
            return;
        }
        break;
    }
    case EXPR_CAST: {
        ASTExprCast* cast = &expr->expr.cast;
        analyze_expr(c, cast->expr_to_cast);
        if(cast->expr_to_cast->kind == EXPR_INVALID)
        {
            expr->kind = EXPR_INVALID;
            return;
        }
        break;
    }
    case EXPR_CONSTANT:
        return;
    case EXPR_FUNC_CALL: {
        ASTExprCall* call = &expr->expr.call;
        analyze_expr(c, call->func_expr);
        if(call->func_expr->kind == EXPR_INVALID)
        {
            expr->kind = EXPR_INVALID;
            return;
        }
        if(call->func_expr->kind != EXPR_IDENT || call->func_expr->expr.ident.obj->kind != OBJ_FUNC)
        {
            sema_error(c, &expr->loc, "Attempted to call non-function and non-function-pointer variable.");
            expr->kind = EXPR_INVALID;
            return;
        }
        break;
    }
    case EXPR_IDENT:
        SIC_ERROR_DBG("Shouldn't be here.");
        break;
    case EXPR_INVALID:
    case EXPR_NOP:
        return;
    case EXPR_PRE_SEMANTIC_IDENT: {
        ASTExprIdent* ident = &expr->expr.ident;
        ident->obj = find_obj(c, &expr->loc);
        if(ident->obj == NULL)
        {
            sema_error(c, &expr->loc, "Reference to undefined symbol \'%.*s\'.", expr->loc.len, expr->loc.start);
            expr->kind = EXPR_INVALID;
            return;
        }
        expr->type = ident->obj->var.type;
        expr->kind = EXPR_IDENT;
        break;
    }
    case EXPR_TERNARY:
        break;
    case EXPR_UNARY: {
        ASTExprUnary* unary = &expr->expr.unary;
        analyze_expr(c, unary->child);
        if(unary->child->kind == EXPR_INVALID)
        {
            expr->kind = EXPR_INVALID;
            return;
        }
        analyze_unary(c, expr);
        break;
    }
    }
}

static void analyze_unary(SemaContext* c, ASTExpr* expr)
{
    ASTExpr* child = expr->expr.unary.child;
    switch(expr->expr.unary.kind)
    {
    case UNARY_ADDR_OF:
        if(child->kind != EXPR_IDENT)
        {
            sema_error(c, &expr->loc, "Cannot take address of rvalue.");
            expr->kind = EXPR_INVALID;
            return;
        }
        expr->type = pointer_to(child->type);
        break;
    case UNARY_DEREF:
        if(child->type->kind != TYPE_POINTER)
        {
            sema_error(c, &expr->loc, "Cannot dereference non-pointer type.");
            expr->kind = EXPR_INVALID;
            return;
        }
        expr->type = child->type->pointer_base;
        break;
    case UNARY_INVALID:
        break;
    case UNARY_NEG:
        if(child->type->kind < TYPE_NUMERIC_START ||
           child->type->kind > TYPE_NUMERIC_END)
        {
            sema_error(c, &expr->loc, "Cannot negate non-numeric type.");
            expr->kind = EXPR_INVALID;
            return;
        }
        expr->type = child->type;
        break;
    }
}

// static bool cast_allowed(SemaContext* c, TypeKind from, TypeKind to, bool explicit)
// {
//
// }
// static void try_collapse_unary(SemaContext* c, ASTExpr* unary)
// {
//     ASTExpr* child = unary->expr.unary.child;
//     if(child->kind == EXPR_CONSTANT &&
//        unary->expr.unary.kind == UNARY_NEG)
//         ;
// }

static void declare_obj(SemaContext* c, Object* obj)
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

static Object* find_obj(SemaContext* c, SourceLoc* symbol)
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
