#include "semantics.h"
#include "utils/da.h"

static void analyze_unit(SemaContext* c, CompilationUnit* unit);
static void analyze_function(SemaContext* c, Object* function);
static void analyze_stmt(SemaContext* c, ASTStmt* stmt);
static void analyze_declaration(SemaContext* c, ASTDeclaration* decl);
static void analyze_swap(SemaContext* c, ASTStmt* stmt);

static void declare_global_obj(SemaContext* c, Object* global);

static inline void push_scope(SemaContext* c, Scope* scope);
static inline void pop_scope(SemaContext* c);

void semantic_declaration(CompilationUnit* unit)
{
    SIC_ASSERT(unit != NULL);
    SemaContext context = {0};
    context.unit = unit;
    context.priv_syms = &unit->priv_symbols;
    context.prot_syms = &unit->module->symbols;
    for(size_t i = 0; i < unit->types.size; ++i)
        declare_global_obj(&context, unit->types.data[i]);

    for(size_t i = 0; i < unit->funcs.size; ++i)
        declare_global_obj(&context, unit->funcs.data[i]);

    for(size_t i = 0; i < unit->vars.size; ++i)
        declare_global_obj(&context, unit->vars.data[i]);
}

void semantic_analysis(ModulePTRDA* modules)
{
    SIC_ASSERT(modules != NULL);
    SemaContext context = {0};
    for(size_t i = 0; i < modules->size; ++i)
    {
        Module* mod = modules->data[i];
        for(size_t j = 0; j < mod->units.size; ++j)
            analyze_unit(&context, mod->units.data[j]);
    }
}

void declare_obj(SemaContext* c, HashMap* map, Object* obj)
{
    // TODO: Change the way that SourceLoc works, so that it tracks file location
    //       then we can have better error handling for redefinition which would show
    //       where the previous definition is.
    if(hashmap_get(map, obj->symbol) != NULL)
        sema_error(c, &obj->loc, "Redefinition of symbol \'%s\'.", obj->symbol);
    hashmap_put(map, obj->symbol, obj);
}

Object* find_obj(SemaContext* c, Symbol symbol)
{
    Object* o;
    for(Scope* sc = c->cur_scope; sc != NULL; sc = sc->parent)
    {
        o = hashmap_get(&sc->vars, symbol);
        if(o != NULL)
            return o;
        o = hashmap_get(&sc->types, symbol);
        if(o != NULL)
            return o;
    }
    
    o = hashmap_get(c->priv_syms, symbol);
    if(o != NULL)
        return o;
    return hashmap_get(c->prot_syms, symbol);
}

bool expr_is_lvalue(SemaContext* c, ASTExpr* expr)
{
    switch(expr->kind)
    {
    case EXPR_ARRAY_ACCESS:
    case EXPR_MEMBER_ACCESS:
        return true;
    case EXPR_IDENT:
        if(expr->expr.ident->kind != OBJ_VAR)
        {
            sema_error(c, &expr->loc, "Unable to assign object \'%s\'.",
                       expr->expr.ident->symbol);
            return false;
        }
        return true;
    case EXPR_UNARY:
        if(expr->expr.unary.kind != UNARY_DEREF)
            break;
        return true;
    default:
        break;
    }

    sema_error(c, &expr->loc, "Expression is not assignable.");
    return false;
}

static void analyze_unit(SemaContext* c, CompilationUnit* unit)
{
    c->unit = unit;
    c->priv_syms = &unit->priv_symbols;
    c->prot_syms = &unit->module->symbols;
    for(size_t i = 0; i < unit->funcs.size; ++i)
        analyze_function(c, unit->funcs.data[i]);
}

static void analyze_function(SemaContext* c, Object* function)
{
    hashmap_clear(&c->func_scope.vars);
    hashmap_clear(&c->func_scope.types);
    c->cur_scope = &c->func_scope;
    c->cur_func = function;
    ObjectDA* params = &function->func.signature->params;
    for(size_t i = 0; i < params->size; ++i)
    {
        Object* param = params->data[i];
        resolve_type(c, param->var.type, false);
        declare_var(c, param);
    }

    ASTStmt* stmt = function->func.body;
    while(stmt != NULL)
    {
        analyze_stmt(c, stmt);
        stmt = stmt->next;
    }

    c->cur_func = NULL;
    c->cur_scope = NULL;
}

static void analyze_stmt(SemaContext* c, ASTStmt* stmt)
{
    switch(stmt->kind)
    {
    case STMT_BLOCK: {
        Scope new_scope = {0};
        push_scope(c, &new_scope);
        ASTStmt* sub_stmt = stmt->stmt.block.body;
        while(sub_stmt != NULL)
        {
            analyze_stmt(c, sub_stmt);
            sub_stmt = sub_stmt->next;
        }
        pop_scope(c);
        return;
    }
    case STMT_BREAK:
    case STMT_CASE:
    case STMT_CONTINUE:
        SIC_TODO();
    case STMT_EXPR_STMT:
        analyze_expr(c, stmt->stmt.expr);
        return;
    case STMT_FOR: {
        ASTFor* for_stmt = &stmt->stmt.for_;
        if(for_stmt->init_stmt != NULL)
            analyze_stmt(c, for_stmt->init_stmt);
        if(for_stmt->cond_expr != NULL)
        {
            analyze_expr(c, for_stmt->cond_expr);
            implicit_cast(c, for_stmt->cond_expr, g_type_bool);
        }
        if(for_stmt->loop_expr != NULL)
            analyze_expr(c, for_stmt->loop_expr);
        analyze_stmt(c, for_stmt->body);
        return;
    }
    case STMT_GOTO:
        SIC_TODO();
    case STMT_IF: {
        ASTIf* if_stmt = &stmt->stmt.if_;
        analyze_expr(c, if_stmt->cond);
        implicit_cast(c, if_stmt->cond, g_type_bool);
        analyze_stmt(c, if_stmt->then_stmt);
        if(if_stmt->else_stmt != NULL)
            analyze_stmt(c, if_stmt->else_stmt);
        return;
    }
    case STMT_LABEL:
        SIC_TODO();
    case STMT_MULTI_DECL: {
        ASTDeclDA* decl_list = &stmt->stmt.multi_decl;
        if(resolve_type(c, decl_list->data[0].obj->var.type, false))
            for(size_t i = 0; i < decl_list->size; ++i)
                analyze_declaration(c, decl_list->data + i);
        return;
    }
    case STMT_NOP:
        return;
    case STMT_RETURN: {
        Type* ret_type = c->cur_func->func.signature->ret_type;
        if(stmt->stmt.return_.ret_expr != NULL)
        {
            if(ret_type->kind == TYPE_VOID)
            {
                sema_error(c, &stmt->stmt.return_.ret_expr->loc,
                           "Function returning void should not have expression in return statement.");
                return;
            }
            analyze_expr(c, stmt->stmt.return_.ret_expr);
            implicit_cast(c, stmt->stmt.return_.ret_expr, ret_type);
        }
        else if(ret_type->kind != TYPE_VOID)
        {
            sema_error(c, &stmt->loc,
                       "Function returning non-void should have expression in return statement.");
        }
        return;
    }
    case STMT_SINGLE_DECL: {
        ASTDeclaration* decl = &stmt->stmt.single_decl;
        if(resolve_type(c, decl->obj->var.type, false))
            analyze_declaration(c, decl);
        return;
    }
    case STMT_SWAP:
        analyze_swap(c, stmt);
        return;
    case STMT_SWITCH:
    case STMT_TYPE_DECL:
        SIC_TODO();
    case STMT_WHILE: {
        ASTWhile* while_stmt = &stmt->stmt.while_;
        analyze_expr(c, while_stmt->cond);
        implicit_cast(c, while_stmt->cond, g_type_bool);
        analyze_stmt(c, while_stmt->body);
        return;
    }
    case STMT_INVALID:
        return;
    }
    SIC_UNREACHABLE();
}

static void analyze_declaration(SemaContext* c, ASTDeclaration* decl)
{
    declare_var(c, decl->obj);
    if(decl->init_expr != NULL)
    {
        analyze_expr(c, decl->init_expr);
        implicit_cast(c, decl->init_expr, decl->obj->var.type);
    }

}

static void analyze_swap(SemaContext* c, ASTStmt* stmt)
{
    ASTExpr* left = stmt->stmt.swap.left;
    ASTExpr* right = stmt->stmt.swap.right;
    analyze_expr(c, left);
    analyze_expr(c, right);
    if(left->kind == EXPR_INVALID || right->kind == EXPR_INVALID ||
       !expr_is_lvalue(c, left) || !expr_is_lvalue(c, right))
        return;

    if(!type_equal(left->type, right->type))
    {
        sema_error(c, &stmt->loc, 
                   "Operands of swap statement have mismatched types \'%s\' and \'%s\'",
                   type_to_string(left->type), type_to_string(right->type));
    }

    if(!type_is_trivially_copyable(left->type))
    {
        ObjFunc* func = &c->cur_func->func;
        func->swap_stmt_size = MAX(func->swap_stmt_size, type_size(left->type));
        func->swap_stmt_align = MAX(func->swap_stmt_align, type_alignment(left->type));
    }
}

static void declare_global_obj(SemaContext* c, Object* global)
{
    HashMap* pub  = &c->unit->module->public_symbols;
    Object* other = hashmap_get(c->priv_syms, global->symbol);
    if(other == NULL)
        other = hashmap_get(c->prot_syms, global->symbol);

    if(other != NULL)
        sema_error(c, &global->loc, "Redefinition of symbol \'%s\'.", global->symbol);

    switch(global->access)
    {
    case ACCESS_PRIVATE:
        hashmap_put(c->priv_syms, global->symbol, global);
        return;
    case ACCESS_PUBLIC:
        hashmap_put(pub, global->symbol, global);
        FALLTHROUGH;
    case ACCESS_PROTECTED:
        hashmap_put(c->prot_syms, global->symbol, global);
        return;
    }
}

static void push_scope(SemaContext* c, Scope* scope)
{
    scope->parent = c->cur_scope;
    c->cur_scope = scope;
}

static void pop_scope(SemaContext* c)
{
    c->cur_scope = c->cur_scope->parent;
}
