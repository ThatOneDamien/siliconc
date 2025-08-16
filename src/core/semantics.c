#include "semantics.h"
#include "utils/da.h"

static void analyze_unit(SemaContext* c, CompilationUnit* unit);
static void analyze_function(SemaContext* c, Object* function);
static void analyze_stmt(SemaContext* c, ASTStmt* stmt, bool add_scope);
static void analyze_stmt_block(SemaContext* c, ASTStmt* stmt, bool add_scope);
static void analyze_break(SemaContext* c, ASTStmt* stmt);
static void analyze_continue(SemaContext* c, ASTStmt* stmt);
static void analyze_for(SemaContext* c, ASTStmt* stmt);
static void analyze_if(SemaContext* c, ASTStmt* stmt);
static void analyze_return(SemaContext* c, ASTStmt* stmt);
static void analyze_switch(SemaContext* c, ASTStmt* stmt);
static void analyze_while(SemaContext* c, ASTStmt* stmt);
static void analyze_declaration(SemaContext* c, ASTDeclaration* decl);
static void analyze_swap(SemaContext* c, ASTStmt* stmt);

static void declare_global_obj(SemaContext* c, Object* global);

static inline void push_scope(SemaContext* c);
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

void declare_obj(SemaContext* c, Object* obj)
{
    // TODO: Change the way that SourceLoc works, so that it tracks file location
    //       then we can have better error handling for redefinition which would show
    //       where the previous definition is.
    Scope* cur = get_cur_scope(c);
    Object* prev;
    if((prev = hashmap_get(&cur->objs, obj->symbol)) != NULL)
        sic_error_redef(obj, prev);
    hashmap_put(&cur->objs, obj->symbol, obj);
}

Object* find_obj(SemaContext* c, Symbol symbol)
{
    Object* o;
    for(size_t i = c->scope_stack.size - 1; i < c->scope_stack.size; --i)
    {
        Scope* s = c->scope_stack.data + i;
        o = hashmap_get(&s->objs, symbol);
        if(o != NULL)
            return o;
    }
    
    o = hashmap_get(c->priv_syms, symbol);
    if(o != NULL)
        return o;
    return hashmap_get(c->prot_syms, symbol);
}

bool expr_is_lvalue(ASTExpr* expr)
{
    switch(expr->kind)
    {
    case EXPR_ARRAY_ACCESS:
    case EXPR_MEMBER_ACCESS:
        return true;
    case EXPR_IDENT:
        if(expr->expr.ident->kind != OBJ_VAR)
        {
            sic_error_at(expr->loc, "Unable to assign object \'%s\'.",
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

    sic_error_at(expr->loc, "Expression is not assignable.");
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
    push_scope(c);
    c->cur_func = function;
    ObjectDA* params = &function->func.signature->params;
    for(size_t i = 0; i < params->size; ++i)
    {
        Object* param = params->data[i];
        resolve_type(c, param->var.type, false);
        declare_obj(c, param);
    }

    ASTStmt* stmt = function->func.body;
    while(stmt != NULL)
    {
        analyze_stmt(c, stmt, true);
        stmt = stmt->next;
    }

    c->cur_func = NULL;
    pop_scope(c);
}

static void analyze_stmt(SemaContext* c, ASTStmt* stmt, bool add_scope)
{
    switch(stmt->kind)
    {
    case STMT_BLOCK:
        analyze_stmt_block(c, stmt->stmt.block.body, add_scope);
        return;
    case STMT_BREAK:
        analyze_break(c, stmt);
        return;
    case STMT_CONTINUE:
        analyze_continue(c, stmt);
        return;
    case STMT_EXPR_STMT:
        analyze_expr(c, stmt->stmt.expr);
        return;
    case STMT_FOR:
        analyze_for(c, stmt);
        return;
    case STMT_GOTO:
        SIC_TODO();
    case STMT_IF:
        analyze_if(c, stmt);
        return;
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
    case STMT_RETURN:
        analyze_return(c, stmt);
        return;
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
        analyze_switch(c, stmt);
        return;
    case STMT_TYPE_DECL:
        SIC_TODO();
    case STMT_WHILE:
        analyze_while(c, stmt);
        return;
    case STMT_INVALID:
        return;
    }
    SIC_UNREACHABLE();
}

static void analyze_stmt_block(SemaContext* c, ASTStmt* stmt, bool add_scope)
{
    if(add_scope)
        push_scope(c);
    while(stmt != NULL)
    {
        analyze_stmt(c, stmt, true);
        stmt = stmt->next;
    }
    if(add_scope)
        pop_scope(c);
}

static void analyze_break(SemaContext* c, ASTStmt* stmt)
{
    if(~c->block_context & BLOCK_BREAKABLE)
        sic_error_at(stmt->loc, "Cannot break in the current context.");
}

static void analyze_continue(SemaContext* c, ASTStmt* stmt)
{
    if(~c->block_context & BLOCK_CONTINUABLE)
        sic_error_at(stmt->loc, "Cannot continue in the current context.");
}

static void analyze_for(SemaContext* c, ASTStmt* stmt)
{
    ASTFor* for_stmt = &stmt->stmt.for_;
    push_scope(c);
    if(for_stmt->init_stmt != NULL)
        analyze_stmt(c, for_stmt->init_stmt, false);
    if(for_stmt->cond_expr != NULL)
    {
        analyze_expr(c, for_stmt->cond_expr);
        implicit_cast(c, &for_stmt->cond_expr, g_type_bool);
    }
    if(for_stmt->loop_expr != NULL)
        analyze_expr(c, for_stmt->loop_expr);

    BlockContext context = c->block_context;
    c->block_context = BLOCK_LOOP;
    analyze_stmt(c, for_stmt->body, false);
    c->block_context = context;

    pop_scope(c);
}

static void analyze_if(SemaContext* c, ASTStmt* stmt)
{
    ASTIf* if_stmt = &stmt->stmt.if_;
    analyze_expr(c, if_stmt->cond);
    implicit_cast(c, &if_stmt->cond, g_type_bool);
    analyze_stmt(c, if_stmt->then_stmt, true);
    if(if_stmt->else_stmt != NULL)
        analyze_stmt(c, if_stmt->else_stmt, true);
}

static void analyze_return(SemaContext* c, ASTStmt* stmt)
{
    ASTReturn* ret = &stmt->stmt.return_;
    Type* ret_type = c->cur_func->func.signature->ret_type;
    if(ret->ret_expr != NULL)
    {
        if(ret_type->kind == TYPE_VOID)
        {
            sic_error_at(ret->ret_expr->loc, 
                            "Function returning void should not return a value.");
            return;
        }
        analyze_expr(c, ret->ret_expr);
        implicit_cast(c, &ret->ret_expr, ret_type);
    }
    else if(ret_type->kind != TYPE_VOID)
        sic_error_at(stmt->loc, "Function returning non-void should return a value.");
}

static void analyze_switch(SemaContext* c, ASTStmt* stmt)
{
    ASTSwitch* swi = &stmt->stmt.switch_;
    bool has_default = false;
    analyze_expr(c, swi->expr);
    if(!type_is_integer(swi->expr->type))
    {
        sic_error_at(swi->expr->loc, "Switch expression must be an integer type.");
        return;
    }
    if(type_size(swi->expr->type) < 4)
        implicit_cast(c, &swi->expr, g_type_int);

    BlockContext context = c->block_context;
    c->block_context = BLOCK_SWITCH;
    for(uint32_t i = 0; i < swi->cases.size; ++i)
    {
        ASTCase* cas = swi->cases.data + i;
        if(cas->expr != NULL)
        {
            analyze_expr(c, cas->expr);
            implicit_cast(c, &cas->expr, swi->expr->type);
        }
        else if(has_default)
        {
            // TODO: Improve this error message, Im just too fucking lazy right now.
            sic_error_at(swi->expr->loc, "Switch statement contains duplicate default cases.");
            continue;
        }
        else
            has_default = true;

        analyze_stmt_block(c, cas->body, true);
    }
    c->block_context = context;
}

static void analyze_while(SemaContext* c, ASTStmt* stmt)
{
    ASTWhile* while_stmt = &stmt->stmt.while_;
    analyze_expr(c, while_stmt->cond);
    implicit_cast(c, &while_stmt->cond, g_type_bool);
    BlockContext context = c->block_context;
    c->block_context = BLOCK_LOOP; 
    analyze_stmt(c, while_stmt->body, true);
    c->block_context = context;
}

static void analyze_declaration(SemaContext* c, ASTDeclaration* decl)
{
    ASTExpr* init = decl->init_expr;
    Type* type = decl->obj->var.type;
    if(decl->init_expr != NULL)
    {
        if(decl->init_expr->kind == EXPR_DEFAULT)
            resolve_default(init, type);
        else
            analyze_expr(c, init);
        implicit_cast(c, &decl->init_expr, type);
    }
    declare_obj(c, decl->obj);
}

static void analyze_swap(SemaContext* c, ASTStmt* stmt)
{
    ASTExpr* left = stmt->stmt.swap.left;
    ASTExpr* right = stmt->stmt.swap.right;
    analyze_expr(c, left);
    analyze_expr(c, right);
    if(expr_is_bad(left) || expr_is_bad(right) || 
       !expr_is_lvalue(left) || !expr_is_lvalue(right))
        return;

    if(!type_equal(left->type, right->type))
    {
        sic_error_at(stmt->loc, 
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
        sic_error_redef(global, other);

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
