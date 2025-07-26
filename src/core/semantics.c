#include "semantics.h"
#include "utils/da.h"

static void analyze_function(SemaContext* c, Object* function);
static void analyze_stmt(SemaContext* c, ASTStmt* stmt);
static void analyze_declaration(SemaContext* c, ASTDeclaration* decl);
static void analyze_swap(SemaContext* c, ASTStmt* stmt);

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
    ObjectDA* types = &context.unit->types;
    ObjectDA* vars  = &context.unit->vars;
    hashmap_initn(&context.unit_scope.vars, funcs->size + vars->size);
    hashmap_initn(&context.unit_scope.types, types->size);
    for(size_t i = 0; i < types->size; ++i)
    {
        Object* type = types->data[i];
        if(find_obj(&context, &type->symbol) != NULL)
            sema_error(&context, &type->symbol, "Symbol redefined.");
        hashmap_putn(&context.unit_scope.types, type->symbol.start, type->symbol.len, type);
    }


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

    for(size_t i = 0; i < vars->size; ++i)
    {
        Object* var = vars->data[i];
        if(find_obj(&context, &var->symbol) != NULL)
            sema_error(&context, &var->symbol, "Symbol redefined.");
        hashmap_putn(&context.unit_scope.vars, var->symbol.start, var->symbol.len, var);
    }

    for(size_t i = 0; i < types->size; ++i)
        resolve_struct_type(&context, types->data[i], false);

    for(size_t i = 0; i < funcs->size; ++i)
        analyze_function(&context, funcs->data[i]);
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
        o = hashmap_getn(&sc->types, symbol->start, symbol->len);
        if(o != NULL)
            return o;
    }
    
    return NULL;
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
            sema_error(c, &expr->loc, "Unable to assign object \'%.*s\'.",
                       expr->expr.ident->symbol.len, expr->expr.ident->symbol.start);
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

static void analyze_function(SemaContext* c, Object* function)
{
    push_scope(c);
    c->cur_func = function;
    ObjectDA* params = &function->func.signature->params;
    for(size_t i = 0; i < params->size; ++i)
    {
        Object* param = params->data[i];
        resolve_type(c, param->var.type, false);
        hashmap_putn(&c->cur_scope->vars, param->symbol.start, param->symbol.len, param);
    }

    ASTStmt* stmt = function->func.body;
    while(stmt != NULL)
    {
        analyze_stmt(c, stmt);
        stmt = stmt->next;
    }
    pop_scope(c);
}

static void analyze_stmt(SemaContext* c, ASTStmt* stmt)
{
RETRY:
    switch(stmt->kind)
    {
    case STMT_AMBIGUOUS: {
        Type* possible_type = stmt->stmt.ambiguous;
        Object* obj = find_obj(c, &possible_type->unresolved);
        if(obj == NULL)
        {
            sema_error(c, &possible_type->unresolved, 
                       "Reference to undefined symbol \'%.*s\'",
                       possible_type->unresolved.len, possible_type->unresolved.start);
            return;
        }
        Lexer l;
        lexer_set_pos_in_unit(&l, c->unit, &possible_type->unresolved);
        if(obj->kind == OBJ_FUNC || obj->kind == OBJ_VAR)
        {
            stmt->kind = STMT_EXPR_STMT;
            stmt->stmt.expr = parse_ambiguous_expr(&l);
            analyze_expr(c, stmt->stmt.expr);
            return;
        }
        parse_ambiguous_decl(&l, stmt);
        goto RETRY;
    }
    case STMT_BLOCK: {
        push_scope(c);
        ASTStmt* sub_stmt = stmt->stmt.block.body;
        while(sub_stmt != NULL)
        {
            analyze_stmt(c, sub_stmt);
            sub_stmt = sub_stmt->next;
        }
        pop_scope(c);
        return;
    }
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
    case STMT_IF: {
        ASTIf* if_stmt = &stmt->stmt.if_;
        analyze_expr(c, if_stmt->cond);
        implicit_cast(c, if_stmt->cond, g_type_bool);
        analyze_stmt(c, if_stmt->then_stmt);
        if(if_stmt->else_stmt != NULL)
            analyze_stmt(c, if_stmt->else_stmt);
        return;
    }
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
    declare_obj(c, decl->obj);
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
}


static void push_scope(SemaContext* c)
{
    Scope* new_scope = CALLOC_STRUCT(Scope);
    new_scope->parent = c->cur_scope;
    hashmap_init(&new_scope->vars);
    hashmap_init(&new_scope->types);
    c->cur_scope = new_scope;
}

static void pop_scope(SemaContext* c)
{
    c->cur_scope = c->cur_scope->parent;
}
