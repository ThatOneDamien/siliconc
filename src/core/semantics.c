#include "semantics.h"
#include "utils/da.h"

static void analyze_unit(SemaContext* c, CompilationUnit* unit);
static void analyze_function(SemaContext* c, Object* function);
static void analyze_global_var(SemaContext* c, Object* global_var);
static bool analyze_main(Object* main);
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

static void analyze_enum_obj(SemaContext* c, Object* type_obj);
static void analyze_struct_obj(SemaContext* c, Object* type_obj);
static void analyze_union_obj(SemaContext* c, Object* type_obj);
static void check_circular_def(SemaContext* c, Object* other, SourceLoc loc);

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
    for(uint32_t i = 0; i < unit->types.size; ++i)
        declare_global_obj(&context, unit->types.data[i]);

    for(uint32_t i = 0; i < unit->funcs.size; ++i)
        declare_global_obj(&context, unit->funcs.data[i]);

    for(uint32_t i = 0; i < unit->vars.size; ++i)
        declare_global_obj(&context, unit->vars.data[i]);
}

void semantic_analysis(ModulePTRDA* modules)
{
    SIC_ASSERT(modules != NULL);
    SemaContext context = {0};
    for(uint32_t i = 0; i < modules->size; ++i)
    {
        Module* mod = modules->data[i];
        for(size_t j = 0; j < mod->units.size; ++j)
        {
            analyze_unit(&context, mod->units.data[j]);
#ifdef SI_DEBUG
            if(g_args.debug_output & DEBUG_SEMA)
                print_unit(mod->units.data[j]);
#endif
        }
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
    for(uint32_t i = c->scope_stack.size - 1; i < c->scope_stack.size; --i)
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

    for(uint32_t i = 0; i < unit->types.size; ++i)
        analyze_type_obj(c, unit->types.data[i], false);

    for(uint32_t i = 0; i < unit->vars.size; ++i)
        analyze_global_var(c, unit->vars.data[i]);

    for(uint32_t i = 0; i < unit->funcs.size; ++i)
        analyze_function(c, unit->funcs.data[i]);
}

static void analyze_function(SemaContext* c, Object* function)
{
    push_scope(c);
    c->cur_func = function;
    ObjectDA* params = &function->func.signature->params;
    for(uint32_t i = 0; i < params->size; ++i)
    {
        Object* param = params->data[i];
        if(resolve_type(c, param->type))
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

static void analyze_global_var(SemaContext* c, Object* global_var)
{
    ObjVar* var = &global_var->var;
    if(var->global_initializer == NULL || 
       !implicit_cast(c, &var->global_initializer, global_var->type))
        return;
}

static bool analyze_main(Object* main)
{
    if(main->kind != OBJ_FUNC)
    {
        sic_error_at(main->loc, "Symbol 'main' is reserved for entry function.");
        return false;
    }

    if(g_compiler.main_function != NULL)
    {
        sic_error_redef(main, g_compiler.main_function);
        return false;
    }

    FuncSignature* sig = main->func.signature;
    if(sig->ret_type->kind != TYPE_INT)
        goto BAD_SIG;

    if(sig->params.size >= 1 && sig->params.data[0]->type->kind != TYPE_INT)
        goto BAD_SIG;

    Type* second;
    if(sig->params.size >= 2)
    {
        second = sig->params.data[1]->type;
        if(second->kind != TYPE_POINTER)
            goto BAD_SIG;
        second = second->pointer_base;
        if(second->kind != TYPE_POINTER)
            goto BAD_SIG;
        second = second->pointer_base;
        if(second->kind != TYPE_UBYTE)
            goto BAD_SIG;
    }

    g_compiler.main_function = main;
    return true;

BAD_SIG:
    sic_error_at(main->loc, "The signature of the main function is invalid. "
                            "The return type should be 'int', with optional "
                            "parameters 'int, ubyte**'.");
    return false;
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
        if(resolve_type(c, decl_list->data[0].obj->type))
            for(uint32_t i = 0; i < decl_list->size; ++i)
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
        if(resolve_type(c, decl->obj->type))
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
        implicit_cast(c, &for_stmt->cond_expr, g_type_bool);
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
            implicit_cast(c, &cas->expr, swi->expr->type);
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
    implicit_cast(c, &while_stmt->cond, g_type_bool);
    BlockContext context = c->block_context;
    c->block_context = BLOCK_LOOP; 
    analyze_stmt(c, while_stmt->body, true);
    c->block_context = context;
}

static void analyze_declaration(SemaContext* c, ASTDeclaration* decl)
{
    Type* type = decl->obj->type;
    if(decl->init_expr != NULL)
        implicit_cast(c, &decl->init_expr, type);
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

void analyze_type_obj(SemaContext* c, Object* type_obj, bool is_pointer)
{
    if(type_obj->status == STATUS_RESOLVED)
        return;
    if(type_obj->status == STATUS_RESOLVING)
    {
        if(is_pointer)
            return;
        sic_error_at(type_obj->loc, "Circular structure definition.");
        c->circular_def = type_obj;
        type_obj->kind = OBJ_INVALID;
        return;
    }
    type_obj->status = STATUS_RESOLVING;
    switch(type_obj->kind)
    {
    case OBJ_BITFIELD:
        SIC_TODO();
    case OBJ_ENUM:
        analyze_enum_obj(c, type_obj);
        break;
    case OBJ_STRUCT:
        analyze_struct_obj(c, type_obj);
        break;
    case OBJ_TYPE_ALIAS:
    case OBJ_TYPE_DISTINCT:
        SIC_TODO();
    case OBJ_UNION:
        analyze_union_obj(c, type_obj);
        break;
    case OBJ_INVALID:
    case OBJ_ALIAS_EXPR:
    case OBJ_ENUM_VALUE:
    case OBJ_FUNC:
    case OBJ_VAR:
        SIC_UNREACHABLE();
    }
    type_obj->status = STATUS_RESOLVED;
}

static void analyze_enum_obj(SemaContext* c, Object* type_obj)
{
    ObjEnum* enum_ = &type_obj->enum_;
    Type* enum_type = type_obj->type = CALLOC_STRUCT(Type);
    enum_type->kind = TYPE_ENUM;
    enum_type->status = STATUS_RESOLVED;
    enum_type->user_def = type_obj;
    if(enum_->underlying == NULL)
        enum_->underlying = g_type_int;
    else if(!resolve_type(c, enum_->underlying) ||
            !type_is_integer(enum_->underlying))
    {
        sic_error_at(type_obj->loc, "Expected integral underlying type for enum.");
        type_obj->kind = OBJ_INVALID;
        return;
    }

    push_scope(c);
    uint64_t last_value = -1;
    for(uint32_t i = 0; i < enum_->values.size; ++i)
    {
        Object* value = enum_->values.data[i];
        value->type = enum_->underlying;
        ASTExpr* val_expr = value->enum_val.value;
        if(val_expr == NULL)
        {
            value->enum_val.const_val = ++last_value;
        }
        else if(!analyze_expr(c, val_expr))
        {
            type_obj->kind = OBJ_INVALID;
            continue;
        }
        else if(val_expr->kind != EXPR_CONSTANT)
        {
            sic_error_at(value->loc, "Enum value must be assigned a constant integer expression.");
            type_obj->kind = OBJ_INVALID;
            continue;
        }
        else if(!implicit_cast(c, &value->enum_val.value, enum_->underlying))
        {
            type_obj->kind = OBJ_INVALID;
            continue;
        }
        else
            last_value = value->enum_val.const_val = val_expr->expr.constant.val.i;
        declare_obj(c, value);
    }
    pop_scope(c);
}

static void analyze_struct_obj(SemaContext* c, Object* type_obj)
{
    ObjStruct* struct_ = &type_obj->struct_;
    for(uint32_t i = 0; i < struct_->members.size; ++i)
    {
        Object* member = struct_->members.data[i];
        Type* next_ty = member->type;
        if(!resolve_type(c, next_ty))
        {
            check_circular_def(c, type_obj, member->loc);
            type_obj->kind = OBJ_INVALID;
            continue;
        }
        uint32_t align = type_alignment(next_ty);
        SIC_ASSERT(is_pow_of_2(align));
        struct_->size = ALIGN_UP(struct_->size, align) + type_size(next_ty);
        struct_->align = MAX(struct_->align, align);

    }
    type_obj->status = STATUS_RESOLVED;
}

static void analyze_union_obj(SemaContext* c, Object* type_obj)
{
    ObjStruct* struct_ = &type_obj->struct_;
    uint32_t largest_size = 0;
    for(uint32_t i = 0; i < struct_->members.size; ++i)
    {
        Object* member = struct_->members.data[i];
        Type* next_ty = member->type;
        if(!resolve_type(c, next_ty))
        {
            check_circular_def(c, next_ty->user_def, member->loc);
            type_obj->kind = OBJ_INVALID;
            continue;
        }
        uint32_t next_size = type_size(next_ty);
        if(largest_size < next_size)
        {
            largest_size = next_size;
            struct_->largest_type = next_ty;
        }
    }
}

static void check_circular_def(SemaContext* c, Object* other, SourceLoc loc)
{
    if(c->circular_def == other)
        c->circular_def = NULL;
    else if(c->circular_def != NULL)
        sic_diagnostic_at(loc, DIAG_NOTE, "From declaration here.");
}

static void declare_global_obj(SemaContext* c, Object* global)
{
    if(memcmp(global->symbol, "main", 4) == 0 &&
       !analyze_main(global))
        return;
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
