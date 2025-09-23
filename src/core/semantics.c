#include "semantics.h"
#include "utils/da.h"

static void declare_type(SemaContext* c, Object* type_obj);
static void declare_function(SemaContext* c, Object* function);
static void declare_global_var(SemaContext* c, Object* var);
static void declare_global_obj(SemaContext* c, Object* global);

static void analyze_unit(SemaContext* c, CompilationUnit* unit);
static void analyze_function(SemaContext* c, Object* function);
static bool analyze_main(Object* main);
static void analyze_stmt(SemaContext* c, ASTStmt* stmt, bool add_scope);
static void analyze_stmt_block(SemaContext* c, ASTStmt* stmt);
static void analyze_break(SemaContext* c, ASTStmt* stmt);
static void analyze_continue(SemaContext* c, ASTStmt* stmt);
static void analyze_for(SemaContext* c, ASTStmt* stmt);
static void analyze_if(SemaContext* c, ASTStmt* stmt);
static void analyze_return(SemaContext* c, ASTStmt* stmt);
static void analyze_switch(SemaContext* c, ASTStmt* stmt);
static void analyze_while(SemaContext* c, ASTStmt* stmt);
static void analyze_declaration(SemaContext* c, ASTDeclaration* decl);
static void analyze_swap(SemaContext* c, ASTStmt* stmt);

static bool analyze_enum_obj(SemaContext* c, Object* type_obj);
static bool analyze_struct_obj(SemaContext* c, Object* type_obj);
static bool analyze_type_alias(SemaContext* c, Object* type_obj, ResolutionFlags flags,
                               SourceLoc err_loc, const char* err_str);
static bool analyze_union_obj(SemaContext* c, Object* type_obj);


void semantic_declaration(CompilationUnit* unit)
{
    SIC_ASSERT(unit != NULL);
    SemaContext context = {0};
    context.unit = unit;
    context.priv_syms = &unit->priv_symbols;
    context.prot_syms = &unit->module->symbols;
    for(uint32_t i = 0; i < unit->types.size; ++i)
        declare_type(&context, unit->types.data[i]);

    for(uint32_t i = 0; i < unit->funcs.size; ++i)
        declare_function(&context, unit->funcs.data[i]);

    for(uint32_t i = 0; i < unit->vars.size; ++i)
        declare_global_var(&context, unit->vars.data[i]);
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

bool expr_is_lvalue(ASTExpr* expr)
{
    switch(expr->kind)
    {
    case EXPR_ARRAY_ACCESS:
    case EXPR_MEMBER_ACCESS:
        return true;
    case EXPR_IDENT: {
        Object* o = expr->expr.ident;
        if(o->kind != OBJ_VAR)
            return false;
        switch(o->var.kind)
        {
        case VAR_LOCAL:
        case VAR_GLOBAL:
        case VAR_PARAM:
            return true;
        case VAR_INVALID:
            break;
        }
        SIC_UNREACHABLE();
    }
    case EXPR_UNARY:
        return expr->expr.unary.kind == UNARY_DEREF;
    default:
        return false;
    }
}

bool expr_is_const_eval(ASTExpr* expr)
{
    (void)expr;
    return true;
    // switch(expr->kind)
    // {
    // case EXPR_IDENT
    // }
}

static void declare_type(SemaContext* c, Object* type_obj)
{
    declare_global_obj(c, type_obj);
    TypeKind typekind;
    switch(type_obj->kind)
    {
    case OBJ_BITFIELD:
        SIC_TODO();
    case OBJ_ENUM:
        typekind = TYPE_ENUM;
        break;
    case OBJ_STRUCT:
        typekind = TYPE_STRUCT;
        break;
    case OBJ_TYPE_ALIAS:
    case OBJ_TYPE_DISTINCT:
        return;
    case OBJ_UNION:
        typekind = TYPE_UNION;
        break;
    case OBJ_INVALID:
    case OBJ_ALIAS_EXPR:
    case OBJ_ENUM_VALUE:
    case OBJ_FUNC:
    case OBJ_VAR:
        SIC_UNREACHABLE();
    }
    Type* type = type_obj->type = CALLOC_STRUCT(Type);
    type->kind = typekind;
    type->visibility = type_obj->visibility;
    type->user_def = type_obj;
}

static void declare_function(SemaContext* c, Object* function)
{
    declare_global_obj(c, function);
    FuncSignature* sig = function->func.signature;
    ObjectDA* params = &sig->params;

    if(resolve_type(c, &sig->ret_type, RES_ALLOW_VOID, function->loc, "Function cannot have return type") 
       && sig->ret_type->visibility < function->visibility)
        sic_error_at(function->loc, "Function's return type has less visibility than parent function.");

    for(uint32_t i = 0; i < params->size; ++i)
    {
        Object* param = params->data[i];
        if(!resolve_type(c, &param->type, RES_NORMAL, param->loc, "Variable cannot be of type"))
            continue;
        if(param->type->visibility < function->visibility)
        {
            sic_error_at(param->loc, "Parameter's type has less visibility than parent function.");
            continue;
        }
    }
}

static void declare_global_var(SemaContext* c, Object* var)
{
    declare_global_obj(c, var);
    resolve_type(c, &var->type, RES_NORMAL, var->loc, "Variable cannot be of type");
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


    switch(global->visibility)
    {
    case VIS_PRIVATE:
        hashmap_put(c->priv_syms, global->symbol, global);
        return;
    case VIS_PUBLIC:
        hashmap_put(pub, global->symbol, global);
        FALLTHROUGH;
    case VIS_PROTECTED:
        hashmap_put(c->prot_syms, global->symbol, global);
        return;
    }
}

static void analyze_unit(SemaContext* c, CompilationUnit* unit)
{
    c->unit = unit;
    c->priv_syms = &unit->priv_symbols;
    c->prot_syms = &unit->module->symbols;

    for(uint32_t i = 0; i < unit->types.size; ++i)
        analyze_type_obj(c, unit->types.data[i], NULL, RES_ALLOW_VOID, (SourceLoc){}, NULL);

    for(uint32_t i = 0; i < unit->vars.size; ++i)
        analyze_global_var(c, unit->vars.data[i]);

    for(uint32_t i = 0; i < unit->funcs.size; ++i)
        analyze_function(c, unit->funcs.data[i]);
}

static void analyze_function(SemaContext* c, Object* function)
{
    uint32_t scope = push_scope();
    c->cur_func = function;
    ObjectDA* params = &function->func.signature->params;
    for(uint32_t i = 0; i < params->size; ++i)
        push_obj(params->data[i]);

    analyze_stmt_block(c, function->func.body);

    c->cur_func = NULL;
    pop_scope(scope);
}

void analyze_global_var(SemaContext* c, Object* global_var)
{
    SIC_ASSERT(global_var->kind == OBJ_VAR && global_var->var.kind == VAR_GLOBAL);
    if(global_var->status == STATUS_RESOLVED)
        return;
    else if(global_var->status == STATUS_RESOLVING)
    {
    }

    global_var->status = STATUS_RESOLVING;
    if(global_var->var.initial_val == NULL || 
       !analyze_expr(c, &global_var->var.initial_val))
    {
        global_var->kind = OBJ_INVALID;
        return;
    }
    // TODO: Check that this is constant. Make a function that checks if an expression is constant,
    // because something like &global_var, is technically a constant expression.
    if(global_var->type->visibility < global_var->visibility)
        // TODO: Make this error print the actual visibility of both.
        sic_error_at(global_var->loc, "Global variable's type has less visibility than the object itself.");

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
    TypeKind rt_kind = sig->ret_type->kind;
    if(rt_kind != TYPE_INT && rt_kind != TYPE_VOID)
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
                            "The return type should be 'int' or 'void', with "
                            "optional parameters 'int, ubyte**'.");
    return false;
}

static void analyze_stmt(SemaContext* c, ASTStmt* stmt, bool add_scope)
{
    switch(stmt->kind)
    {
    case STMT_BLOCK: {
        uint32_t scope;
        if(add_scope)
            scope = push_scope();
        analyze_stmt_block(c, stmt->stmt.block.body);
        if(add_scope)
            pop_scope(scope);
        return;
    }
    case STMT_BREAK:
        analyze_break(c, stmt);
        return;
    case STMT_CONTINUE:
        analyze_continue(c, stmt);
        return;
    case STMT_EXPR_STMT:
        analyze_expr(c, &stmt->stmt.expr);
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
        for(uint32_t i = 0; i < decl_list->size; ++i)
            analyze_declaration(c, decl_list->data + i);
        return;
    }
    case STMT_NOP:
        return;
    case STMT_RETURN:
        analyze_return(c, stmt);
        return;
    case STMT_SINGLE_DECL:
        analyze_declaration(c, &stmt->stmt.single_decl);
        return;
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

static void analyze_stmt_block(SemaContext* c, ASTStmt* stmt)
{
    while(stmt != NULL)
    {
        analyze_stmt(c, stmt, true);
        stmt = stmt->next;
    }
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
    uint32_t scope = push_scope();
    if(for_stmt->init_stmt != NULL)
        analyze_stmt(c, for_stmt->init_stmt, false);
    if(for_stmt->cond_expr != NULL)
        implicit_cast(c, &for_stmt->cond_expr, g_type_bool);
    if(for_stmt->loop_expr != NULL)
        analyze_expr(c, &for_stmt->loop_expr);

    BlockContext context = c->block_context;
    c->block_context = BLOCK_LOOP;
    analyze_stmt(c, for_stmt->body, false);
    c->block_context = context;

    pop_scope(scope);
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
        c->ident_mask = IDENT_VAR_ONLY;
        implicit_cast(c, &ret->ret_expr, ret_type);
    }
    else if(ret_type->kind != TYPE_VOID)
        sic_error_at(stmt->loc, "Function returning non-void should return a value.");
}

static void analyze_switch(SemaContext* c, ASTStmt* stmt)
{
    ASTSwitch* swi = &stmt->stmt.switch_;
    bool has_default = false;
    analyze_expr(c, &swi->expr);
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

        uint32_t scope = push_scope();
        analyze_stmt_block(c, cas->body);
        pop_scope(scope);
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
    Type** type_ref = &decl->obj->type;
    c->ident_mask = IDENT_VAR_ONLY;
    if((*type_ref)->kind == TYPE_AUTO)
    {
        if(decl->init_expr == NULL)
        {
            sic_error_at(decl->obj->loc, "Declaring a variable with auto requires "
                                         "it to be initialized with an expression.");
            return;
        }
        if(!analyze_expr(c, &decl->init_expr))
            return;
        decl->obj->type = decl->init_expr->type;
    }
    else if(!resolve_type(c, type_ref, RES_NORMAL, decl->obj->loc, "Variable cannot be of type"))
        decl->obj->kind = OBJ_INVALID;
    else if(decl->init_expr != NULL)
        implicit_cast(c, &decl->init_expr, *type_ref);
    push_obj(decl->obj);
}

static void analyze_swap(SemaContext* c, ASTStmt* stmt)
{
    analyze_expr(c, &stmt->stmt.swap.left);
    analyze_expr(c, &stmt->stmt.swap.right);
    ASTExpr* left = stmt->stmt.swap.left;
    ASTExpr* right = stmt->stmt.swap.right;
    if(expr_is_bad(left) || expr_is_bad(right) ||
       !expr_ensure_lvalue(left) || !expr_ensure_lvalue(right))
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

bool analyze_type_obj(SemaContext* c, Object* type_obj, Type** o_type, 
                      ResolutionFlags flags, SourceLoc err_loc, const char* err_str)
{
    switch(type_obj->kind)
    {
    case OBJ_BITFIELD:
        SIC_TODO();
    case OBJ_ENUM:
        if(type_obj->status == STATUS_RESOLVED) break;
        if(!analyze_enum_obj(c, type_obj)) goto ERR;
        break;
    case OBJ_STRUCT:
        if(type_obj->status == STATUS_RESOLVED) break;
        if(c->in_ptr || c->in_typedef) break;
        if(!analyze_struct_obj(c, type_obj)) goto ERR;
        break;
    case OBJ_TYPE_ALIAS:
        if(!analyze_type_alias(c, type_obj, flags, err_loc, err_str)) goto ERR;
        break;
    case OBJ_TYPE_DISTINCT:
        SIC_TODO();
    case OBJ_UNION:
        if(type_obj->status == STATUS_RESOLVED) break;
        if(c->in_ptr || c->in_typedef) break;
        if(!analyze_union_obj(c, type_obj)) goto ERR;
        break;
    case OBJ_INVALID:
        return false;
    case OBJ_ALIAS_EXPR:
    case OBJ_ENUM_VALUE:
    case OBJ_FUNC:
    case OBJ_VAR:
        SIC_UNREACHABLE();
    }
    if(o_type != NULL)
        *o_type = type_obj->type;
    return true;
ERR:
    type_obj->kind = OBJ_INVALID;
    type_obj->status = STATUS_RESOLVED;
    return false;
}

static bool analyze_enum_obj(SemaContext* c, Object* type_obj)
{
    ObjEnum* enum_ = &type_obj->enum_;
    type_obj->type->status = STATUS_RESOLVED;
    type_obj->status = STATUS_RESOLVED;
    if(enum_->underlying == NULL)
        enum_->underlying = g_type_int;
    else if(!resolve_type(c, &enum_->underlying, RES_NORMAL, type_obj->loc, "An enum's underlying type cannot be of type"))
        return false;
    else if(!type_is_integer(enum_->underlying))
    {
        sic_error_at(type_obj->loc, "Expected integral underlying type for enum.");
        return false;
    }

    uint32_t scope = push_scope();
    uint64_t last_value = -1;
    for(uint32_t i = 0; i < enum_->values.size; ++i)
    {
        Object* value = enum_->values.data[i];
        value->type = enum_->underlying;
        if(value->enum_val.value == NULL)
            value->enum_val.const_val = ++last_value;
        else if(!analyze_expr(c, &value->enum_val.value))
            return false;
        else if(value->enum_val.value->kind != EXPR_CONSTANT)
        {
            sic_error_at(value->loc, "Enum value must be assigned a constant integer expression.");
            return false;
        }
        else if(!implicit_cast(c, &value->enum_val.value, enum_->underlying))
            return false;
        else
            last_value = value->enum_val.const_val = value->enum_val.value->expr.constant.val.i;
        push_obj(value);
    }
    pop_scope(scope);
    return true;
}

static bool analyze_struct_obj(SemaContext* c, Object* type_obj)
{
    if(type_obj->status == STATUS_RESOLVING)
    {
        set_circular_def(c, type_obj);
        return false;
    }
    type_obj->status = STATUS_RESOLVING;
    ObjStruct* struct_ = &type_obj->struct_;
    for(uint32_t i = 0; i < struct_->members.size; ++i)
    {
        Object* member = struct_->members.data[i];
        if(!resolve_type(c, &member->type, RES_NORMAL, member->loc, "Struct member cannot be of type"))
        {
            check_circular_def(c, member, member->loc);
            return false;
        }
        if(member->type->visibility < type_obj->visibility)
        {
            // TODO: Make this error print the actual visibility of both.
            sic_error_at(member->loc, "Member has type with less visibility than parent.");
            return false;
        }
        uint32_t align = type_alignment(member->type);
        SIC_ASSERT(is_pow_of_2(align));
        struct_->size = ALIGN_UP(struct_->size, align) + type_size(member->type);
        struct_->align = MAX(struct_->align, align);

    }
    type_obj->type->status = STATUS_RESOLVED;
    type_obj->status = STATUS_RESOLVED;
    return true;
}

static bool analyze_type_alias(SemaContext* c, Object* type_obj, ResolutionFlags flags,
                               SourceLoc err_loc, const char* err_str)
{
    switch(type_obj->status)
    {
    case STATUS_RESOLVED:
        if(!resolve_type(c, &type_obj->type, flags, err_loc, err_str))
        {
            check_circular_def(c, type_obj, type_obj->loc);
            return false;
        }
        return true;
    case STATUS_RESOLVING:
        if(c->in_typedef)
        {
            set_circular_def(c, type_obj);
            return false;
        }
        FALLTHROUGH;
    case STATUS_UNRESOLVED: {
        type_obj->status = STATUS_RESOLVING;
        bool prev = c->in_typedef;
        c->in_typedef = true;
        bool success = resolve_type(c, &type_obj->type, RES_ALLOW_VOID, type_obj->loc, "Typedef cannot be assigned to type");
        c->in_typedef = prev;
        if(!success)
        {
            check_circular_def(c, type_obj, type_obj->loc);
            return false;
        }
        type_obj->status = STATUS_RESOLVED;
        return resolve_type(c, &type_obj->type, flags, err_loc, err_str);
    }
    default:
        SIC_UNREACHABLE();
    }
}

static bool analyze_union_obj(SemaContext* c, Object* type_obj)
{
    if(type_obj->status == STATUS_RESOLVING)
    {
        set_circular_def(c, type_obj);
        return false;
    }
    type_obj->status = STATUS_RESOLVING;
    ObjStruct* struct_ = &type_obj->struct_;
    uint32_t largest_size = 0;
    for(uint32_t i = 0; i < struct_->members.size; ++i)
    {
        Object* member = struct_->members.data[i];
        if(!resolve_type(c, &member->type, RES_NORMAL, member->loc, "Union member cannot be of type"))
        {
            check_circular_def(c, member->type->user_def, member->loc);
            return false;
        }
        if(member->type->visibility < type_obj->visibility)
        {
            // TODO: Make this error print the actual visibility of both.
            sic_error_at(member->loc, "Member has type with less visibility than parent.");
            return false;
        }
        uint32_t next_size = type_size(member->type);
        if(largest_size < next_size)
        {
            largest_size = next_size;
            struct_->largest_type = member->type;
        }
    }
    type_obj->type->status = STATUS_RESOLVED;
    type_obj->status = STATUS_RESOLVED;
    return true;
}

