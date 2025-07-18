#include "debug.h"
#include "core/core.h"
#include "core/internal.h"

#ifdef SI_DEBUG
#define PRINT_DEPTH(depth) do { for(int i = 0; i < (int)(depth); ++i) printf("  "); } while(0)

static const char* s_stmt_type_strs[];
static const char* s_binary_op_strs[];
static const char* s_unary_op_strs[];
static const char* s_access_strs[];
static void print_func(const Object* func);
static void print_stmt(const ASTStmt* stmt, int depth, bool print_depth);
static void print_expr(const ASTExpr* expr, int depth, bool print_depth);
static void print_constant(const ASTExpr* expr);

static inline const char* debug_type_to_str(Type* type)
{
    if(type == NULL)
        return "Unknown";
    if(type->kind == TYPE_PRE_SEMA_ARRAY)
        return str_format("Pre-Sema-Array %s[]", type_to_string(type->array.elem_type));
    return type_to_string(type);
}

void print_all_tokens(Lexer lexer)
{
    while(lexer_advance(&lexer))
    {
        Token* tok = lexer.la_buf.buf + lexer.la_buf.head;
        printf("%-15s: Len: %-4u   %.*s\n", 
               tok_kind_to_str(tok->kind),
               tok->loc.len,
               (int)tok->loc.len, 
               tok->loc.start);
    }
}

void print_unit(const CompilationUnit* unit)
{
    SIC_ASSERT(unit != NULL);
    printf("Compilation Unit: \'%s\' (%zu Funcs, %zu Global Vars)\n", unit->file.full_path, unit->funcs.size, unit->vars.size);
    for(size_t i = 0; i < unit->funcs.size; ++i)
        print_func(unit->funcs.data[i]);
}

static void print_func(const Object* func)
{
    const ObjFunc* comps = &func->func;
    const FuncSignature* sig = comps->signature;
    printf("Function \'%.*s\' %s (returns %s):\n", 
           (int)func->symbol.len,
           func->symbol.start,
           s_access_strs[func->access],
           debug_type_to_str(sig->ret_type));
    printf("  Params (count: %lu):\n", sig->params.size);
    for(size_t i = 0; i < sig->params.size; ++i)
    {
        Object* param = sig->params.data[i];
        printf("    %.*s (type %s)\n", 
               (int)param->symbol.len, 
               param->symbol.start,
               debug_type_to_str(param->var.type));
    }

    printf("  Local Variables:\n");
    for(size_t i = 0; i < comps->local_objs.size; ++i)
    {
        Object* local = comps->local_objs.data[i];
        printf("    %.*s (type %s)\n", 
               (int)local->symbol.len, 
               local->symbol.start,
               debug_type_to_str(local->var.type));
    }

    printf("  Body:\n");
    ASTStmt* cur_stmt = comps->body;
    while(cur_stmt != NULL)
    {
        print_stmt(cur_stmt, 2, true);
        cur_stmt = cur_stmt->next;
    }
    printf("\n");
}

static void print_stmt(const ASTStmt* stmt, int depth, bool print_depth)
{
    if(stmt == NULL)
        return;

    if(print_depth)
        PRINT_DEPTH(depth);
    printf("( %s )\n", s_stmt_type_strs[stmt->kind]);
    switch(stmt->kind)
    {
    case STMT_AMBIGUOUS:
        return;
    case STMT_BLOCK: {
        ASTStmt* cur = stmt->stmt.block.body;
        while(cur != NULL)
        {
            print_stmt(cur, depth + 1, true);
            cur = cur->next;
        }
        return;
    }
    case STMT_EXPR_STMT:
        print_expr(stmt->stmt.expr, depth + 1, true);
        return;
    case STMT_IF:
        PRINT_DEPTH(depth + 1);
        printf("cond: ");
        print_expr(stmt->stmt.if_.cond, depth + 1, false);
        PRINT_DEPTH(depth + 1);
        printf("then: ");
        print_stmt(stmt->stmt.if_.then_stmt, depth + 1, false);
        PRINT_DEPTH(depth + 1);
        printf("else: ");
        print_stmt(stmt->stmt.if_.else_stmt, depth + 1, false);
        return;
    case STMT_MULTI_DECL: {
        const ASTDeclDA* decls = &stmt->stmt.multi_decl;
        for(size_t i = 0; i < decls->size; ++i)
        {
            PRINT_DEPTH(depth + 1);
            printf("%.*s = ", decls->data[i].obj->symbol.len, decls->data[i].obj->symbol.start);
            print_expr(decls->data[i].init_expr, depth + 1, false);
        }
        return;
    }
    case STMT_RETURN:
        print_expr(stmt->stmt.return_.ret_expr, depth + 1, true);
        return;
    case STMT_SINGLE_DECL: {
        const ASTDeclaration* decl = &stmt->stmt.single_decl;
        PRINT_DEPTH(depth + 1);
        printf("%.*s = ", decl->obj->symbol.len, decl->obj->symbol.start);
        if(decl->init_expr != NULL)
            print_expr(decl->init_expr, depth + 1, false);
        else
            printf("( Uninitialized )\n");
        return;
    }
    case STMT_TYPE_DECL:
        SIC_TODO();
    case STMT_WHILE:
        PRINT_DEPTH(depth + 1);
        printf("cond: ");
        print_expr(stmt->stmt.while_.cond, depth + 1, false);
        PRINT_DEPTH(depth + 1);
        printf("body: ");
        print_stmt(stmt->stmt.while_.body, depth + 1, false);
        return;
    case STMT_INVALID:
        break;
    }
    SIC_UNREACHABLE();
}

static void print_expr(const ASTExpr* expr, int depth, bool print_depth)
{
    if(expr == NULL)
        return;
    if(print_depth)
        PRINT_DEPTH(depth);
    printf("[ ");
    const SourceLoc* loc = &expr->loc;
    switch(expr->kind)
    {
    case EXPR_ARRAY_ACCESS:
        printf("ARRAY ACCESS ] (Type: %s)\n", debug_type_to_str(expr->type));
        print_expr(expr->expr.array_access.array_expr, depth + 1, true);
        print_expr(expr->expr.array_access.index_expr, depth + 1, true);
        return;
    case EXPR_BINARY:
        printf("BINARY \'%s\' ] (Type: %s)\n", s_binary_op_strs[expr->expr.binary.kind], 
               debug_type_to_str(expr->type));
        print_expr(expr->expr.binary.lhs, depth + 1, true);
        print_expr(expr->expr.binary.rhs, depth + 1, true);
        return;
    case EXPR_CAST:
        printf("Cast ] (Type: %s)\n", debug_type_to_str(expr->type));
        print_expr(expr->expr.cast.expr_to_cast, depth + 1, true);
        return;
    case EXPR_CONSTANT:
        print_constant(expr);
        return;
    case EXPR_FUNC_CALL:
        printf("Call ] (Type: %s)\n", debug_type_to_str(expr->type));
        print_expr(expr->expr.call.func_expr, depth + 1, true);
        for(size_t i = 0; i < expr->expr.call.args.size; ++i)
            print_expr(expr->expr.call.args.data[i], depth + 1, true);
        return;
    case EXPR_IDENT: {
        Object* obj = expr->expr.ident;
        switch(obj->kind)
        {
        case OBJ_VAR:
            printf("Variable \'%.*s\' ] (Type: %s)\n", loc->len, loc->start, 
                   debug_type_to_str(expr->type));
            return;
        case OBJ_FUNC:
            printf("Function \'%.*s\' ]\n", loc->len, loc->start);
            return;
        default:
            SIC_UNREACHABLE();
        }
    }
    case EXPR_INVALID:
        printf("Invalid ]\n");
        return;
    case EXPR_MEMBER_ACCESS:
        printf("Member Access ] (Type: %s)\n", debug_type_to_str(expr->type));
        print_expr(expr->expr.member_access.parent_expr, depth + 1, true);
        return;
    case EXPR_NOP:
        printf("Nop ]\n");
        return;
    case EXPR_PRE_SEMANTIC_IDENT: {
        printf("Pre-Sema Identifier \'%.*s\' ] (Type: %s)\n", loc->len, loc->start, debug_type_to_str(expr->type));
        return;
    }
    case EXPR_TERNARY:
        SIC_TODO();
    case EXPR_UNARY:
        printf("Unary \'%s\' ] (Type: %s)\n", s_unary_op_strs[expr->expr.unary.kind],
               debug_type_to_str(expr->type));
        print_expr(expr->expr.unary.child, depth + 1, true);
        return;
    case EXPR_UNRESOLVED_ACCESS:
        printf("Unresolved Access ]\n");
        print_expr(expr->expr.unresolved_access.parent_expr, depth + 1, true);
        print_expr(expr->expr.unresolved_access.member_expr, depth + 1, true);
        return;
    }
    SIC_UNREACHABLE();
}

static void print_constant(const ASTExpr* expr)
{
    const ASTExprConstant* constant = &expr->expr.constant;
    switch(constant->kind)
    {
    case CONSTANT_INVALID:
        printf("Invalid Constant ]\n");
        break;
    case CONSTANT_INTEGER:
        printf("Constant Integer val: %lu hex: 0x%lX] (Type: %s)\n",
               constant->val.i, constant->val.i, debug_type_to_str(expr->type));
        break;
    case CONSTANT_BOOL:
        printf("Constant Boolean val: %s ]\n",
               constant->val.i ? "true" : "false");
        break;
    case CONSTANT_FLOAT:
        printf("Constant Float val: %lf ] (Type: %s)\n",
               constant->val.f, debug_type_to_str(expr->type));
        break;
    case CONSTANT_STRING:
        printf("Constant String \'%.*s\' ]\n", 
               expr->loc.len, expr->loc.start);
               
        break;
    }
}

static const char* s_stmt_type_strs[] = {
    [STMT_INVALID]     = "Invalid",
    [STMT_AMBIGUOUS]   = "Ambiguous Statement",
    [STMT_BLOCK]       = "Block",
    [STMT_IF]          = "If Statement",
    [STMT_SINGLE_DECL] = "Single Declaration",
    [STMT_MULTI_DECL]  = "Multi Declaration",
    [STMT_EXPR_STMT]   = "Expression Statement",
    [STMT_RETURN]      = "Return Statement",
    [STMT_TYPE_DECL]   = "Type Declaration",
    [STMT_WHILE]       = "While Statement",
};

static const char* s_binary_op_strs[] = {
    [BINARY_INVALID]        = "Invalid",
    [BINARY_ADD]            = "Addition",
    [BINARY_SUB]            = "Subtraction",
    [BINARY_MUL]            = "Multiplication",
    [BINARY_DIV]            = "Division",
    [BINARY_MOD]            = "Modulo",
    [BINARY_LOG_OR]         = "Log Or",
    [BINARY_LOG_AND]        = "Log And",
    [BINARY_EQ]             = "Equal",
    [BINARY_NE]             = "Not Equal",
    [BINARY_LT]             = "Less Than",
    [BINARY_LE]             = "Less Than Or Equal",
    [BINARY_GT]             = "Greater Than",
    [BINARY_GE]             = "Greater Than Or Equal",
    [BINARY_SHL]            = "Shift Left",
    [BINARY_LSHR]           = "Logical Shift Right",
    [BINARY_ASHR]           = "Arithmetic Shift Right",
    [BINARY_BIT_OR]         = "Bit Or",
    [BINARY_BIT_XOR]        = "Bit Xor",
    [BINARY_BIT_AND]        = "Bit And",
    [BINARY_ASSIGN]         = "Assign",
    [BINARY_ADD_ASSIGN]     = "Add and Assign",
    [BINARY_SUB_ASSIGN]     = "Sub and Assign",
    [BINARY_MUL_ASSIGN]     = "Mul and Assign",
    [BINARY_DIV_ASSIGN]     = "Div and Assign",
    [BINARY_MOD_ASSIGN]     = "Mod and Assign",
    [BINARY_BIT_OR_ASSIGN]  = "Bit Or and Assign",
    [BINARY_BIT_XOR_ASSIGN] = "Bit Xor and Assign",
    [BINARY_BIT_AND_ASSIGN] = "Bit And and Assign",
    [BINARY_SHL_ASSIGN]     = "Shift Left and Assign",
    [BINARY_LSHR_ASSIGN]    = "Logical Shift Right and Assign",
    [BINARY_ASHR_ASSIGN]    = "Arithmetic Shift Right and Assign",
};

static const char* s_unary_op_strs[] = {
    [UNARY_INVALID] = "Invalid",
    [UNARY_ADDR_OF] = "Address Of",
    [UNARY_DEREF]   = "Deref",
    [UNARY_NEG]     = "Negate",
};

static const char* s_access_strs[] = {
    [ACCESS_PUBLIC]     = "public",
    [ACCESS_PROTECTED]  = "protected",
    [ACCESS_PRIVATE]    = "private",
};

#endif // SI_DEBUG
