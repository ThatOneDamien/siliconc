#include "core/internal.h"
#include "file_utils.h"

#define HL_RED    "\033[31m"
#define HL_YELLOW "\033[33m"
#define HL_GREEN  "\033[32m"
#define HL_BLUE   "\033[34m"
#define HL_STOP   "\033[0m"

#ifdef SI_DEBUG
#define PRINT_DEPTH(depth) do { for(int i = 0; i < (int)(depth); ++i) printf("  "); } while(0)

static const char* s_stmt_type_strs[];
static const char* s_binary_op_strs[];
static const char* s_unary_op_strs[];
static const char* s_vis_strs[];

static void print_stmt_at_depth(const ASTStmt* stmt, int depth, const char* name, bool allow_unresolved);
static void print_expr_at_depth(const ASTExpr* expr, int depth, const char* name, bool allow_unresolved);
static void print_declaration(const ObjVar* decl, int depth, bool allow_unresolved);
static void print_constant(const ASTExpr* expr, bool allow_unresolved);
static inline const char* debug_type_to_str(Type* type, bool allow_unresolved);

void print_token(const Token* tok)
{
    const char* repr;
    switch(tok->kind)
    {
    case TOKEN_IDENT:
    case TOKEN_ATTRIBUTE_IDENT:
        repr = tok->sym;
        break;
    case TOKEN_BIN_INT_LITERAL:
    case TOKEN_OCT_INT_LITERAL:
    case TOKEN_DEC_INT_LITERAL:
    case TOKEN_HEX_INT_LITERAL:
    case TOKEN_FLOAT_LITERAL:
    case TOKEN_CHAR_LITERAL:
    case TOKEN_STRING_LITERAL:
        scratch_clear();
        scratch_appendn(tok->start, tok->loc.len);
        repr = scratch_string();
        break;
    default:
        repr = tok_kind_to_str(tok->kind);
        break;
    }
    // FIXME: Fix this 
    printf("(%3d) %-15s: Line: %-6u Col: %-4u Len: %-4u \n", 
           tok->kind,
           repr,
           tok->line_col.line,
           tok->line_col.col,
           tok->loc.len);
}


void print_module(const ObjModule* module, bool allow_unresolved)
{
    DBG_ASSERT(module != NULL);
    scratch_clear();
    scratch_append_module_path(module);
    printf("Module: \'%s\' (%u Funcs, %u Global Vars)\n", 
           scratch_string(), module->funcs.size, module->vars.size);
    for(uint32_t i = 0; i < module->funcs.size; ++i)
        print_func(module->funcs.data[i], allow_unresolved);
    for(uint32_t i = 0; i < module->vars.size; ++i)
        print_global_var(module->vars.data[i], allow_unresolved);
}

void print_type_obj(const Object* obj)
{
    DBG_ASSERT(obj_is_type(obj));

}

void print_func(const ObjFunc* func, bool allow_unresolved)
{
    const FuncSignature* sig = &func->signature;
    printf("%s Function \'%s\' (returns %s):\n", 
           s_vis_strs[func->header.visibility],
           func->header.symbol,
           debug_type_to_str(sig->ret_type.type, allow_unresolved));
    printf("  Params (count: %u):\n", sig->params.size);
    for(uint32_t i = 0; i < sig->params.size; ++i)
    {
        ObjVar* param = sig->params.data[i];
        printf("    %s (type %s)\n", 
               param->header.symbol,
               debug_type_to_str(param->type_loc.type, allow_unresolved));
    }

    printf("  Body:\n");
    ASTStmt* cur_stmt = func->body;
    while(cur_stmt != NULL)
    {
        print_stmt_at_depth(cur_stmt, 2, NULL, allow_unresolved);
        cur_stmt = cur_stmt->next;
    }
    printf("\n");
}

void print_global_var(const ObjVar* var, bool allow_unresolved)
{
    printf("%s Global Var \'%s\' (Type: %s)\n",
           s_vis_strs[var->header.visibility],
           var->header.symbol,
           debug_type_to_str(var->type_loc.type, allow_unresolved));
}

void print_stmt(const ASTStmt* stmt, bool allow_unresolved)
{
    print_stmt_at_depth(stmt, 0, NULL, allow_unresolved);
}

void print_expr(const ASTExpr* expr, bool allow_unresolved)
{
    print_expr_at_depth(expr, 0, NULL, allow_unresolved);
}

static void print_stmt_at_depth(const ASTStmt* stmt, int depth, const char* name, bool allow_unresolved)
{
    if(stmt == NULL)
        return;

    PRINT_DEPTH(depth);
    if(name != NULL)
        printf("%s: ", name);
    printf("( %s )\n", s_stmt_type_strs[stmt->kind]);
    switch(stmt->kind)
    {
    case STMT_BLOCK: {
        ASTStmt* cur = stmt->stmt.block.body;
        while(cur != NULL)
        {
            print_stmt_at_depth(cur, depth + 1, NULL, allow_unresolved);
            cur = cur->next;
        }
        return;
    }
    case STMT_BREAK:
    case STMT_CONTINUE:
        return;
    case STMT_DECLARATION:
        print_declaration(stmt->stmt.declaration, depth + 1, allow_unresolved);
        return;
    case STMT_EXPR_STMT:
        print_expr_at_depth(stmt->stmt.expr, depth + 1, NULL, allow_unresolved);
        return;
    case STMT_FOR:
        SIC_TODO();
        return;
    case STMT_IF:
        print_expr_at_depth(stmt->stmt.if_.cond, depth + 1, "cond", allow_unresolved);
        print_stmt_at_depth(stmt->stmt.if_.then_stmt, depth + 1, "then", allow_unresolved);
        print_stmt_at_depth(stmt->stmt.if_.else_stmt, depth + 1, "else", allow_unresolved);
        return;
    case STMT_NOP:
        return;
    case STMT_RETURN:
        print_expr_at_depth(stmt->stmt.return_.ret_expr, depth + 1, NULL, allow_unresolved);
        return;
    case STMT_SWAP:
        print_expr_at_depth(stmt->stmt.swap.left,  depth + 1, NULL, allow_unresolved);
        print_expr_at_depth(stmt->stmt.swap.right, depth + 1, NULL, allow_unresolved);
        return;
    case STMT_SWITCH:
        return;
    case STMT_WHILE:
        print_expr_at_depth(stmt->stmt.while_.cond, depth + 1, "cond", allow_unresolved);
        print_stmt_at_depth(stmt->stmt.while_.body, depth + 1, "body", allow_unresolved);
        return;
    case STMT_CT_ASSERT:
    case STMT_CT_UNREACHABLE:
        SIC_TODO();
    case STMT_INVALID:
        break;
    }
    SIC_UNREACHABLE();
}

static void print_expr_at_depth(const ASTExpr* expr, int depth, const char* name, bool allow_unresolved)
{
    if(expr == NULL)
        return;

    PRINT_DEPTH(depth);
    if(name != NULL)
        printf("%s: ", name);
    printf("[ ");
    switch(expr->kind)
    {
    case EXPR_ARRAY_ACCESS:
        printf("Array Access ] (Type: %s)\n", debug_type_to_str(expr->type, allow_unresolved));
        print_expr_at_depth(expr->expr.array_access.array_expr, depth + 1, NULL, allow_unresolved);
        print_expr_at_depth(expr->expr.array_access.index_expr, depth + 1, NULL, allow_unresolved);
        return;
    case EXPR_ARRAY_INIT_LIST:
        printf("Array init list ]\n");
        return;
    case EXPR_BINARY:
        printf("Binary \'%s\' ] (Type: %s)\n", s_binary_op_strs[expr->expr.binary.kind], 
               debug_type_to_str(expr->type, allow_unresolved));
        print_expr_at_depth(expr->expr.binary.lhs, depth + 1, NULL, allow_unresolved);
        print_expr_at_depth(expr->expr.binary.rhs, depth + 1, NULL, allow_unresolved);
        return;
    case EXPR_CAST:
        printf("Cast ] (Type: %s)\n", debug_type_to_str(expr->type, allow_unresolved));
        print_expr_at_depth(expr->expr.cast.inner, depth + 1, NULL, allow_unresolved);
        return;
    case EXPR_CONSTANT:
        print_constant(expr, allow_unresolved);
        return;
    case EXPR_FUNC_CALL:
        printf("Call ] (Type: %s)\n", debug_type_to_str(expr->type, allow_unresolved));
        print_expr_at_depth(expr->expr.call.func_expr, depth + 1, NULL, allow_unresolved);
        for(uint32_t i = 0; i < expr->expr.call.args.size; ++i)
            print_expr_at_depth(expr->expr.call.args.data[i], depth + 1, NULL, allow_unresolved);
        return;
    case EXPR_IDENT: {
        Object* obj = expr->expr.ident;
        switch(obj->kind)
        {
        case OBJ_VAR:
            printf("Variable \'%s\' ] (Type: %s)\n", obj->symbol,
                   debug_type_to_str(expr->type, allow_unresolved));
            return;
        case OBJ_FUNC:
            printf("Function \'%s\' ]\n", obj->symbol);
            return;
        default:
            SIC_UNREACHABLE();
        }
    }
    case EXPR_INVALID:
        printf(HL_BLUE "Invalid" HL_STOP " ]\n");
        return;
    case EXPR_MEMBER_ACCESS:
        printf("Member Access ] (Type: %s)\n", debug_type_to_str(expr->type, allow_unresolved));
        print_expr_at_depth(expr->expr.member_access.parent_expr, depth + 1, NULL, allow_unresolved);
        return;
    case EXPR_POSTFIX:
        printf("Postfix \'%s\' ] (Type: %s)\n", 
               s_unary_op_strs[expr->expr.unary.kind], 
               debug_type_to_str(expr->type, allow_unresolved));
        return;
    case EXPR_RANGE:
    case EXPR_STRUCT_INIT_LIST:
        SIC_TODO();
    case EXPR_TERNARY:
        printf("Ternary ] (Type: %s)\n", debug_type_to_str(expr->type, allow_unresolved));
        print_expr_at_depth(expr->expr.ternary.cond_expr, depth + 1, "cond", allow_unresolved);
        print_expr_at_depth(expr->expr.ternary.then_expr, depth + 1, "then", allow_unresolved);
        print_expr_at_depth(expr->expr.ternary.else_expr, depth + 1, "else", allow_unresolved);
        return;
    case EXPR_TUPLE:
    case EXPR_TYPE_IDENT:
        SIC_TODO();
    case EXPR_UNARY:
        printf("Unary \'%s\' ] (Type: %s)\n", s_unary_op_strs[expr->expr.unary.kind],
               debug_type_to_str(expr->type, allow_unresolved));
        print_expr_at_depth(expr->expr.unary.inner, depth + 1, NULL, allow_unresolved);
        return;
    case EXPR_UNRESOLVED_DOT:
        printf("Unresolved Dot ]\n");
        goto UNRESOLVED_ARROW;
    case EXPR_UNRESOLVED_ARROW:
        printf("Unresolved Arrow ]\n");
UNRESOLVED_ARROW:
        print_expr_at_depth(expr->expr.unresolved_access.parent_expr, depth + 1, NULL, allow_unresolved);
        PRINT_DEPTH(depth + 1);
        printf("member: %s\n", expr->expr.unresolved_access.member.sym);
        return;
    case EXPR_UNRESOLVED_IDENT: {
        scratch_clear();
        scratch_append(expr->expr.pre_sema_ident.data[0].sym);
        for(uint32_t i = 1; i < expr->expr.pre_sema_ident.size; ++i)
        {
            scratch_appendc(':');
            scratch_appendc(':');
            scratch_append(expr->expr.pre_sema_ident.data[i].sym);
        }
        printf("Pre-Sema Identifier \'%s\' ] (Type: %s)\n", 
               scratch_string(), debug_type_to_str(expr->type, allow_unresolved));
        return;
    }
    case EXPR_ZEROED_OUT:
    case EXPR_CT_ALIGNOF:
    case EXPR_CT_OFFSETOF:
    case EXPR_CT_SIZEOF:
        SIC_TODO();
    }
}

static void print_declaration(const ObjVar* decl, int depth, bool allow_unresolved)
{
    if(decl->initial_val != NULL)
        print_expr_at_depth(decl->initial_val, depth + 1, decl->header.symbol, allow_unresolved);
    else
    {
        PRINT_DEPTH(depth + 1);
        printf("%s: ( %s )\n", decl->header.symbol, decl->uninitialized ? "Uninitialized" : "Default initialized");
    }
}

static void print_constant(const ASTExpr* expr, bool allow_unresolved)
{
    const ASTExprConstant* constant = &expr->expr.constant;
    switch(constant->kind)
    {
    case CONSTANT_INVALID:
        break;
    case CONSTANT_BOOL:
        printf("Constant Bool val: %s ] (Type: %s)\n",
               constant->b ? "true" : "false", 
               debug_type_to_str(expr->type, allow_unresolved));
        return;
    case CONSTANT_CHAR:
        printf("Constant Char val: %d (%c) ] (Type: %s)\n",
               constant->c, (char)constant->c, debug_type_to_str(expr->type, allow_unresolved));
        return;
    case CONSTANT_FLOAT:
        printf("Constant Float val: %lf ] (Type: %s)\n",
               constant->f, debug_type_to_str(expr->type, allow_unresolved));
        return;
    case CONSTANT_INTEGER:
        printf("Constant Integer val: %lu hex: 0x%lX%lX] (Type: %s)\n",
               constant->i.lo, constant->i.hi, constant->i.lo, 
               debug_type_to_str(expr->type, allow_unresolved));
        return;
    case CONSTANT_POINTER:
        printf("Constant Pointer val: 0x%lX] (Type: %s)\n",
               constant->i.lo, debug_type_to_str(expr->type, allow_unresolved));
        return;
    case CONSTANT_STRING:
        printf("Constant String ]\n");
        return;
    case CONSTANT_ENUM:
        SIC_TODO();
    }
    SIC_UNREACHABLE();
}

static inline const char* debug_type_to_str(Type* type, bool allow_unresolved)
{
    if(type == NULL)
        return allow_unresolved ? HL_YELLOW "<Null>" HL_STOP : HL_RED "<Null>" HL_STOP;

    const char* type_string;
    ResolveStatus status = type->status;
    switch(type->kind)
    {
    case TYPE_INVALID:
        return HL_BLUE "<Invalid>" HL_STOP;
    case TYPE_TYPEOF:
        type_string = "<typeof(...)>";
        break;
    case TYPE_PS_ARRAY:
        type_string = type->status == STATUS_RESOLVED ? 
                            type_to_string(type) : 
                            str_format("%s[?]", type_to_string(type->array.elem_type));
        break;
    case TYPE_PS_USER:
    default:
        // Hack to get past assert. I dont want to remove the check from type_to_string
        type->status = STATUS_RESOLVED;
        type_string = type_to_string(type);
        type->status = status;
        break;
    }

    if(status == STATUS_RESOLVED)
        return str_format(HL_GREEN "%s" HL_STOP, type_string);

    return str_format(allow_unresolved ? (HL_YELLOW "%s" HL_STOP) : (HL_RED "%s" HL_STOP), type_string);

}

static const char* s_stmt_type_strs[] = {
    [STMT_INVALID]     = (HL_BLUE "Invalid" HL_STOP),
    [STMT_BLOCK]       = "Block",
    [STMT_BREAK]       = "Break",
    [STMT_CONTINUE]    = "Continue",
    [STMT_DECLARATION] = "Declaration",
    [STMT_EXPR_STMT]   = "Expression",
    [STMT_FOR]         = "For Loop",
    [STMT_IF]          = "If Statement",
    [STMT_NOP]         = "Nop",
    [STMT_RETURN]      = "Return Statement",
    [STMT_SWAP]        = "Swap Statement",
    [STMT_SWITCH]      = "Switch Statement",
    [STMT_WHILE]       = "While Loop",
};

static const char* s_binary_op_strs[] = {
    [BINARY_INVALID]        = "Invalid",
    [BINARY_ADD]            = "+",
    [BINARY_SUB]            = "-",
    [BINARY_MUL]            = "*",
    [BINARY_DIV]            = "/",
    [BINARY_MOD]            = "%",
    [BINARY_LOG_OR]         = "||",
    [BINARY_LOG_AND]        = "&&",
    [BINARY_EQ]             = "==",
    [BINARY_NE]             = "!=",
    [BINARY_LT]             = "<",
    [BINARY_LE]             = "<=",
    [BINARY_GT]             = ">",
    [BINARY_GE]             = ">=",
    [BINARY_SHL]            = "<<",
    [BINARY_LSHR]           = ">>",
    [BINARY_ASHR]           = ">>>",
    [BINARY_BIT_OR]         = "|",
    [BINARY_BIT_XOR]        = "^",
    [BINARY_BIT_AND]        = "&",
    [BINARY_ASSIGN]         = "=",
    [BINARY_ADD_ASSIGN]     = "+=",
    [BINARY_SUB_ASSIGN]     = "-=",
    [BINARY_MUL_ASSIGN]     = "*=",
    [BINARY_DIV_ASSIGN]     = "/=",
    [BINARY_MOD_ASSIGN]     = "%=",
    [BINARY_BIT_OR_ASSIGN]  = "|=",
    [BINARY_BIT_XOR_ASSIGN] = "^=",
    [BINARY_BIT_AND_ASSIGN] = "&=",
    [BINARY_SHL_ASSIGN]     = "<<=",
    [BINARY_LSHR_ASSIGN]    = ">>=",
    [BINARY_ASHR_ASSIGN]    = ">>>=",
};

static const char* s_unary_op_strs[] = {
    [UNARY_INVALID] = "Invalid",
    [UNARY_ADDR_OF] = "&",
    [UNARY_BIT_NOT] = "~",
    [UNARY_DEC]     = "--",
    [UNARY_DEREF]   = "*",
    [UNARY_INC]     = "++",
    [UNARY_LOG_NOT] = "!",
    [UNARY_NEG]     = "-",
};

static const char* s_vis_strs[] = {
    [VIS_PUBLIC]     = "Public",
    [VIS_PRIVATE]    = "Private",
};

#endif // SI_DEBUG
