#include "internal.h"
#include "utils/da.h"
#include "utils/file_utils.h"
#include "utils/lib.h"

#include <float.h>

typedef ASTExpr* (*ExprPrefixFunc)(Lexer*);
typedef ASTExpr* (*ExprInfixFunc)(Lexer*, ASTExpr*);
typedef struct ExprParseRule ExprParseRule;
struct ExprParseRule
{
    ExprPrefixFunc prefix;
    ExprInfixFunc  infix;
    OpPrecedence   precedence;
};

// Top-level grammar
static bool parse_top_level(Lexer* l);
static bool parse_module_path(Lexer* l, ModulePath* out);
static bool parse_function_decl(Lexer* l, Visibility vis);
static bool parse_import(Lexer* l, Visibility vis);
static bool parse_module_decl(Lexer* l, Visibility vis);
static bool parse_func_params(Lexer* l, bool allow_unnamed, ObjectDA* params, bool* is_var_args);
static bool parse_global_var_decl(Lexer* l, Visibility vis);

// Types
static Visibility  parse_visibility(Lexer* l);
static Object*     parse_enum_decl(Lexer* l, Visibility vis);
static Object*     parse_struct_decl(Lexer* l, ObjKind kind, Visibility vis);
static Object*     parse_bitfield_decl(Lexer* l, Visibility vis);
static Object*     parse_typedef(Lexer* l, Visibility vis);
static bool        parse_decl_type_or_expr(Lexer* l, bool allow_func, Type** type, ASTExpr** expr);
static inline bool parse_decl_type(Lexer* l, bool allow_func, Type** type)
{
    return parse_decl_type_or_expr(l, allow_func, type, NULL);
}

// Statements
static ASTStmt* parse_stmt(Lexer* l);
static ASTStmt* parse_stmt_block(Lexer* l);
static ASTStmt* parse_break_continue(Lexer* l, StmtKind kind);
static ASTStmt* parse_for(Lexer* l);
static ASTStmt* parse_if(Lexer* l);
static ASTStmt* parse_return(Lexer* l);
static ASTStmt* parse_switch(Lexer* l);
static ASTStmt* parse_while(Lexer* l);
static ASTStmt* parse_expr_stmt(Lexer* l);
static ASTStmt* parse_declaration(Lexer* l, Type* type);

// Expressions
static ASTExpr* parse_expr_with_prec(Lexer* l, OpPrecedence precedence, ASTExpr* left);
static inline ASTExpr* parse_expr(Lexer* l)
{
    return parse_expr_with_prec(l, PREC_ASSIGN, NULL);
}
static ASTExpr* parse_binary(Lexer* l, ASTExpr* lhs);
static ASTExpr* parse_call(Lexer* l, ASTExpr* func_expr);
static ASTExpr* parse_cast(Lexer* l, ASTExpr* expr_to_cast);
static ASTExpr* parse_ternary(Lexer* l, ASTExpr* cond);
static ASTExpr* parse_array_access(Lexer* l, ASTExpr* array_expr);
static ASTExpr* parse_member_access(Lexer* l, ASTExpr* struct_expr);
static ASTExpr* parse_incdec_postfix(Lexer* l, ASTExpr* left);
static ASTExpr* parse_struct_init_list(Lexer* l);
static ASTExpr* parse_array_init_list(Lexer* l);
static ASTExpr* parse_identifier_expr(Lexer* l);
static ASTExpr* parse_paren_expr(Lexer* l);
static ASTExpr* parse_unary_prefix(Lexer* l);
static ASTExpr* parse_binary_literal(Lexer* l);
static ASTExpr* parse_octal_literal(Lexer* l);
static ASTExpr* parse_decimal_literal(Lexer* l);
static ASTExpr* parse_hexadecimal_literal(Lexer* l);
static ASTExpr* parse_char_literal(Lexer* l);
static ASTExpr* parse_float_literal(Lexer* l);
static ASTExpr* parse_string_literal(Lexer* l);
static ASTExpr* parse_default_expr(Lexer* l);
static ASTExpr* parse_bool_literal(Lexer* l);
static ASTExpr* parse_nullptr(Lexer* l);

// Inline helpers
static inline Token*   peek_prev(Lexer* l)     { return lexer_prev(l); }
static inline Token*   peek(Lexer* l)          { return lexer_peek(l); }
static inline Token*   peek_next(Lexer* l)     { return lexer_next(l); }
static inline Token*   UNUSED peek_nextnext(Lexer* l) { return lexer_nextnext(l); }

static inline bool     tok_equal(Lexer* lexer, TokenKind kind);
static inline void     parser_error(Lexer* lexer, const char* message, ...);
static inline bool     expect(Lexer* l, TokenKind kind);
static inline void     advance(Lexer* l);
static inline bool     try_consume(Lexer* l, TokenKind kind);
static inline bool     consume(Lexer* l, TokenKind kind);
static inline void     recover_to(Lexer* l, const TokenKind stopping_kinds[], size_t count);
static inline void     recover_top_level(Lexer* l);
static inline Object*  new_obj(Token* t, ObjKind kind, Visibility vis);
static inline Object*  new_var(Token* t, VarKind kind, Visibility vis, Type* type);
static inline ASTStmt* new_stmt(Lexer* l, StmtKind kind);
static inline ASTExpr* new_expr(Lexer* l, ExprKind kind);
static inline ASTExpr* new_constant(Lexer* l, ConstantKind kind);

static ASTStmt s_bad_stmt = {0};
static ASTExpr s_bad_expr = {0};
ASTStmt* g_bad_stmt = &s_bad_stmt;
ASTExpr* g_bad_expr = &s_bad_expr;
static ASTStmt s_nop_stmt = { .kind = STMT_NOP };
static ExprParseRule expr_rules[__TOKEN_COUNT];

#define ERROR_AND_RET(ret_val, ...)   do { parser_error(l, __VA_ARGS__); return ret_val; } while(0)
#define CONSUME_OR_RET(kind, ret_val) do { if(!consume(l, kind)) return ret_val; } while(0)
#define EXPECT_OR_RET(kind, ret_val)  do { if(!expect(l, kind)) return ret_val; } while(0)
#define ASSIGN_EXPR_OR_RET(expr, ret_val)  do { if(expr_is_bad(expr = parse_expr(l))) return ret_val; } while(0)
#define ASSIGN_STMT_OR_RET(stmt, ret_val)  do { if(stmt_is_bad(stmt = parse_stmt(l))) return ret_val; } while(0)
#define BAD_STMT     (&s_bad_stmt)
#define BAD_EXPR     (&s_bad_expr)
#define NOP_STMT     (&s_nop_stmt)
#define DEFAULT_EXPR (&s_default_expr)

void parse_source_file(FileId fileid)
{
    Lexer lex = lexer_from_source(fileid);
    Lexer* const l = &lex;

    while(!tok_equal(l, TOKEN_EOF))
    {
        if(!parse_top_level(l))
            recover_top_level(l);
    }
}


static bool parse_top_level(Lexer* l)
{
    Visibility vis = parse_visibility(l);
    ObjKind kind = OBJ_UNION;
    Object* type;
    switch(peek(l)->kind)
    {
    case TOKEN_ALIAS:
        SIC_TODO();
    case TOKEN_BITFIELD:
        advance(l);
        type = parse_bitfield_decl(l, vis);
        break;
    case TOKEN_ENUM:
        advance(l);
        type = parse_enum_decl(l, vis);
        break;
    case TOKEN_FN:
        advance(l);
        return parse_function_decl(l, vis);
    case TOKEN_IMPORT:
        advance(l);
        return parse_import(l, vis);
    case TOKEN_MODULE:
        advance(l);
        return parse_module_decl(l, vis);
    case TOKEN_STRUCT:
        kind = OBJ_STRUCT;
        FALLTHROUGH;
    case TOKEN_UNION:
        advance(l);
        type = parse_struct_decl(l, kind, vis);
        break;
    case TOKEN_TYPEDEF:
        advance(l);
        type = parse_typedef(l, vis);
        break;
    default: 
        return parse_global_var_decl(l, vis);
    }

    if(type == NULL)
        return false;
    da_append(&l->module->types, type);
    return true;
}

static bool parse_module_path(Lexer* l, ModulePath* out)
{
    do
    {
        if(!tok_equal(l, TOKEN_IDENT))
            ERROR_AND_RET(false, "Expected an identifier.");
        da_reserve(out, out->size + 1);
        out->data[out->size++] = (SymbolLoc){ .sym = peek(l)->sym, .loc = peek(l)->loc };
        advance(l);
    } while(try_consume(l, TOKEN_NAMESPACE));

    da_compact(out);
    return true;
}

static bool parse_function_decl(Lexer* l, Visibility vis)
{
    CONSUME_OR_RET(TOKEN_IDENT, false);
    Object* func = new_obj(peek_prev(l), OBJ_FUNC, vis);
    ObjFunc* comps = &func->func;
    comps->signature = CALLOC_STRUCT(FuncSignature);
    func->type = type_func_ptr(comps->signature);

    CONSUME_OR_RET(TOKEN_LPAREN, false);
    if(!parse_func_params(l, false, &comps->signature->params, &comps->signature->is_var_arg))
        return false;

    if(!try_consume(l, TOKEN_ARROW))
        comps->signature->ret_type = g_type_void;
    else if(!parse_decl_type(l, false, &comps->signature->ret_type))
        return false;

    da_append(&l->module->funcs, func);

    if(try_consume(l, TOKEN_SEMI))
        return true;

    EXPECT_OR_RET(TOKEN_LBRACE, false);
    comps->body = parse_stmt_block(l);
    return true;
}

static bool parse_import(Lexer* l, UNUSED Visibility vis)
{

    ModulePath path = {0};
    if(!parse_module_path(l, &path))
        return false;
    for(uint32_t i = 0; i < path.size; ++i)
    {
        printf("%s\n", path.data[i].sym);
    }
    CONSUME_OR_RET(TOKEN_SEMI, false);
    return true;
}

static bool parse_module_decl(Lexer* l, UNUSED Visibility vis)
{
    CONSUME_OR_RET(TOKEN_IDENT, false);
    Module* parent = l->module;
    Symbol mod_name = peek_prev(l)->sym;
    SourceLoc mod_loc = peek_prev(l)->loc;
    if(try_consume(l, TOKEN_LBRACE))
    {
        l->module = module_add_submodule(parent, mod_name, mod_loc, true);
        while(!try_consume(l, TOKEN_RBRACE))
        {
            if(tok_equal(l, TOKEN_EOF))
            {
                l->module = parent;
                ERROR_AND_RET(false, "Encountered end of file, expected '}'.");
            }

            if(!parse_top_level(l))
                recover_top_level(l);
        }
        l->module = parent;
        return true;
    }
    CONSUME_OR_RET(TOKEN_SEMI, false);
    return true;
}

static bool parse_func_params(Lexer* l, bool allow_unnamed, ObjectDA* params, bool* is_var_args)
{
    while(!try_consume(l, TOKEN_RPAREN))
    {
        if(tok_equal(l, TOKEN_EOF))
            ERROR_AND_RET(false, "Encountered end of file, expected ')'.");
        
        if(params->size > 0)
            CONSUME_OR_RET(TOKEN_COMMA, false);

        if(tok_equal(l, TOKEN_ELLIPSIS))
        {
            advance(l);
            CONSUME_OR_RET(TOKEN_RPAREN, false);
            *is_var_args = true;
            return true;
        }

        Type* type;
        if(!parse_decl_type(l, false, &type))
            return false;

        Object* p = CALLOC_STRUCT(Object);
        p->kind       = OBJ_VAR;
        p->visibility = VIS_DEFAULT;
        p->type       = type;
        p->var.kind   = VAR_PARAM;
        p->loc        = peek(l)->loc;

        if(!try_consume(l, TOKEN_UNDERSCORE))
        {
            if(try_consume(l, TOKEN_IDENT))
                p->symbol = peek_prev(l)->sym;
            else if(!allow_unnamed)
                ERROR_AND_RET(false, "Expected parameter name or '_' for unused parameter.");
        }

        da_append(params, p);
    }
    
    da_compact(params);
    *is_var_args = false;
    return true;
}

static bool parse_global_var_decl(Lexer* l, Visibility vis)
{
    Type* type;
    if(!parse_decl_type(l, false, &type))
        return false;
    CONSUME_OR_RET(TOKEN_IDENT, false);
    Object* var = new_var(peek_prev(l), VAR_GLOBAL, vis, type);

    if(try_consume(l, TOKEN_ASSIGN))
        ASSIGN_EXPR_OR_RET(var->var.initial_val, false);

    CONSUME_OR_RET(TOKEN_SEMI, false);
    da_append(&l->module->vars, var);
    return true;
}

static Visibility parse_visibility(Lexer* l)
{
    Visibility vis = VIS_DEFAULT;
    switch(peek(l)->kind)
    {
    case TOKEN_PRIV:
        vis = VIS_PRIVATE;
        break;
    case TOKEN_PUB:
        vis = VIS_PUBLIC;
        break;
    default:
        return vis;
    }
    advance(l);
    return vis;
}

static Object* parse_enum_decl(Lexer* l, Visibility vis)
{
    bool is_distinct = try_consume(l, TOKEN_DISTINCT);
    CONSUME_OR_RET(TOKEN_IDENT, NULL);
    Object* obj = new_obj(peek_prev(l), OBJ_ENUM, vis);
    Type* type = obj->type = CALLOC_STRUCT(Type);
    type->kind = is_distinct ? TYPE_ENUM_DISTINCT : TYPE_ENUM;
    type->visibility = vis;
    type->user_def = obj;

    if(try_consume(l, TOKEN_COLON) && 
       ((!tok_equal(l, TOKEN_IDENT) && !token_is_typename(peek(l)->kind)) ||
        peek_next(l)->kind != TOKEN_LBRACE ||
        !parse_decl_type(l, false, &obj->enum_.underlying)))
    {
        ERROR_AND_RET(NULL, "Expected unqualified typename (e.g. int).");
    }

    CONSUME_OR_RET(TOKEN_LBRACE, NULL);
    if(tok_equal(l, TOKEN_RBRACE))
    {
        sic_error_at(obj->loc, "Enum declaration is empty.");
        return NULL;
    }

    ObjectDA* members = &obj->struct_.members;
    while(!try_consume(l, TOKEN_RBRACE))
    {
        CONSUME_OR_RET(TOKEN_IDENT, obj);
        Object* member = new_obj(peek_prev(l), OBJ_ENUM_VALUE, vis);
        if(try_consume(l, TOKEN_ASSIGN))
            ASSIGN_EXPR_OR_RET(member->enum_val.value, obj);
        da_append(members, member);
        if(!try_consume(l, TOKEN_COMMA))
            EXPECT_OR_RET(TOKEN_RBRACE, NULL);
    }

    da_compact(members);
    return obj;
    
}

static Object* parse_struct_decl(Lexer* l, ObjKind kind, Visibility vis)
{
    CONSUME_OR_RET(TOKEN_IDENT, NULL); // TODO: Change this to allow anonymous structs
    Object* obj = new_obj(peek_prev(l), kind, vis);
    Type* type = obj->type = CALLOC_STRUCT(Type);
    type->kind = kind == OBJ_STRUCT ? TYPE_STRUCT : TYPE_UNION;
    type->visibility = vis;
    type->user_def = obj;
    type->canonical = type;

    CONSUME_OR_RET(TOKEN_LBRACE, NULL);
    if(tok_equal(l, TOKEN_RBRACE))
    {
        sic_error_at(obj->loc, "%s declaration is empty.", kind == OBJ_STRUCT ? "Struct" : "Union");
        return NULL;
    }

    ObjectDA* members = &obj->struct_.members;
    while(!try_consume(l, TOKEN_RBRACE))
    {
        Type* ty;
        if(!parse_decl_type(l, false, &ty))
            return NULL;

        do
        {
            // For now we don't allow anonymous members. This will change
            CONSUME_OR_RET(TOKEN_IDENT, obj);
            Object* member = new_obj(peek_prev(l), OBJ_VAR, vis);
            member->type = ty;
            da_append(members, member);
        } while(try_consume(l, TOKEN_COMMA));

        CONSUME_OR_RET(TOKEN_SEMI, obj);
    }

    da_compact(members);
    return obj;
}

static Object* parse_bitfield_decl(Lexer* l, Visibility vis)
{
    CONSUME_OR_RET(TOKEN_IDENT, NULL); // TODO: Change this to allow anonymous structs
    Object* obj = new_obj(peek_prev(l), OBJ_BITFIELD, vis);
    // Type* type = obj->type = CALLOC_STRUCT(Type);
    // type->kind = TYPE_BITFIELD;
    // type->visibility = vis;
    // type->user_def = obj;
    // type->canonical = type;

    CONSUME_OR_RET(TOKEN_LBRACE, NULL);
    if(tok_equal(l, TOKEN_RBRACE))
    {
        sic_error_at(obj->loc, "Bitfield declaration is empty.");
        return NULL;
    }

    ObjectDA* members = &obj->struct_.members;
    while(!try_consume(l, TOKEN_RBRACE))
    {
        Type* ty;
        if(!parse_decl_type(l, false, &ty))
            return NULL;

        // For now we don't allow anonymous members. This will change
        CONSUME_OR_RET(TOKEN_IDENT, obj);
        Object* member = new_obj(peek_prev(l), OBJ_VAR, vis);
        member->type = ty;
        da_append(members, member);
        CONSUME_OR_RET(TOKEN_SEMI, obj);
    }

    da_compact(members);
    return obj;
}

static Object* parse_typedef(Lexer* l, Visibility vis)
{
    bool is_distinct = try_consume(l, TOKEN_DISTINCT);
    CONSUME_OR_RET(TOKEN_IDENT, NULL);
    Object* obj = new_obj(peek_prev(l), OBJ_TYPE_ALIAS, vis);
    Type* type = obj->type = CALLOC_STRUCT(Type);
    type->kind = is_distinct ? TYPE_ALIAS_DISTINCT : TYPE_ALIAS;
    type->visibility = vis;
    type->user_def = obj;
    CONSUME_OR_RET(TOKEN_ASSIGN, NULL);
    if(!parse_decl_type(l, true, &obj->type_alias))
        return NULL;
    CONSUME_OR_RET(TOKEN_SEMI, NULL);
    return obj;
}


static bool parse_decl_type_or_expr(Lexer* l, bool allow_func, Type** type, ASTExpr** expr)
{
    *type = NULL;
    Type* ty = NULL;

    TokenKind kind = peek(l)->kind;
    switch(kind)
    {
    case TOKEN_VOID:
    case TOKEN_BOOL:
    case TOKEN_CHAR:
    case TOKEN_UBYTE:
    case TOKEN_BYTE:
    case TOKEN_USHORT:
    case TOKEN_SHORT:
    case TOKEN_UINT:
    case TOKEN_INT:
    case TOKEN_ULONG:
    case TOKEN_LONG:
    case TOKEN_IPTR:
    case TOKEN_UPTR:
    case TOKEN_ISZ:
    case TOKEN_USZ:
    case TOKEN_FLOAT:
    case TOKEN_DOUBLE:
        ty = type_from_token(kind);
        advance(l);
        goto TYPE_SUFFIX;
    case TOKEN_AUTO:
        ty = g_type_auto;
        advance(l);
        goto TYPE_SUFFIX;
    case TOKEN_FN:
        if(!allow_func)
        {
            ERROR_AND_RET(false, "Function pointer types must first be assigned to a "
                                 "typedef before use (i.e. typedef cb = fn () -> void;).");
        }
        advance(l);
        CONSUME_OR_RET(TOKEN_LPAREN, false);
        FuncSignature* sig = CALLOC_STRUCT(FuncSignature);
        if(!parse_func_params(l, true, &sig->params, &sig->is_var_arg))
            return false;
        if(!try_consume(l, TOKEN_ARROW))
            sig->ret_type = g_type_void;
        else if(!parse_decl_type(l, false, &sig->ret_type))
            return false;
        ty = type_func_ptr(sig);
        goto TYPE_SUFFIX;
    case TOKEN_CT_TYPEOF:
        advance(l);
        CONSUME_OR_RET(TOKEN_LPAREN, false);
        ty = CALLOC_STRUCT(Type);
        ty->kind = TYPE_TYPEOF;
        ty->canonical = ty;
        ASSIGN_EXPR_OR_RET(ty->type_of, false);
        CONSUME_OR_RET(TOKEN_RPAREN, false);
        goto TYPE_SUFFIX;
    default:
        break;
    }

    // If we didnt find a builtin type, and the token kind is not
    // an identifier, this is not a type but maybe an expression.
    if(!tok_equal(l, TOKEN_IDENT))
    {
        if(expr != NULL)
        {
            *expr = parse_expr(l);
            return !expr_is_bad(*expr);
        }
        parser_error(l, "Expected typename.");
        return false;
    }

    ModulePath ident_path = {0};
    if(!parse_module_path(l, &ident_path))
        return false;

    // This can be either an expression or a type.
    // Imagine this example:
    //    something[1]
    // This can either be an array called 'something' being accessed
    // at index 1, or an array type of size 1 with element type 'something'.
    // We don't know during parse time whether something is a type or not.
    if(expr != NULL)
    {
        ASTExpr* temp_buf[64];
        uint32_t dims = 0;
        while(try_consume(l, TOKEN_LBRACKET))
        {
            if(dims > 63)
            {
                parser_error(l, "Exceeded maximum dimensions for array type (64 dimensions).");
                return false;
            }
            if(peek(l)->kind == TOKEN_ASTERISK && peek_next(l)->kind == TOKEN_RBRACKET)
            {
                // Automatically sized array, definitely a type.
                ty = CALLOC_STRUCT(Type);
                ty->kind = TYPE_PS_USER;
                ty->unresolved = ident_path;
                for(uint32_t i = 0; i < dims; ++i)
                    ty = type_array_of(ty, temp_buf[i]);
                ty = type_array_of(ty, NULL);
                advance(l);
                advance(l);
                goto TYPE_SUFFIX;
            }
            temp_buf[dims] = parse_expr(l);
            if(expr_is_bad(temp_buf[dims++]) || !consume(l, TOKEN_RBRACKET))
                return false;
        }
        
        if(tok_equal(l, TOKEN_IDENT) || tok_equal(l, TOKEN_ASTERISK)) // Treat it as a type
        {
            ty = CALLOC_STRUCT(Type);
            ty->kind = TYPE_PS_USER;
            ty->unresolved = ident_path;
            for(uint32_t i = 0; i < dims; ++i)
                ty = type_array_of(ty, temp_buf[i]);
        }
        else
        {
            ASTExpr* cur = CALLOC_STRUCT(ASTExpr);
            cur->kind = EXPR_PS_IDENT;
            cur->loc = ident_path.data[0].loc;
            cur->expr.pre_sema_ident = ident_path;
            for(uint32_t i = 0; i < dims; ++i)
            {
                ASTExpr* next = CALLOC_STRUCT(ASTExpr);
                next->loc = temp_buf[i]->loc;
                next->kind = EXPR_ARRAY_ACCESS;
                next->expr.array_access.array_expr = cur;
                next->expr.array_access.index_expr = temp_buf[i];
                cur = next;
            }
            return !expr_is_bad(*expr = parse_expr_with_prec(l, PREC_ASSIGN, cur));
        }
    }
    else
    {
        ty = CALLOC_STRUCT(Type);
        ty->kind = TYPE_PS_USER;
        ty->unresolved = ident_path;
        advance(l);
    }

TYPE_SUFFIX:
    while(true)
    {
        if(try_consume(l, TOKEN_LBRACKET))
        {
            ASTExpr* size_expr;
            if(peek(l)->kind == TOKEN_ASTERISK &&
               peek_next(l)->kind == TOKEN_RBRACKET)
            {
                // We have an array whose size is to be determined.
                size_expr = NULL;
                advance(l);
                advance(l);
            }
            else
            {
                ASSIGN_EXPR_OR_RET(size_expr, false);
                CONSUME_OR_RET(TOKEN_RBRACKET, false);
            }

            ty = type_array_of(ty, size_expr);
            continue;
        }


        if(try_consume(l, TOKEN_ASTERISK))
            ty = type_pointer_to(ty);
        else
            break;
    }

    *type = ty;
    return true;
}


static const TokenKind s_stmt_recover_list[] = { TOKEN_SEMI, TOKEN_RBRACE };

static ASTStmt* parse_stmt(Lexer* l)
{
    ASTStmt* stmt;
    switch(peek(l)->kind)
    {
    case TOKEN_SEMI:
        advance(l);
        return NOP_STMT;
    case TOKEN_LBRACE:
        stmt = parse_stmt_block(l);
        return stmt; // If stmt is invalid, we know we hit the EOF, see parse_stmt_block
    case TOKEN_BREAK:
        stmt = parse_break_continue(l, STMT_BREAK);
        return stmt;
    case TOKEN_CONTINUE:
        stmt = parse_break_continue(l, STMT_CONTINUE);
        return stmt;
    case TOKEN_CASE:
    case TOKEN_DEFAULT:
        parser_error(l, "Case/Default statement in invalid location.");
        stmt = BAD_STMT;
        break;
    case TOKEN_FOR:
        stmt = parse_for(l);
        break;
    case TOKEN_IF:
        stmt = parse_if(l);
        break;
    case TOKEN_RETURN:
        stmt = parse_return(l);
        break;
    case TOKEN_SWITCH:
        stmt = parse_switch(l);
        break;
    case TOKEN_WHILE:
        stmt = parse_while(l);
        break;
    default:
        stmt = parse_expr_stmt(l);
        break;
    }
    if(stmt_is_bad(stmt))
        recover_to(l, s_stmt_recover_list, 2);
    return stmt;
}

static ASTStmt* parse_stmt_block(Lexer* l)
{
    ASTStmt* block = new_stmt(l, STMT_BLOCK);
    advance(l);
    ASTStmt head;
    head.next = NULL;
    ASTStmt* cur_stmt = &head;

    while(!try_consume(l, TOKEN_RBRACE))
    {
        if(tok_equal(l, TOKEN_EOF))
            ERROR_AND_RET(BAD_STMT, "Encountered end of file, expected '}'.");
        cur_stmt->next = parse_stmt(l);
        if(!stmt_is_bad(cur_stmt->next) && cur_stmt->next != NOP_STMT)
            cur_stmt = cur_stmt->next;
    }

    block->stmt.block.body = head.next;
    return block;
}

static ASTStmt* parse_break_continue(Lexer* l, StmtKind kind)
{
    ASTStmt* stmt = new_stmt(l, kind);
    advance(l);
    CONSUME_OR_RET(TOKEN_SEMI, BAD_STMT);
    return stmt;
}

static ASTStmt* parse_for(Lexer* l)
{
    ASTStmt* stmt = new_stmt(l, STMT_FOR);
    ASTFor* for_stmt = &stmt->stmt.for_;
    advance(l);
    CONSUME_OR_RET(TOKEN_LPAREN, BAD_STMT);

    if(try_consume(l, TOKEN_SEMI))
        for_stmt->init_stmt = NULL;
    else
    {
        for_stmt->init_stmt = parse_expr_stmt(l);
        if(stmt_is_bad(for_stmt->init_stmt))
            return BAD_STMT;
    }

    if(try_consume(l, TOKEN_SEMI))
        for_stmt->cond_expr = NULL;
    else if(expr_is_bad(for_stmt->cond_expr = parse_expr(l)) ||
            !consume(l, TOKEN_SEMI))
        return BAD_STMT;

    if(try_consume(l, TOKEN_RPAREN))
        for_stmt->loop_expr = NULL;
    else if(expr_is_bad(for_stmt->loop_expr = parse_expr(l)) ||
            !consume(l, TOKEN_RPAREN))
        return BAD_STMT;

    for_stmt->body = parse_stmt(l);
    if(stmt_is_bad(for_stmt->body))
        return BAD_STMT;
    return stmt;
}

static ASTStmt* parse_if(Lexer* l)
{
    ASTStmt* stmt = new_stmt(l, STMT_IF);
    ASTIf* if_stmt = &stmt->stmt.if_;
    advance(l);
    CONSUME_OR_RET(TOKEN_LPAREN, BAD_STMT);

    // Condition parsing
    ASSIGN_EXPR_OR_RET(if_stmt->cond, BAD_STMT);
    CONSUME_OR_RET(TOKEN_RPAREN, BAD_STMT);

    // Then statement
    ASSIGN_STMT_OR_RET(if_stmt->then_stmt, BAD_STMT);

    // Optional else statement
    if(try_consume(l, TOKEN_ELSE))
        ASSIGN_STMT_OR_RET(if_stmt->else_stmt, BAD_STMT);

    return stmt;

}

static ASTStmt* parse_return(Lexer* l)
{
    ASTStmt* stmt = new_stmt(l, STMT_RETURN);
    advance(l);
    if(!try_consume(l, TOKEN_SEMI))
    {
        ASSIGN_EXPR_OR_RET(stmt->stmt.return_.ret_expr, BAD_STMT);
        CONSUME_OR_RET(TOKEN_SEMI, BAD_STMT);
    }
    return stmt;
}

static ASTStmt* parse_switch(Lexer* l)
{
    ASTStmt* stmt = new_stmt(l, STMT_SWITCH);
    ASTCaseDA* cases = &stmt->stmt.switch_.cases;
    ASTStmt  head;
    ASTStmt* cur = &head;
    head.next = NULL;
    advance(l);
    CONSUME_OR_RET(TOKEN_LPAREN, BAD_STMT);
    ASSIGN_EXPR_OR_RET(stmt->stmt.switch_.expr, BAD_STMT);
    CONSUME_OR_RET(TOKEN_RPAREN, BAD_STMT);
    CONSUME_OR_RET(TOKEN_LBRACE, BAD_STMT);
    while(!try_consume(l, TOKEN_RBRACE))
    {
        switch(peek(l)->kind)
        {
        case TOKEN_CASE: 
            advance(l);
            da_resize(cases, cases->size + 1);
            ASSIGN_EXPR_OR_RET(cases->data[cases->size - 1].expr, BAD_STMT);
            break;
        case TOKEN_DEFAULT:
            advance(l);
            da_resize(cases, cases->size + 1);
            break;
        case TOKEN_EOF:
            ERROR_AND_RET(false, "Encountered end of file, expected '}'.");
        default:
            if(cases->size == 0)
                ERROR_AND_RET(BAD_STMT, "Statement in switch must fall under a case.");
            cur->next = parse_stmt(l);
            if(!stmt_is_bad(cur->next) && cur->next != NOP_STMT)
                cur = cur->next;
            continue;
        }

        CONSUME_OR_RET(TOKEN_COLON, BAD_STMT);
        if(cases->size > 1)
            cases->data[cases->size - 2].body = head.next;
        head.next = NULL;
        cur = &head;
    }
    if(head.next != NULL)
        cases->data[cases->size - 1].body = head.next;
    return stmt;
}

static ASTStmt* parse_while(Lexer* l)
{
    ASTStmt* stmt = new_stmt(l, STMT_WHILE);
    ASTWhile* while_stmt = &stmt->stmt.while_;
    advance(l);
    CONSUME_OR_RET(TOKEN_LPAREN, BAD_STMT);

    while_stmt->cond = parse_expr(l);
    if(expr_is_bad(while_stmt->cond) || !consume(l, TOKEN_RPAREN))
        return BAD_STMT;

    while_stmt->body = parse_stmt(l);
    if(stmt_is_bad(while_stmt->body))
        return BAD_STMT;

    return stmt;
}

static ASTStmt* parse_expr_stmt(Lexer* l)
{
    if(try_consume(l, TOKEN_SEMI))
        return NOP_STMT;

    ASTStmt* stmt;
    ASTExpr* expr;
    Type* type;
    if(!parse_decl_type_or_expr(l, false, &type, &expr))
        return BAD_STMT;

    if(type != NULL)
        return parse_declaration(l, type);

    SIC_ASSERT(expr != NULL);

    if(tok_equal(l, TOKEN_SWAP))
    {
        stmt = new_stmt(l, STMT_SWAP);
        advance(l);
        stmt->stmt.swap.left = expr;
        stmt->stmt.swap.right = parse_expr(l);
        if(expr_is_bad(stmt->stmt.swap.right) || !consume(l, TOKEN_SEMI))
            return BAD_STMT;
        return stmt;
    }

    CONSUME_OR_RET(TOKEN_SEMI, BAD_STMT);

    stmt = new_stmt(l, STMT_EXPR_STMT);
    stmt->stmt.expr = expr;
    return stmt;
}

static ASTStmt* parse_declaration(Lexer* l, Type* type)
{
    CONSUME_OR_RET(TOKEN_IDENT, BAD_STMT);
    ASTStmt* decl_stmt = CALLOC_STRUCT(ASTStmt);
    decl_stmt->kind = STMT_SINGLE_DECL;
    decl_stmt->loc = peek_prev(l)->loc;
    Object* var = new_var(peek_prev(l), VAR_LOCAL, VIS_DEFAULT, type);
    decl_stmt->stmt.single_decl.obj = var;
    ASTExpr* expr = NULL;
    if(try_consume(l, TOKEN_ASSIGN))
    {
        expr = parse_expr(l);
        if(expr_is_bad(expr))
            goto ERR;
        decl_stmt->stmt.single_decl.init_expr = expr;
    }
    if(try_consume(l, TOKEN_SEMI))
        return decl_stmt;

    decl_stmt->kind = STMT_MULTI_DECL;
    ASTDeclDA* decl_list = &decl_stmt->stmt.multi_decl;
    memset(decl_list, 0, sizeof(ASTDeclDA));
    da_resize(decl_list, 1);
    decl_list->data[0].obj       = var;
    decl_list->data[0].init_expr = expr;

    while(try_consume(l, TOKEN_COMMA))
    {
        CONSUME_OR_RET(TOKEN_IDENT, BAD_STMT);
        da_resize(decl_list, decl_list->size + 1);
        ASTDeclaration* decl = decl_list->data + decl_list->size - 1;
        decl->obj = new_var(peek_prev(l), VAR_LOCAL, VIS_DEFAULT, type);
        if(try_consume(l, TOKEN_ASSIGN))
        {
            expr = parse_expr(l);
            if(expr_is_bad(expr))
                goto ERR;
            decl->init_expr = expr;
        }
    }

    if(consume(l, TOKEN_SEMI))
        return decl_stmt;
    decl_stmt = BAD_STMT;
ERR:
    recover_to(l, s_stmt_recover_list, 2);
    return decl_stmt;
}

static ASTExpr* parse_expr_with_prec(Lexer* l, OpPrecedence precedence, ASTExpr* left)
{
    if(left == NULL)
    {
        ExprPrefixFunc prefix = expr_rules[peek(l)->kind].prefix;
        if(prefix == NULL)
            ERROR_AND_RET(BAD_EXPR, "Expected an expression.");
        left = prefix(l);
    }

    while(!expr_is_bad(left))
    {
        ExprParseRule* rule = expr_rules + peek(l)->kind;
        if(rule->precedence < precedence)
            break;

        SIC_ASSERT(rule->infix != NULL);
        left = rule->infix(l, left);
    }
    return left;
}

static ASTExpr* parse_binary(Lexer* l, ASTExpr* lhs)
{
    ASTExpr* binary = new_expr(l, EXPR_BINARY);
    TokenKind kind = peek(l)->kind;
    advance(l);

    ASTExpr* rhs;
    OpPrecedence rhs_pref = expr_rules[kind].precedence;
    if(rhs_pref != PREC_ASSIGN)
        rhs_pref++;

    rhs = parse_expr_with_prec(l, rhs_pref, NULL);
    if(expr_is_bad(rhs))
        return BAD_EXPR;

    binary->expr.binary.lhs = lhs;
    binary->expr.binary.rhs = rhs;
    binary->expr.binary.kind = tok_to_binary_op(kind);
    return binary;
}


static ASTExpr* parse_call(Lexer* l, ASTExpr* func_expr)
{
    ASTExpr* call = new_expr(l, EXPR_FUNC_CALL);
    call->expr.call.func_expr = func_expr;
    ASTExprDA* args = &call->expr.call.args;
    advance(l);

    while(!try_consume(l, TOKEN_RPAREN))
    {
        if(args->size > 0)
            CONSUME_OR_RET(TOKEN_COMMA, BAD_EXPR);

        da_append(args, parse_expr(l));
        if(expr_is_bad(args->data[args->size - 1]))
            return BAD_EXPR;
    }

    da_compact(args);
    return call;
}

static ASTExpr* parse_cast(Lexer* l, ASTExpr* expr_to_cast)
{
    ASTExpr* cast = new_expr(l, EXPR_CAST);
    advance(l);
    
    Type* ty;
    if(!parse_decl_type(l, false, &ty))
        return BAD_EXPR;
    cast->expr.cast.inner = expr_to_cast;
    cast->type = ty;

    return cast;
}

static ASTExpr* parse_ternary(Lexer* l, ASTExpr* cond)
{
    ASTExpr* tern = new_expr(l, EXPR_TERNARY);
    advance(l);
    if(tok_equal(l, TOKEN_COLON))
        advance(l);
    else if(expr_is_bad(tern->expr.ternary.then_expr = parse_expr(l)) ||
            !consume(l, TOKEN_COLON))
        return BAD_EXPR;
    
    if(expr_is_bad(tern->expr.ternary.else_expr = parse_expr_with_prec(l, PREC_TERNARY, NULL)))
        return BAD_EXPR;

    tern->expr.ternary.cond_expr = cond;
    return tern;
}

static ASTExpr* parse_array_access(Lexer* l, ASTExpr* array_expr)
{
    ASTExpr* access = new_expr(l, EXPR_ARRAY_ACCESS);
    access->expr.array_access.array_expr = array_expr;
    advance(l);

    ASTExpr* index_expr = parse_expr(l);
    if(expr_is_bad(index_expr) || !consume(l, TOKEN_RBRACKET))
        return BAD_EXPR;

    access->expr.array_access.index_expr = index_expr;
    return access;
}

static ASTExpr* parse_member_access(Lexer* l, ASTExpr* struct_expr)
{
    ASTExpr* access = new_expr(l, tok_equal(l, TOKEN_ARROW) ? EXPR_UNRESOLVED_ARR : 
                                                              EXPR_UNRESOLVED_DOT);
    advance(l);
    access->expr.unresolved_access.parent_expr = struct_expr;
    EXPECT_OR_RET(TOKEN_IDENT, BAD_EXPR);
    access->expr.unresolved_access.member_sym = peek(l)->sym;
    access->expr.unresolved_access.member_loc = peek(l)->loc;
    advance(l);
    return access;
}

static ASTExpr* parse_incdec_postfix(Lexer* l, ASTExpr* left)
{
    ASTExpr* result = new_expr(l, EXPR_POSTFIX);
    result->expr.unary.inner = left;
    result->expr.unary.kind = tok_to_unary_op(peek(l)->kind);
    advance(l);
    return result;
}

static ASTExpr* parse_struct_init_list(Lexer* l)
{
    ASTExpr* expr = new_expr(l, EXPR_STRUCT_INIT_LIST);
    InitList* list = &expr->expr.init_list;
    advance(l);
    while(true)
    {
        da_reserve(list, list->size + 1);
        InitListEntry* entry = list->data + list->size;
        switch(peek(l)->kind)
        {
        case TOKEN_IDENT:
            if(peek_next(l)->kind != TOKEN_COLON)
                break;
            entry->member = parse_identifier_expr(l);
            list->size++;
            advance(l);
            break;
        case TOKEN_RBRACE:
            goto OUTER;
        case TOKEN_EOF:
            ERROR_AND_RET(BAD_EXPR, "Encountered end of file, expected '}'.");
        default:
            list->size++;
            break;
        }
        ASSIGN_EXPR_OR_RET(entry->init_value, BAD_EXPR);

        if(!try_consume(l, TOKEN_COMMA))
            break;
    }
    da_compact(list);
OUTER:
    CONSUME_OR_RET(TOKEN_RBRACE, BAD_EXPR);
    return expr;
}

static ASTExpr* parse_array_init_list(Lexer* l)
{
    ASTExpr* expr = new_expr(l, EXPR_ARRAY_INIT_LIST);
    InitList* list = &expr->expr.init_list;
    advance(l);
    while(!tok_equal(l, TOKEN_RBRACKET))
    {
        if(tok_equal(l, TOKEN_EOF))
            ERROR_AND_RET(BAD_EXPR, "Encountered end of file, expected '}'.");
        da_reserve(list, list->size + 1);
        InitListEntry* entry = list->data + list->size;
        ASSIGN_EXPR_OR_RET(entry->init_value, BAD_EXPR);
        list->size++;
        if(try_consume(l, TOKEN_COLON))
        {
            entry->arr_index = entry->init_value;
            ASSIGN_EXPR_OR_RET(entry->init_value, BAD_EXPR);
        }
        if(!try_consume(l, TOKEN_COMMA))
            break;
    }

    da_compact(list);
    CONSUME_OR_RET(TOKEN_RBRACKET, BAD_EXPR);
    expr->type = g_type_anon_arr;
    return expr;

}

static ASTExpr* parse_invalid(UNUSED Lexer* l)
{
    advance(l);
    return BAD_EXPR;
}

static ASTExpr* parse_identifier_expr(Lexer* l)
{
    ASTExpr* expr = new_expr(l, EXPR_PS_IDENT);
    if(!parse_module_path(l, &expr->expr.pre_sema_ident))
        return BAD_EXPR;
    return expr;
}

static ASTExpr* parse_paren_expr(Lexer* l)
{
    SIC_ASSERT(peek(l)->kind == TOKEN_LPAREN);
    advance(l);
    ASTExpr* inside_expr = parse_expr(l);
    CONSUME_OR_RET(TOKEN_RPAREN, BAD_EXPR);
    return inside_expr;
}

static ASTExpr* parse_unary_prefix(Lexer* l)
{
    ASTExpr* expr = new_expr(l, EXPR_UNARY);
    TokenKind kind = peek(l)->kind;
    advance(l);
    expr->expr.unary.inner = parse_expr_with_prec(l, PREC_PRIMARY_POSTFIX, NULL);
    if(expr_is_bad(expr->expr.unary.inner))
        return BAD_EXPR;
    expr->expr.unary.kind = tok_to_unary_op(kind);
    return expr;

}

static ASTExpr* parse_binary_literal(Lexer* l)
{
    ASTExpr* expr = new_constant(l, CONSTANT_INTEGER);
    advance(l);

    const char* src = peek_prev(l)->start;
    uint64_t val = 0;
    for(uint32_t i = 2; i < expr->loc.len; ++i)
    {
        if(src[i] == '_')
            continue;
        if(val > (UINT64_MAX >> 1))
            ERROR_AND_RET(BAD_EXPR, "Integer value exceeds maximum possible 64 bit value.");
        val = (val << 1) + src[i] - '0';
    }

    expr->expr.constant.val.i = val;
    expr->type = val > UINT32_MAX ? g_type_ulong : g_type_uint;

    // TODO: Deal with the suffix.
    return expr;
}

static ASTExpr* parse_octal_literal(Lexer* l)
{
    ASTExpr* expr = new_constant(l, CONSTANT_INTEGER);
    advance(l);

    const char* src = peek_prev(l)->start;
    uint64_t val = 0;
    for(uint32_t i = 2; i < expr->loc.len; ++i)
    {
        if(src[i] == '_')
            continue;
        if(val > (UINT64_MAX >> 3))
            ERROR_AND_RET(BAD_EXPR, "Integer value exceeds maximum possible 64 bit value.");
        val = (val << 3) + src[i] - '0';
    }

    expr->expr.constant.val.i = val;
    expr->type = val > UINT32_MAX ? g_type_ulong : g_type_uint;

    // TODO: Deal with the suffix.
    return expr;
}

static ASTExpr* parse_decimal_literal(Lexer* l)
{
    ASTExpr* expr = new_constant(l, CONSTANT_INTEGER);
    advance(l);

    const char* src = peek_prev(l)->start;
    uint64_t val = 0;
    for(uint32_t i = 0; i < expr->loc.len; ++i)
    {
        if(src[i] == '_')
            continue;
        uint64_t digit = src[i] - '0';
        if(val > (UINT64_MAX - digit) / 10)
            ERROR_AND_RET(BAD_EXPR, "Integer value exceeds maximum possible 64 bit value.");
        val = (val * 10) + digit;
    }

    expr->expr.constant.val.i = val;
    expr->type = val > UINT32_MAX ? g_type_ulong : g_type_uint;

    // TODO: Deal with the suffix.
    return expr;
}

static ASTExpr* parse_hexadecimal_literal(Lexer* l)
{
    ASTExpr* expr = new_constant(l, CONSTANT_INTEGER);
    advance(l);

    const char* src = peek_prev(l)->start;
    uint64_t val = 0;
    for(uint32_t i = 2; i < expr->loc.len; ++i)
    {
        if(src[i] == '_')
            continue;
        if(val > (UINT64_MAX >> 4))
            ERROR_AND_RET(BAD_EXPR, "Integer value exceeds maximum possible 64 bit value.");
        uint64_t hex_val = g_hex_char_to_val[(uint8_t)src[i]] - 1;
        SIC_ASSERT(hex_val < 16);
        val = (val << 4) + hex_val;
    }

    expr->expr.constant.val.i = val;
    expr->type = val > UINT32_MAX ? g_type_ulong : g_type_uint;

    // TODO: Deal with the suffix.
    return expr;
    
}

static ASTExpr* parse_char_literal(Lexer* l)
{
    // TODO: Add checks for char16 and char32 once added.
    ASTExpr* expr = new_constant(l, CONSTANT_INTEGER);
    expr->expr.constant.val.i = *(uint32_t*)peek(l)->chr.val; // Hack for now
    expr->type = g_type_char;
    advance(l);
    return expr;
}

static ASTExpr* parse_float_literal(Lexer* l)
{
    ASTExpr* expr = new_constant(l, CONSTANT_FLOAT);
    advance(l);
    const char* src = peek_prev(l)->start;
    double val = 0.0f;
    uint32_t index = 0;
    while(true)
    {
        if(index >= expr->loc.len)
            goto END;
        char c = src[index];
        if(!c_is_num(c))
            break;
        double prev = val;
        val *= 10.0;
        val += (double)(c - '0');
        if(prev > val)
            ERROR_AND_RET(BAD_EXPR, "Float value exceeds maximum double value.");
        index++;
    }
    
    if(src[index] == '.')
    {
        index++;
        double factor = 1;
        while(true)
        {
            if(index >= expr->loc.len)
                goto END;
            char c = src[index];
            if(!c_is_num(c))
                break;
            factor *= 0.1;
            val += (double)(c - '0') * factor;
            index++;
        }
    }

END:
    expr->type = val > FLT_MAX ? g_type_double : g_type_float;
    expr->expr.constant.val.f = val;
    return expr;
}

static ASTExpr* parse_string_literal(Lexer* l)
{
    ASTExpr* expr = new_constant(l, CONSTANT_STRING);
    expr->expr.constant.val.str = peek(l)->str.val;
    expr->expr.constant.val.str_len = peek(l)->str.len;
    expr->type = g_type_anon_arr;
    advance(l);
    return expr;
}

static ASTExpr* parse_default_expr(Lexer* l)
{
    ASTExpr* expr = new_expr(l, EXPR_DEFAULT);
    advance(l);
    return expr;
}

static ASTExpr* parse_bool_literal(Lexer* l)
{
    ASTExpr* expr = new_constant(l, CONSTANT_INTEGER);
    expr->expr.constant.val.i = peek(l)->kind == TOKEN_TRUE;
    expr->type = g_type_bool;
    advance(l);
    return expr;
}

static ASTExpr* parse_nullptr(Lexer* l)
{
    ASTExpr* expr = new_constant(l, CONSTANT_POINTER);
    expr->expr.constant.val.i = 0;
    expr->type = g_type_voidptr;
    advance(l);
    return expr;
}

static ASTExpr* parse_sizeof(Lexer* l)
{
    ASTExpr* expr = new_expr(l, EXPR_CT_SIZEOF);
    advance(l);
    CONSUME_OR_RET(TOKEN_LPAREN, BAD_EXPR);
    if(!parse_decl_type(l, false, &expr->expr.ct_sizeof_type))
        return BAD_EXPR;
    CONSUME_OR_RET(TOKEN_RPAREN, BAD_EXPR);
    expr->type = g_type_ulong;
    return expr;
}

static inline bool tok_equal(Lexer* l, TokenKind kind)
{
    return peek(l)->kind == kind;
}

static inline void parser_error(Lexer* l, const char* message, ...)
{
    va_list va;
    va_start(va, message);
    sic_diagnostic_atv(peek(l)->loc, DIAG_ERROR, message, va);
    va_end(va);
}

static inline bool expect(Lexer* l, TokenKind kind)
{
    if(tok_equal(l, kind))
        return true;

    parser_error(l, "Expected \'%s\'", tok_kind_to_str(kind));
    return false;
}

static inline void advance(Lexer* l)
{
#ifdef SI_DEBUG
    if(g_compiler.debug_output & DEBUG_LEXER)
        print_token(peek(l));
#endif
    lexer_advance(l);
}

static inline bool try_consume(Lexer* l, TokenKind kind)
{
    if(tok_equal(l, kind))
    {
        advance(l);
        return true;
    }
    return false;
}

static inline bool consume(Lexer* l, TokenKind kind)
{
    if(tok_equal(l, kind))
    {
        advance(l);
        return true;
    }

    parser_error(l, "Expected \'%s\'.", tok_kind_to_str(kind));
    return false;
}

static inline Object* new_obj(Token* t, ObjKind kind, Visibility vis)
{
    SIC_ASSERT(t->kind == TOKEN_IDENT);
    Object* obj     = CALLOC_STRUCT(Object);
    obj->symbol     = t->sym;
    obj->loc        = t->loc;
    obj->kind       = kind;
    obj->visibility = vis;
    return obj;
}

static inline Object* new_var(Token* t, VarKind kind, Visibility vis, Type* type)
{
    Object* obj     = CALLOC_STRUCT(Object);
    obj->symbol     = t->kind == TOKEN_IDENT ? t->sym : NULL;
    obj->loc        = t->loc;
    obj->kind       = OBJ_VAR;
    obj->visibility = vis;
    obj->var.kind   = kind;
    obj->type       = type;
    return obj;
    
}

static inline ASTStmt* new_stmt(Lexer* l, StmtKind kind)
{
    ASTStmt* stmt = CALLOC_STRUCT(ASTStmt);
    stmt->kind = kind;
    stmt->loc = peek(l)->loc;
    return stmt;
}

static inline ASTExpr* new_expr(Lexer* l, ExprKind kind)
{
    ASTExpr* expr = CALLOC_STRUCT(ASTExpr);
    expr->kind = kind;
    expr->loc = peek(l)->loc;
    return expr;
}

static inline ASTExpr* new_constant(Lexer* l, ConstantKind kind)
{
    ASTExpr* expr = CALLOC_STRUCT(ASTExpr);
    expr->kind = EXPR_CONSTANT;
    expr->loc = peek(l)->loc;
    expr->expr.constant.kind = kind;
    expr->const_eval = true;
    return expr;

}

static inline void recover_to(Lexer* l, const TokenKind stopping_kinds[], size_t count)
{
    while(true)
    {
        TokenKind kind = peek(l)->kind;
        if(kind == TOKEN_EOF)
            return;
        for(size_t i = 0; i < count; ++i)
            if(kind == stopping_kinds[i])
                return;
        advance(l);
    }
}

static inline void recover_top_level(Lexer* l)
{
    advance(l);
    Token* t = peek(l);
    while(t->kind != TOKEN_EOF && 
          (t->loc.col_num > 1 || 
           (t->kind != TOKEN_IDENT && !token_is_keyword(t->kind))))
    {
        advance(l);
        t = peek(l);
    }
}

static ExprParseRule expr_rules[__TOKEN_COUNT] = {
    [TOKEN_INVALID]         = { parse_invalid, NULL, PREC_NONE },
    [TOKEN_IDENT]           = { parse_identifier_expr, NULL, PREC_NONE },
    [TOKEN_BIN_INT_LITERAL] = { parse_binary_literal, NULL, PREC_NONE },
    [TOKEN_OCT_INT_LITERAL] = { parse_octal_literal, NULL, PREC_NONE },
    [TOKEN_DEC_INT_LITERAL] = { parse_decimal_literal, NULL, PREC_NONE },
    [TOKEN_HEX_INT_LITERAL] = { parse_hexadecimal_literal, NULL, PREC_NONE },
    [TOKEN_CHAR_LITERAL]    = { parse_char_literal, NULL, PREC_NONE },
    [TOKEN_FLOAT_LITERAL]   = { parse_float_literal, NULL, PREC_NONE },
    [TOKEN_STRING_LITERAL]  = { parse_string_literal, NULL, PREC_NONE },
    [TOKEN_AMP]             = { parse_unary_prefix, parse_binary, PREC_BIT_AND },
    [TOKEN_ASTERISK]        = { parse_unary_prefix, parse_binary, PREC_MUL_DIV_MOD },
    [TOKEN_LOG_NOT]         = { parse_unary_prefix, NULL, PREC_NONE },
    [TOKEN_BIT_NOT]         = { parse_unary_prefix, NULL, PREC_NONE },
    [TOKEN_BIT_OR]          = { NULL, parse_binary, PREC_BIT_OR },
    [TOKEN_BIT_XOR]         = { NULL, parse_binary, PREC_BIT_XOR },
    [TOKEN_ASSIGN]          = { NULL, parse_binary, PREC_ASSIGN },
    [TOKEN_LT]              = { NULL, parse_binary, PREC_RELATIONAL },
    [TOKEN_GT]              = { NULL, parse_binary, PREC_RELATIONAL },
    [TOKEN_DIV]             = { NULL, parse_binary, PREC_MUL_DIV_MOD },
    [TOKEN_DOT]             = { NULL, parse_member_access, PREC_PRIMARY_POSTFIX },
    [TOKEN_LBRACE]          = { parse_struct_init_list, NULL, PREC_NONE },
    [TOKEN_LBRACKET]        = { parse_array_init_list, parse_array_access, PREC_PRIMARY_POSTFIX },
    [TOKEN_LPAREN]          = { parse_paren_expr, parse_call, PREC_PRIMARY_POSTFIX },
    [TOKEN_ADD]             = { NULL, parse_binary, PREC_ADD_SUB },
    [TOKEN_SUB]             = { parse_unary_prefix, parse_binary, PREC_ADD_SUB },
    [TOKEN_MODULO]          = { NULL, parse_binary, PREC_MUL_DIV_MOD },
    [TOKEN_QUESTION]        = { NULL, parse_ternary, PREC_TERNARY },
    [TOKEN_ARROW]           = { NULL, parse_member_access, PREC_PRIMARY_POSTFIX },
    [TOKEN_LSHR]            = { NULL, parse_binary, PREC_SHIFTS },
    [TOKEN_ASHR]            = { NULL, parse_binary, PREC_SHIFTS },
    [TOKEN_SHL]             = { NULL, parse_binary, PREC_SHIFTS },
    [TOKEN_LOG_AND]         = { NULL, parse_binary, PREC_LOG_AND },
    [TOKEN_LOG_OR]          = { NULL, parse_binary, PREC_LOG_OR },
    [TOKEN_EQ]              = { NULL, parse_binary, PREC_RELATIONAL },
    [TOKEN_NE]              = { NULL, parse_binary, PREC_RELATIONAL },
    [TOKEN_LE]              = { NULL, parse_binary, PREC_RELATIONAL },
    [TOKEN_GE]              = { NULL, parse_binary, PREC_RELATIONAL },
    [TOKEN_BIT_AND_ASSIGN]  = { NULL, parse_binary, PREC_ASSIGN },
    [TOKEN_BIT_OR_ASSIGN]   = { NULL, parse_binary, PREC_ASSIGN },
    [TOKEN_BIT_XOR_ASSIGN]  = { NULL, parse_binary, PREC_ASSIGN },
    [TOKEN_ADD_ASSIGN]      = { NULL, parse_binary, PREC_ASSIGN },
    [TOKEN_SUB_ASSIGN]      = { NULL, parse_binary, PREC_ASSIGN },
    [TOKEN_MUL_ASSIGN]      = { NULL, parse_binary, PREC_ASSIGN },
    [TOKEN_DIV_ASSIGN]      = { NULL, parse_binary, PREC_ASSIGN },
    [TOKEN_MOD_ASSIGN]      = { NULL, parse_binary, PREC_ASSIGN },
    [TOKEN_LSHR_ASSIGN]     = { NULL, parse_binary, PREC_ASSIGN },
    [TOKEN_ASHR_ASSIGN]     = { NULL, parse_binary, PREC_ASSIGN },
    [TOKEN_SHL_ASSIGN]      = { NULL, parse_binary, PREC_ASSIGN },
    [TOKEN_INCREM]          = { parse_unary_prefix, parse_incdec_postfix, PREC_PRIMARY_POSTFIX },
    [TOKEN_DECREM]          = { parse_unary_prefix, parse_incdec_postfix, PREC_PRIMARY_POSTFIX },

    [TOKEN_AS]              = { NULL, parse_cast, PREC_PRIMARY_POSTFIX },
    [TOKEN_DEFAULT]         = { parse_default_expr, NULL, PREC_NONE },
    [TOKEN_FALSE]           = { parse_bool_literal, NULL, PREC_NONE },
    [TOKEN_NULLPTR]         = { parse_nullptr, NULL, PREC_NONE },
    [TOKEN_TRUE]            = { parse_bool_literal, NULL, PREC_NONE },

    [TOKEN_CT_ALIGNOF]      = {},
	[TOKEN_CT_ASSERT]       = {},
	[TOKEN_CT_OFFSETOF]     = {},
	[TOKEN_CT_SIZEOF]       = { parse_sizeof, NULL, PREC_NONE },
	[TOKEN_CT_TYPEOF]       = {},
};
