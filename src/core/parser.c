#include "internal.h"
#include "utils/da.h"
#include "utils/error.h"
#include "utils/lib.h"

typedef ASTExpr* (*ExprPrefixFunc)(Lexer*);
typedef ASTExpr* (*ExprInfixFunc)(Lexer*, ASTExpr*);
typedef struct ExprParseRule ExprParseRule;
struct ExprParseRule
{
    ExprPrefixFunc prefix;
    ExprInfixFunc  infix;
    OpPrecedence   precedence;
};

// Global check and add functions
static bool      parse_top_level(Lexer* l);
static bool      function_declaration(Lexer* l, ObjAccess access, Type* ret_type, ObjAttr attribs);
static bool      global_var_declaration(Lexer* l, ObjAccess access, Type* type, ObjAttr attribs);
static ObjAccess parse_access(Lexer* l);
static Object*   parse_struct_decl(Lexer* l, ObjAccess access);

// Grammar parsing
static bool     parse_attribute(Lexer* l, ObjAttr* attribs);
static bool     parse_type_qualifier(Lexer* l, TypeQualifier* qual);
static bool     parse_type_base(Lexer* l, Type** type, ObjAttr* attribs);
static Type*    parse_type_tail(Lexer* l, Type* base_type);
static bool     parse_func_params(Lexer* l, ObjectDA* params, bool* is_var_args);
static ASTStmt* parse_stmt_block(Lexer* l);
static ASTStmt* parse_stmt(Lexer* l);
static ASTStmt* parse_declaration(Lexer* l, ASTStmt* overwrite, Type* type, ObjAttr attribs);
static ASTExpr* parse_expr(Lexer* l, OpPrecedence precedence);

// Inline helpers

static inline Token* peek(Lexer* lexer)
{
    return lexer->la_buf.buf + lexer->la_buf.head;
}

static inline Token* peek_forw(Lexer* lexer, uint32_t count)
{
    SIC_ASSERT(count < LOOK_AHEAD_SIZE);
    return lexer->la_buf.buf + ((lexer->la_buf.head + count) % LOOK_AHEAD_SIZE);
}

static inline bool tok_equal(Lexer* lexer, TokenKind kind)
{
    return peek(lexer)->kind == kind;
}

static inline bool tok_equal_forw(Lexer* lexer, uint32_t count, TokenKind kind)
{
    return peek_forw(lexer, count)->kind == kind;
}

static inline void parser_error(Lexer* lexer, const char* restrict message, ...)
{
    va_list va;
    va_start(va, message);
    Token* t = peek(lexer);
    sic_error_atv(lexer->unit->file.full_path, &t->loc, message, va);
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
    if(!lexer_advance(l))
        return;
}

static inline void advance_many(Lexer* l, uint32_t steps)
{
    SIC_ASSERT(steps > 1);
    for(uint32_t i = 0; i < steps; ++i)
    {
        if(!lexer_advance(l))
            return;
    }
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
    if(try_consume(l, kind))
        return true;

    parser_error(l, "Expected \'%s\'.", tok_kind_to_str(kind));
    return false;
}

static inline Object* new_obj(Lexer* l, ObjKind kind, ObjAccess access, ObjAttr attribs)
{
    Object* obj = CALLOC_STRUCT(Object);
    obj->kind = kind;
    obj->access = access;
    obj->attribs = attribs;
    obj->symbol = peek(l)->loc;
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

static inline void recover_to(Lexer* l, const TokenKind stopping_kinds[], size_t count)
{
    while(true)
    {
        TokenKind kind = peek(l)->kind;
        SIC_ASSERT(kind != TOKEN_INVALID);
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
    if(peek(l)->kind == TOKEN_EOF)
        return;
    advance(l);
    Token* t = peek(l);
    while(t->kind != TOKEN_EOF && 
          (t->loc.start != t->loc.line_start || 
           (t->kind != TOKEN_IDENT && !token_is_keyword(t->kind))))
    {
        advance(l);
        t = peek(l);
    }
}

static ASTStmt s_badstmt = {0};
static ASTExpr s_badexpr = {0};
static ExprParseRule expr_rules[__TOKEN_COUNT];

#define ERROR_AND_RET(ret_val, l, ...)   do { parser_error(l, __VA_ARGS__); return ret_val; } while(0)
#define CONSUME_OR_RET(ret_val, l, kind) do { if(!consume(l, kind)) return ret_val; } while(0)
#define EXPECT_OR_RET(ret_val, l, kind)  do { if(!expect(l, kind)) return ret_val; } while(0)
#define BAD_STMT (&s_badstmt)
#define BAD_EXPR (&s_badexpr)


void parser_init(void)
{
}

void parse_unit(CompilationUnit* unit)
{
    SIC_ASSERT(unit != NULL);
    Lexer l;
    lexer_init_unit(&l, unit);
    da_init(&unit->funcs, 0);
    da_init(&unit->types, 0);
    da_init(&unit->vars, 0);
    // if(try_consume(&l, TOKEN_MODULE))
    // {
    // }
    da_append(&g_compiler.top_module.units, unit);

    while(!tok_equal(&l, TOKEN_EOF))
    {
        if(!parse_top_level(&l))
            recover_top_level(&l);
    }
}

void parse_ambiguous_decl(Lexer* l, ASTStmt* ambiguous)
{
    // We start on the identifier when resolving ambiguous stmt
    advance(l);
    Type* ty = parse_type_tail(l, ambiguous->stmt.ambiguous);
    parse_declaration(l, ambiguous, ty, ATTR_NONE);
}

ASTExpr* parse_ambiguous_expr(Lexer* l)
{
    return parse_expr(l, PREC_ASSIGN);
}

static bool parse_top_level(Lexer* l)
{
    ObjAccess access = parse_access(l);
    switch(peek(l)->kind)
    {
    case TOKEN_BITFIELD:
    case TOKEN_ENUM:
    case TOKEN_MODULE:
        SIC_TODO();
    case TOKEN_STRUCT:
        advance(l);
        Object* struct_ = parse_struct_decl(l, access);
        if(struct_ == NULL)
            return false;
        da_append(&l->unit->types, struct_);
        return true;
    case TOKEN_TYPEDEF:
    case TOKEN_UNION:
        SIC_TODO();
    default: {
        Type* type;
        ObjAttr attribs = ATTR_NONE;
        parse_type_base(l, &type, &attribs);
        type = parse_type_tail(l, type);

        if(type == NULL)
        {
            parser_error(l, "Missing type specifier.");
            return false;
        }
        if(!tok_equal(l, TOKEN_IDENT))
        {
            parser_error(l, "Expected identifier.");
            return false;
        }
        if(tok_equal_forw(l, 1, TOKEN_LPAREN))
            return function_declaration(l, access, type, attribs);
        
        return global_var_declaration(l, access, type, attribs);
    }
    }

}

static bool function_declaration(Lexer* l, ObjAccess access, Type* ret_type, ObjAttr attribs)
{
    SIC_ASSERT(tok_equal(l, TOKEN_IDENT));
    SIC_ASSERT(tok_equal_forw(l, 1, TOKEN_LPAREN));

    Object* func = new_obj(l, OBJ_FUNC, access, attribs);
    ObjFunc* comps = &func->func;
    comps->signature = CALLOC_STRUCT(FuncSignature);
    comps->signature->ret_type = ret_type;
    da_init(&comps->signature->params, 8);

    advance_many(l, 2);
    if(!parse_func_params(l, &comps->signature->params, &comps->signature->is_var_arg))
        return false;

    if(try_consume(l, TOKEN_SEMI))
        goto END;
    else if(attribs & ATTR_EXTERN)
    {
        parser_error(l, "Function declared extern should end with ';'.");
        return false;
    }

    ASTStmt* body_block = parse_stmt_block(l);
    comps->body = body_block->stmt.block.body;

END:
    da_append(&l->unit->funcs, func);
    return true;
}

static bool global_var_declaration(Lexer* l, ObjAccess access, Type* type, ObjAttr attribs)
{
    SIC_ASSERT(tok_equal(l, TOKEN_IDENT));
    Object* var = new_obj(l, OBJ_VAR, access, attribs);
    ObjVar* comps = &var->var;
    comps->type = type;
    advance(l);

    if(try_consume(l, TOKEN_SEMI))
        goto END;
    
    SIC_TODO_MSG("Global variable initialization.");

END:
    da_append(&l->unit->vars, var);
    return true;
}

static ObjAccess parse_access(Lexer* l)
{
    ObjAccess access = ACCESS_PROTECTED;
    switch(peek(l)->kind)
    {
    case TOKEN_PRIV:
        access = ACCESS_PRIVATE;
        break;
    case TOKEN_PROT:
        break;
    case TOKEN_PUB:
        access = ACCESS_PUBLIC;
        break;
    default:
        return access;
    }
    advance(l);
    return access;
}

static Object* parse_struct_decl(Lexer* l, ObjAccess access)
{
    Object* obj = new_obj(l, OBJ_STRUCT, access, ATTR_NONE);
    da_init(&obj->struct_.members, 8);
    if(tok_equal(l, TOKEN_IDENT))
        advance(l);
    else // If there is no identifier, we assume it is an anonymous struct
        obj->symbol.start = NULL;

    CONSUME_OR_RET(NULL, l, TOKEN_LBRACE);
    if(tok_equal(l, TOKEN_RBRACE))
        ERROR_AND_RET(NULL, l, "Struct declaration is empty.");
    while(!try_consume(l, TOKEN_RBRACE))
    {
        Type* ty;
        parse_type_base(l, &ty, NULL);
        ty = parse_type_tail(l, ty);
        if(ty == NULL)
            ERROR_AND_RET(NULL, l, "Expected type specifier.");

        // For now we don't allow anonymous members. This will change
        EXPECT_OR_RET(NULL, l, TOKEN_IDENT);
        Object* member = new_obj(l, OBJ_VAR, access, ATTR_NONE);
        advance(l);
        CONSUME_OR_RET(NULL, l, TOKEN_SEMI);
        member->var.type = ty;
        da_append(&obj->struct_.members, member);
        // uint32_t align = type_alignment(ty);
        // uint32_t size = type_size(ty);
        // SIC_ASSERT(is_pow_of_2(align));
        // obj->struct_.size = ALIGN_UP(obj->struct_.size, align) + size;
        // obj->struct_.align = MAX(obj->struct_.align, align);
    }
    // Debug checking
    // SIC_ASSERT(is_pow_of_2(obj->struct_.align));
    // obj->struct_.size = ALIGN_UP(obj->struct_.size, obj->struct_.align);
    // SIC_ASSERT(obj->struct_.size > 0 && (obj->struct_.size & (obj->struct_.align - 1)) == 0);
    return obj;
}

static bool parse_attribute(Lexer* l, ObjAttr* attribs)
{
    ObjAttr temp;
    switch(peek(l)->kind)
    {
    case TOKEN_EXTERN:
        temp = ATTR_EXTERN;
        break;
    default:
        return false;
    }

    if(attribs == NULL)
        parser_error(l, "Object attributes not allowed in this context.");
    else if(*attribs & temp) {} // This would be where we warn for duplicate attribute
    else
        *attribs |= temp;

    advance(l);
    return true;
}

static bool parse_type_qualifier(Lexer* l, TypeQualifier* qual)
{
    TypeQualifier temp;
    switch(peek(l)->kind)
    {
    case TOKEN_CONST:
        temp = QUALIFIER_CONST;
        break;
    default:
        temp = QUALIFIER_NONE;
        break;
    }

    if(temp != QUALIFIER_NONE)
    {
        // This would be where we warn for duplicate type qualifier
        if(*qual & temp) {}
        *qual |= temp;
        advance(l);
        return true;
    }
    return false;

}

static bool parse_type_base(Lexer* l, Type** type, ObjAttr* attribs)
{
    Type* ty = NULL;
    TypeQualifier qual = QUALIFIER_NONE;
    // If this is true, we dont know whether the type prefix is a type
    // or part of an expression, for example random() could be a function
    // call expression or a function type with the return type being a 
    // user-defined struct named random.
    bool ambiguous = true; 

    // First get attributes (e.g. extern) and qualifiers (e.g. const, volatile)
    while(parse_attribute(l, attribs) || parse_type_qualifier(l, &qual))
        ambiguous = false;

    TokenKind kind;
    // Then we parse actual typename, which will be resolved later in semantics
    while(token_is_typename(kind = peek(l)->kind))
    {
        if(ty != NULL)
            parser_error(l, "Two or more data types in type prefix");
        ty = type_from_token(kind);
        advance(l);
    }

    if(ty == NULL && tok_equal(l, TOKEN_IDENT))
    {
        ty = CALLOC_STRUCT(Type);
        ty->kind = TYPE_USER_DEF;
        ty->status = STATUS_UNRESOLVED;
        ty->unresolved = peek(l)->loc;
        ty->qualifiers = qual;
        *type = ty;
        advance(l);
        if(ambiguous)
        {
            static TokenKind bad_toks[] = { TOKEN_ASTERISK, TOKEN_LBRACKET, TOKEN_LPAREN };
            TokenKind next = peek(l)->kind;
            for(size_t i = 0; i < sizeof(bad_toks) / sizeof(bad_toks[0]); ++i)
                if(next == bad_toks[i])
                    return false;
        }
        return true;
    }


    if(ty != NULL && qual != QUALIFIER_NONE)
    {
        ty = type_copy(ty);
        ty->qualifiers = qual;
    }

    *type = ty;
    return true;
}

static Type* parse_type_tail(Lexer* l, Type* base_type)
{
    if(base_type == NULL)
        return NULL;
    Type* res = base_type;
    while(true)
    {
        if(try_consume(l, TOKEN_LBRACKET))
        {
            // TODO: Add auto detection for size when initialized.
            //       For now size must be specified as an expression.
            res = type_array_of(res, parse_expr(l, PREC_ASSIGN));
            CONSUME_OR_RET(NULL, l, TOKEN_RBRACKET);
            continue;
        }


        if(tok_equal(l, TOKEN_LPAREN))
        {
            SIC_TODO_MSG("Function pointer types not implemented yet.");
            // Object* params;
            // size_t count;
            // if(!parse_func_params(l, &params, &count))
            //     return;
        }
        else if(tok_equal(l, TOKEN_ASTERISK))
        {
            res = type_pointer_to(res);
            advance(l);
        }
        else
            return res;

        res->qualifiers = QUALIFIER_NONE;
        while(parse_type_qualifier(l, &res->qualifiers)) {}
    }
}

static bool parse_func_params(Lexer* l, ObjectDA* params, bool* is_var_args)
{
    while(!tok_equal(l, TOKEN_RPAREN))
    {
        if(tok_equal(l, TOKEN_EOF))
            ERROR_AND_RET(false, l, "No closing parentheses.");
        
        if(params->size > 0)
            consume(l, TOKEN_COMMA);

        // TODO: Make a more robust variable argument system
        if(tok_equal(l, TOKEN_ELLIPSIS))
        {
            advance(l);
            CONSUME_OR_RET(false, l, TOKEN_RPAREN);
            *is_var_args = true;
            return true;
        }

        Type* type;
        parse_type_base(l, &type, NULL);
        type = parse_type_tail(l, type);
        if(type == NULL)
            ERROR_AND_RET(false, l, "Missing type specifier.");

        EXPECT_OR_RET(false, l, TOKEN_IDENT);

        da_resize(params, params->size + 1);
        params->data[params->size - 1] = CALLOC_STRUCT(Object);
        params->data[params->size - 1]->kind = OBJ_VAR;
        params->data[params->size - 1]->var.type = type;
        params->data[params->size - 1]->symbol = peek(l)->loc;

        advance(l);
    }
    
    advance(l);
    *is_var_args = false;
    return true;
}

static const TokenKind s_stmt_recover_list[] = { TOKEN_SEMI, TOKEN_RBRACE };

static ASTStmt* parse_stmt_block(Lexer* l)
{
    CONSUME_OR_RET(BAD_STMT, l, TOKEN_LBRACE);
    ASTStmt* block = new_stmt(l, STMT_BLOCK);
    ASTStmt head;
    head.next = NULL;
    ASTStmt* cur_stmt = &head;

    while(!try_consume(l, TOKEN_RBRACE))
    {
        if(tok_equal(l, TOKEN_EOF))
            ERROR_AND_RET(BAD_STMT, l, "No closing }.");
        Lexer copy = *l;
        Type* type;
        ObjAttr attribs = ATTR_NONE;
        if(!parse_type_base(l, &type, &attribs))
        {
            // Ambiguous type base, could be expression or
            // declaration.
            cur_stmt->next = new_stmt(l, STMT_AMBIGUOUS);
            cur_stmt->next->stmt.ambiguous = type;
            cur_stmt = cur_stmt->next;
            recover_to(l, s_stmt_recover_list, 2);
            continue;
        }
        type = parse_type_tail(l, type);

        if(type != NULL && tok_equal(l, TOKEN_IDENT))
        {
            cur_stmt->next = parse_declaration(l, NULL, type, attribs);
            if(cur_stmt->next->kind == STMT_INVALID)
                continue;
        }
        else
        {
            *l = copy;
            cur_stmt->next = parse_stmt(l);
        }
        cur_stmt = cur_stmt->next;
    }

    block->stmt.block.body = head.next;
    return block;
}


static ASTStmt* parse_stmt(Lexer* l)
{
    ASTStmt* stmt;
    switch(peek(l)->kind)
    {
    case TOKEN_LBRACE:
        stmt = parse_stmt_block(l);
        return stmt;
    case TOKEN_IF: {
        stmt = new_stmt(l, STMT_IF);
        ASTIf* if_stmt = &stmt->stmt.if_;
        advance(l);
        if(!consume(l, TOKEN_LPAREN))
            goto RECOVER;

        // Condition parsing
        if_stmt->cond = parse_expr(l, PREC_ASSIGN);
        if(if_stmt->cond->kind == EXPR_INVALID || 
           !consume(l, TOKEN_RPAREN))
            goto RECOVER;

        // Then statement
        if_stmt->then_stmt = parse_stmt(l);
        if(if_stmt->then_stmt->kind == STMT_INVALID)
            goto RECOVER;

        // Optional else statement
        if(try_consume(l, TOKEN_ELSE))
        {
            if_stmt->else_stmt = parse_stmt(l);
            if(if_stmt->else_stmt->kind == STMT_INVALID)
                goto RECOVER;
        }
        return stmt;
    }
    case TOKEN_RETURN:
        stmt = new_stmt(l, STMT_RETURN);
        advance(l);
        if(!try_consume(l, TOKEN_SEMI))
        {
            stmt->stmt.return_.ret_expr = parse_expr(l, PREC_ASSIGN);
            if(stmt->stmt.return_.ret_expr->kind == EXPR_INVALID ||
               !consume(l, TOKEN_SEMI))
                goto RECOVER;
        }
        return stmt;
    case TOKEN_WHILE: {
        stmt = new_stmt(l, STMT_WHILE);
        ASTWhile* while_stmt = &stmt->stmt.while_;
        advance(l);
        if(!consume(l, TOKEN_LPAREN))
            goto RECOVER;

        while_stmt->cond = parse_expr(l, PREC_ASSIGN);
        if(while_stmt->cond->kind == EXPR_INVALID || 
           !consume(l, TOKEN_RPAREN))
            goto RECOVER;

        while_stmt->body = parse_stmt(l);
        if(while_stmt->body->kind == STMT_INVALID)
            goto RECOVER;

        return stmt;
    }
    case TOKEN_SEMI:
        stmt = new_stmt(l, STMT_EXPR_STMT);
        stmt->stmt.expr = new_expr(l, EXPR_NOP);
        advance(l);
        return stmt;
    default:
        stmt = new_stmt(l, STMT_EXPR_STMT);
        stmt->stmt.expr = parse_expr(l, PREC_ASSIGN);
        if(stmt->stmt.expr->kind == EXPR_INVALID ||
           !consume(l, TOKEN_SEMI))
            goto RECOVER;
        return stmt;
    }
RECOVER:
    recover_to(l, s_stmt_recover_list, 2);
    return BAD_STMT;
}

static ASTStmt* parse_declaration(Lexer* l, ASTStmt* overwrite, Type* type, ObjAttr attribs)
{
    ASTStmt* decl_stmt = overwrite != NULL ? overwrite : new_stmt(l, STMT_SINGLE_DECL);
    Object* var = CALLOC_STRUCT(Object);
    var->kind = OBJ_VAR;
    var->attribs = attribs;
    var->var.type = type;
    var->symbol = peek(l)->loc;
    decl_stmt->stmt.single_decl.obj = var;
    ASTExpr* expr = NULL;
    advance(l);
    if(try_consume(l, TOKEN_ASSIGN))
    {
        expr = parse_expr(l, PREC_ASSIGN);
        if(expr->kind == EXPR_INVALID)
            goto ERR;
        decl_stmt->stmt.single_decl.init_expr = expr;
    }
    if(try_consume(l, TOKEN_SEMI))
        return decl_stmt;

    decl_stmt->kind = STMT_MULTI_DECL;
    ASTDeclDA* decl_list = &decl_stmt->stmt.multi_decl;
    da_init(decl_list, 8);
    decl_list->data[0].obj       = var;
    decl_list->data[0].init_expr = expr;
    decl_list->size = 1;

    while(try_consume(l, TOKEN_COMMA))
    {
        da_resize(decl_list, decl_list->size + 1);
        var = CALLOC_STRUCT(Object);
        var->kind = OBJ_VAR;
        var->attribs = attribs;
        var->var.type = type;
        var->symbol = peek(l)->loc;
        advance(l);
        if(try_consume(l, TOKEN_ASSIGN))
        {
            expr = parse_expr(l, PREC_ASSIGN);
            if(expr->kind == EXPR_INVALID)
                goto ERR;
            decl_list->data[decl_list->size - 1].init_expr = expr;
        }
        decl_list->data[decl_list->size - 1].obj = var;
    }

    if(consume(l, TOKEN_SEMI))
        return decl_stmt;
ERR:
    recover_to(l, s_stmt_recover_list, 2);
    if(overwrite == NULL)
        return BAD_STMT;
    overwrite->kind = STMT_INVALID;
    return overwrite;
}

static ASTExpr* parse_expr(Lexer* l, OpPrecedence precedence)
{
    ExprPrefixFunc prefix = expr_rules[peek(l)->kind].prefix;
    if(prefix == NULL)
        ERROR_AND_RET(BAD_EXPR, l, "Expected an expression.");

    ASTExpr* left = prefix(l);
    while(true)
    {
        TokenKind kind = peek(l)->kind;
        if(expr_rules[kind].precedence < precedence)
            break;
        if(left->kind == EXPR_INVALID)
            return left;

        ExprInfixFunc infix = expr_rules[kind].infix;
        if(infix == NULL)
            ERROR_AND_RET(BAD_EXPR, l, "Left side of operator is invalid.");
        left = infix(l, left);
    }
    return left;
}

static ASTExpr* parse_binary(Lexer* l, ASTExpr* lhs)
{
    ASTExpr* binary = new_expr(l, EXPR_BINARY);
    Token tok = *peek(l);
    advance(l);

    ASTExpr* rhs;
    OpPrecedence rhs_pref = expr_rules[tok.kind].precedence;
    if(rhs_pref != PREC_ASSIGN)
        rhs_pref++;

    rhs = parse_expr(l, rhs_pref);
    if(rhs->kind == EXPR_INVALID)
        return BAD_EXPR;

    binary->expr.binary.lhs = lhs;
    binary->expr.binary.rhs = rhs;
    binary->expr.binary.kind = tok_to_binary_op(tok.kind);
    return binary;
}


static ASTExpr* parse_call(Lexer* l, ASTExpr* func_expr)
{
    ASTExpr* call = new_expr(l, EXPR_FUNC_CALL);
    advance(l);
    call->expr.call.func_expr = func_expr;
    if(try_consume(l, TOKEN_RPAREN)) // void return
        return call;
    
    ASTExprDA* args = &call->expr.call.args;
    da_init(args, 8);
    da_append(args, parse_expr(l, PREC_ASSIGN));
    if(args->data[0]->kind == EXPR_INVALID)
        return BAD_EXPR;

    while(!try_consume(l, TOKEN_RPAREN))
    {
        CONSUME_OR_RET(BAD_EXPR, l, TOKEN_COMMA);

        da_append(args, parse_expr(l, PREC_ASSIGN));
        if(args->data[args->size - 1]->kind == EXPR_INVALID)
            return BAD_EXPR;
    }

    return call;
}

static ASTExpr* parse_cast(Lexer* l, ASTExpr* expr_to_cast)
{
    ASTExpr* cast = new_expr(l, EXPR_CAST);
    advance(l);
    
    Type* ty;
    parse_type_base(l, &ty, NULL);
    ty = parse_type_tail(l, ty);
    if(ty == NULL)
        ERROR_AND_RET(BAD_EXPR, l, "Expected a type after keyword \'as\'.");
    cast->expr.cast.expr_to_cast = expr_to_cast;
    cast->expr.cast.cast_type = ty;

    return cast;
}

static ASTExpr* parse_array_access(Lexer* l, ASTExpr* array_expr)
{
    ASTExpr* access = new_expr(l, EXPR_ARRAY_ACCESS);
    access->expr.array_access.array_expr = array_expr;
    advance(l);

    ASTExpr* index_expr = parse_expr(l, PREC_ASSIGN);
    if(index_expr->kind == EXPR_INVALID || !consume(l, TOKEN_RBRACKET))
        return BAD_EXPR;

    access->expr.array_access.index_expr = index_expr;
    return access;
}

static ASTExpr* parse_member_access(Lexer* l, ASTExpr* struct_expr)
{
    ASTExpr* access = new_expr(l, EXPR_UNRESOLVED_ACCESS);
    access->expr.unresolved_access.parent_expr = struct_expr;
    EXPECT_OR_RET(BAD_EXPR, l, TOKEN_IDENT);
    access->expr.unresolved_access.member_expr = parse_expr(l, PREC_PRIMARY_POSTFIX);
    advance(l);
    return access;
}

static ASTExpr* parse_identifier_expr(Lexer* l)
{
    ASTExpr* expr = new_expr(l, EXPR_PRE_SEMANTIC_IDENT);
    advance(l);
    return expr;
}

static ASTExpr* parse_paren_expr(Lexer* l)
{
    SIC_ASSERT(peek(l)->kind == TOKEN_LPAREN);
    advance(l);
    ASTExpr* inside_expr = parse_expr(l, PREC_ASSIGN);
    CONSUME_OR_RET(BAD_EXPR, l, TOKEN_RPAREN);
    return inside_expr;
}

static ASTExpr* parse_unary_prefix(Lexer* l)
{
    ASTExpr* expr = new_expr(l, EXPR_UNARY);
    TokenKind kind = peek(l)->kind;
    advance(l);
    expr->expr.unary.child = parse_expr(l, PREC_UNARY_PREFIX);
    if(expr->expr.unary.child->kind == EXPR_INVALID)
        return BAD_EXPR;
    expr->expr.unary.kind = tok_to_unary_op(kind);
    return expr;
}

static ASTExpr* parse_int_literal(Lexer* l)
{
    ASTExpr* expr = new_expr(l, EXPR_CONSTANT);
    expr->expr.constant.kind = CONSTANT_INTEGER;

    // TODO: Deal with the prefix for hex, octal, binary

    uint64_t val = 0;
    for(uint32_t i = 0; i < expr->loc.len; ++i)
    {
        uint64_t prev = val;
        val *= 10;
        val += expr->loc.start[i] - '0';
        if(prev > val)
        {
            parser_error(l, "Integer value exceeds maximum possible 64 bit value.");
            advance(l);
            return BAD_EXPR;
        }
    }

    expr->expr.constant.val.i = val;
    expr->type = val > 0xFFFFFFFF ? g_type_ulong : g_type_uint;

    // TODO: Deal with the suffix.
    advance(l);
    return expr;
}

static ASTExpr* parse_bool_literal(Lexer* l)
{
    ASTExpr* expr = new_expr(l, EXPR_CONSTANT);
    expr->expr.constant.kind = CONSTANT_BOOL;
    expr->expr.constant.val.i = peek(l)->kind == TOKEN_TRUE ? 1 : 0;
    expr->type = g_type_bool;
    advance(l);
    return expr;
}

static ASTExpr* parse_char_literal(Lexer* l)
{
    ASTExpr* expr = new_expr(l, EXPR_CONSTANT);
    expr->expr.constant.kind = CONSTANT_INTEGER;
    expr->expr.constant.val.i = peek(l)->chr.val;
    expr->type = g_type_ubyte;
    advance(l);
    return expr;
}

static ASTExpr* parse_string_literal(Lexer* l)
{
    ASTExpr* expr = new_expr(l, EXPR_CONSTANT);
    expr->expr.constant.kind = CONSTANT_STRING;

    // TODO: Add capability to concat multiple string literals if they are
    //       side-by-side
    expr->expr.constant.val.s = peek(l)->str.val;
    expr->type = type_pointer_to(g_type_ubyte);
    advance(l);
    return expr;
}


static ExprParseRule expr_rules[__TOKEN_COUNT] = {
    [TOKEN_IDENT]           = { parse_identifier_expr, NULL, PREC_NONE },
    [TOKEN_INT_LITERAL]     = { parse_int_literal, NULL, PREC_NONE },
    [TOKEN_CHAR_LITERAL]    = { parse_char_literal, NULL, PREC_NONE },
    [TOKEN_FLOAT_LITERAL]   = { NULL, NULL, PREC_NONE },
    [TOKEN_STRING_LITERAL]  = { parse_string_literal, NULL, PREC_NONE },
    [TOKEN_AMP]             = { parse_unary_prefix, parse_binary, PREC_BIT_AND },
    [TOKEN_ASTERISK]        = { parse_unary_prefix, parse_binary, PREC_MUL_DIV_MOD },
    [TOKEN_LOG_NOT]         = { parse_unary_prefix, NULL, PREC_UNARY_PREFIX },
    [TOKEN_BIT_NOT]         = { parse_unary_prefix, NULL, PREC_UNARY_PREFIX },
    [TOKEN_BIT_OR]          = { NULL, parse_binary, PREC_BIT_OR },
    [TOKEN_BIT_XOR]         = { NULL, parse_binary, PREC_BIT_XOR },
    [TOKEN_ASSIGN]          = { NULL, parse_binary, PREC_ASSIGN },
    [TOKEN_LT]              = { NULL, parse_binary, PREC_RELATIONAL },
    [TOKEN_GT]              = { NULL, parse_binary, PREC_RELATIONAL },
    [TOKEN_DIV]             = { NULL, parse_binary, PREC_MUL_DIV_MOD },
    [TOKEN_DOT]             = { NULL, parse_member_access, PREC_PRIMARY_POSTFIX },
    // [TOKEN_LBRACE]          = { NULL, NULL, PREC_NONE },
    [TOKEN_LBRACKET]        = { NULL, parse_array_access, PREC_PRIMARY_POSTFIX },
    [TOKEN_LPAREN]          = { parse_paren_expr, parse_call, PREC_PRIMARY_POSTFIX },
    [TOKEN_ADD]             = { NULL, parse_binary, PREC_ADD_SUB },
    [TOKEN_SUB]             = { parse_unary_prefix, parse_binary, PREC_ADD_SUB },
    [TOKEN_MODULO]          = { NULL, parse_binary, PREC_MUL_DIV_MOD },
    [TOKEN_QUESTION]        = { NULL, NULL, PREC_TERNARY },
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
    [TOKEN_INCREM]          = { parse_unary_prefix, NULL, PREC_PRIMARY_POSTFIX },
    [TOKEN_DECREM]          = { parse_unary_prefix, NULL, PREC_PRIMARY_POSTFIX },

    [TOKEN_AS]              = { NULL, parse_cast, PREC_UNARY_PREFIX },
    [TOKEN_FALSE]           = { parse_bool_literal, NULL, PREC_NONE },
    [TOKEN_TRUE]            = { parse_bool_literal, NULL, PREC_NONE },
};
