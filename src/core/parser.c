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
static bool parse_attributes(Lexer* l, AttrDA* attrs);
static bool parse_module_path(Lexer* l, ModulePath* out);
static bool parse_function_decl(Lexer* l, Visibility vis, AttrDA attrs, bool is_extern);
static bool parse_import(Lexer* l, Visibility vis);
static bool parse_module_decl(Lexer* l, Visibility vis);
static bool parse_func_signature(Lexer* l, FuncSignature* sig, bool allow_unnamed);
static bool parse_global_var_decl(Lexer* l, Visibility vis, AttrDA attrs, bool is_extern);

// Types
static Visibility  parse_visibility(Lexer* l);
static Object*     parse_bitfield_decl(Lexer* l, Visibility vis, AttrDA attrs);
static Object*     parse_enum_decl(Lexer* l, Visibility vis, AttrDA attrs);
static Object*     parse_struct_decl(Lexer* l, ObjKind kind, Visibility vis, AttrDA attrs, bool nested);
static Object*     parse_typedef(Lexer* l, Visibility vis, AttrDA attrs);
static bool        parse_type(Lexer* l, TypeLoc* type_loc);
static ObjVar*     parse_var_declaration(Lexer* l, VarKind kind, Visibility vis);

// Statements
static ASTStmt* parse_stmt(Lexer* l);
static ASTStmt* parse_stmt_block(Lexer* l);
static ASTStmt* parse_break_continue(Lexer* l, StmtKind kind);
static ASTStmt* parse_for(Lexer* l);
static ASTStmt* parse_if(Lexer* l);
static ASTStmt* parse_return(Lexer* l);
static ASTStmt* parse_switch(Lexer* l);
static ASTStmt* parse_while(Lexer* l);
static ASTStmt* parse_ct_assert(Lexer* l);
static ASTStmt* parse_labeled_stmt(Lexer* l);
static ASTStmt* parse_expr_stmt(Lexer* l);
static ASTStmt* parse_declaration(Lexer* l);

// Expressions
static ASTExpr* parse_expr_with_prec(Lexer* l, OpPrecedence precedence, ASTExpr* left);
static inline ASTExpr* parse_expr(Lexer* l)
{
    return parse_expr_with_prec(l, PREC_ASSIGN, NULL);
}
static ASTExpr* parse_binary(Lexer* l, ASTExpr* lhs);
static ASTExpr* parse_call(Lexer* l, ASTExpr* func_expr);
static ASTExpr* parse_cast(Lexer* l);
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
static ASTExpr* parse_bool_literal(Lexer* l);
static ASTExpr* parse_nullptr(Lexer* l);

// Inline helpers
static inline Token*   peek_prev(Lexer* l)     { return lexer_prev(l); }
static inline Token*   peek(Lexer* l)          { return lexer_peek(l); }
static inline Token*   peek_next(Lexer* l)     { return lexer_next(l); }
static inline Token*   UNUSED peek_nextnext(Lexer* l) { return lexer_nextnext(l); }

static inline bool     tok_equal(Lexer* lexer, TokenKind kind);
static inline void     parser_error(Lexer* lexer, const char* message, ...);
static inline void     eof_error(Lexer* l, TokenKind expected);
static inline bool     expect(Lexer* l, TokenKind kind);
static inline bool     expect_ident(Lexer* l);
static inline void     advance(Lexer* l);
static inline bool     try_consume(Lexer* l, TokenKind kind);
static inline bool     consume(Lexer* l, TokenKind kind);
static inline void     recover_to(Lexer* l, const TokenKind stopping_kinds[], size_t count);
static inline void     recover_top_level(Lexer* l, uint32_t prev_col);

static inline ASTStmt* new_stmt(Lexer* l, StmtKind kind);
static inline ASTExpr* new_expr(ExprKind kind);
static inline ASTExpr* new_constant(Lexer* l, ConstantKind kind);
static inline void     declare_obj(Lexer* l, Object* global);

static ASTStmt s_bad_stmt = {0};
static ASTExpr s_bad_expr = {0};
ASTStmt* g_bad_stmt = &s_bad_stmt;
ASTExpr* g_bad_expr = &s_bad_expr;
static ASTStmt s_nop_stmt = { .kind = STMT_NOP };
static ExprParseRule expr_rules[__TOKEN_COUNT];

#define ERROR_AND_RET(ret_val, ...)         do { parser_error(l, __VA_ARGS__); return ret_val; } while(0)
#define CONSUME_OR_RET(kind, ret_val)       do { if(!consume(l, kind)) return ret_val; } while(0)
#define EXPECT_OR_RET(kind, ret_val)        do { if(!expect(l, kind)) return ret_val; } while(0)
#define EXPECT_IDENT_OR_RET(ret_val)        do { if(!expect_ident(l)) return ret_val; } while(0)
#define ASSIGN_EXPR_OR_RET(expr, ret_val)   do { if(expr_is_bad(expr = parse_expr(l))) return ret_val; } while(0)
#define ASSIGN_STMT_OR_RET(stmt, ret_val)   do { if(stmt_is_bad(stmt = parse_stmt(l))) return ret_val; } while(0)
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
        uint32_t prev_col = peek(l)->line_col.col;
        if(!parse_top_level(l))
            recover_top_level(l, prev_col);
    }
}

static bool parse_top_level(Lexer* l)
{
    if(try_consume(l, TOKEN_SEMI)) return true; // Ignore semicolons
    AttrDA attrs = {0};
    if(!parse_attributes(l, &attrs)) return false;
    bool is_extern = try_consume(l, TOKEN_EXTERN);
    Visibility vis = parse_visibility(l);
    ObjKind kind = OBJ_UNION;
    Object* type;
    switch(peek(l)->kind)
    {
    case TOKEN_BITFIELD:
        if(is_extern) goto BAD_EXTERN;
        type = parse_bitfield_decl(l, vis, attrs);
        break;
    case TOKEN_CONST:
    case TOKEN_CT_CONST:
    case TOKEN_VAR:
        return parse_global_var_decl(l, vis, attrs, is_extern);
    case TOKEN_ENUM:
        if(is_extern) goto BAD_EXTERN;
        type = parse_enum_decl(l, vis, attrs);
        break;
    case TOKEN_FN:
        return parse_function_decl(l, vis, attrs, is_extern);
    case TOKEN_IMPORT:
        if(is_extern) goto BAD_EXTERN;
        return parse_import(l, vis);
    case TOKEN_MODULE:
        if(is_extern) goto BAD_EXTERN;
        return parse_module_decl(l, vis);
    case TOKEN_STRUCT:
        if(is_extern) goto BAD_EXTERN;
        kind = OBJ_STRUCT;
        FALLTHROUGH;
    case TOKEN_UNION:
        if(is_extern) goto BAD_EXTERN;
        type = parse_struct_decl(l, kind, vis, attrs, false);
        break;
    case TOKEN_TYPEDEF:
        if(is_extern) goto BAD_EXTERN;
        type = parse_typedef(l, vis, attrs);
        break;
    case TOKEN_CT_ASSERT: {
        if(is_extern) goto BAD_EXTERN;
        ASTStmt* res = parse_ct_assert(l);
        if(stmt_is_bad(res)) return false;
        res->next = l->module->ct_asserts;
        l->module->ct_asserts = res;
        return true;
    }
    default:
        ERROR_AND_RET(false, "Bad top level statement.");
    }

    if(type == NULL)
        return false;
    da_append(&l->module->types, type);
    declare_obj(l, type);
    return true;

BAD_EXTERN:
    ERROR_AND_RET(false, "'extern' is not applicable here. Please use extern for global variables and functions only.");
}

static bool parse_attributes(Lexer* l, AttrDA* attrs)
{
    while(tok_equal(l, TOKEN_ATTRIBUTE_IDENT))
    {
        Attr attr = {0};
        attr.symbol = peek(l)->sym;
        attr.loc = peek(l)->loc;
        advance(l);
        for(AttrKind i = 0; i < __ATTR_COUNT; ++i)
        {
            if(g_attr_list[i] == attr.symbol)
            {
                attr.kind = i;
                goto FOUND;
            }
        }

        attr.kind = ATTR_CUSTOM;
        SIC_TODO_MSG("User attributes not implemented");
FOUND:
        // TODO: parse arguments
        da_append(attrs, attr);
    }
    da_compact(attrs);
    return true;
}

static bool parse_module_path(Lexer* l, ModulePath* out)
{
    do
    {
        EXPECT_IDENT_OR_RET(false);
        da_reserve(out, out->size + 1);
        out->data[out->size++] = (SymbolLoc){ .sym = peek(l)->sym, .loc = peek(l)->loc };
        advance(l);
    } while(try_consume(l, TOKEN_NAMESPACE));

    da_compact(out);
    return true;
}

static bool parse_function_decl(Lexer* l, Visibility vis, AttrDA attrs, bool is_extern)
{
    advance(l);
    EXPECT_IDENT_OR_RET(false);
    ObjFunc* func = CALLOC_STRUCT(ObjFunc);
    func->header.symbol = peek(l)->sym;
    func->header.loc = peek(l)->loc;
    func->header.kind = OBJ_FUNC;
    func->header.visibility = vis;
    func->header.attrs = attrs;
    func->header.module = l->module;
    func->is_extern = is_extern;
    advance(l);

    CONSUME_OR_RET(TOKEN_LPAREN, false);
    if(!parse_func_signature(l, &func->signature, false))
        return false;

    func->func_type = type_func_ptr(&func->signature);
    func->func_type->visibility = vis;
    da_append(&l->module->funcs, func);
    declare_obj(l, &func->header);

    if(try_consume(l, TOKEN_SEMI))
        return true;

    EXPECT_OR_RET(TOKEN_LBRACE, false);
    func->body = parse_stmt_block(l);
    return !stmt_is_bad(func->body);
}

static bool parse_import(Lexer* l, Visibility vis)
{
    advance(l);
    ModulePath path = {0};

    while(true)
    {
        if(try_consume(l, TOKEN_LBRACE))
        {
            da_compact(&path);
            SIC_TODO();
        }
        if(try_consume(l, TOKEN_ASTERISK))
        {
            da_reserve(&path, path.size + 1);
            path.data[path.size++] = (SymbolLoc){ .sym = NULL, .loc = peek_prev(l)->loc };
            da_compact(&path);
            ObjImport* import = CALLOC_STRUCT(ObjImport);
            import->header.loc = peek_prev(l)->loc;
            import->header.kind = OBJ_IMPORT;
            import->header.visibility = vis;
            import->header.module = l->module;
            import->unresolved = path;
            da_append(&l->module->imports, import);
            break;
        }

        EXPECT_IDENT_OR_RET(false);
        da_reserve(&path, path.size + 1);
        path.data[path.size++] = (SymbolLoc){ .sym = peek(l)->sym, .loc = peek(l)->loc };
        advance(l);
        if(try_consume(l, TOKEN_NAMESPACE))
            continue;

        if(try_consume(l, TOKEN_AS))
        {
            EXPECT_IDENT_OR_RET(false);
            advance(l);
        }

        da_compact(&path);
        ObjImport* import = CALLOC_STRUCT(ObjImport);
        import->header.symbol = peek_prev(l)->sym;
        import->header.loc = peek_prev(l)->loc;
        import->header.kind = OBJ_IMPORT;
        import->header.visibility = vis;
        import->header.module = l->module;
        import->unresolved = path;
        da_append(&l->module->imports, import);
        break;
    }


    CONSUME_OR_RET(TOKEN_SEMI, false);
    return true;
}

static bool parse_module_decl(Lexer* l, Visibility vis)
{
    advance(l);
    EXPECT_IDENT_OR_RET(false);
    ObjModule* parent = l->module;
    Symbol mod_name = peek(l)->sym;
    SourceLoc mod_loc = peek(l)->loc;
    Object* prev;
    advance(l);
    if((prev = hashmap_get(&parent->module_ns, mod_name)) != NULL)
    {
        sic_diagnostic_at(DIAG_ERROR, mod_loc, "Module with name \'%s\' already exists.", mod_name);
        sic_diagnostic_at(DIAG_NOTE, prev->loc, "Previous definition here.");
        return false;
    }

    ObjModule* new_mod = CALLOC_STRUCT(ObjModule);
    new_mod->header.symbol = mod_name;
    new_mod->header.loc = mod_loc;
    new_mod->header.kind = OBJ_MODULE;
    new_mod->header.visibility = vis;
    new_mod->header.module = l->module;
    new_mod->parent = parent;

    da_append(&parent->submodules, new_mod);
    hashmap_put(&parent->module_ns, mod_name, &new_mod->header);

    if(try_consume(l, TOKEN_LBRACE))
    {
        new_mod->is_inline = true;
        l->module = new_mod;
        while(!try_consume(l, TOKEN_RBRACE))
        {
            if(tok_equal(l, TOKEN_EOF))
            {
                l->module = parent;
                eof_error(l, TOKEN_RBRACE);
                return false;
            }

            uint32_t prev_col = peek(l)->line_col.col;
            if(!parse_top_level(l))
                recover_top_level(l, prev_col);
        }
        l->module = parent;
        return true;
    }
    
    CONSUME_OR_RET(TOKEN_SEMI, false);
    FileId id = find_and_open_module_path(new_mod);
    if(id == FILE_NULL)
        return true;

    parse_source_file(id);
    return true;
}

static bool parse_func_signature(Lexer* l, FuncSignature* sig, bool allow_unnamed)
{
    sig->is_var_arg = false;
    while(!try_consume(l, TOKEN_RPAREN))
    {
        if(tok_equal(l, TOKEN_EOF))
        {
            eof_error(l, TOKEN_RPAREN);
            return false;
        }
        
        if(sig->params.size > 0)
            CONSUME_OR_RET(TOKEN_COMMA, false);

        if(tok_equal(l, TOKEN_ELLIPSIS))
        {
            advance(l);
            CONSUME_OR_RET(TOKEN_RPAREN, false);
            sig->is_var_arg = true;
            break;
        }

        ObjVar* p = CALLOC_STRUCT(ObjVar);
        p->header.kind = OBJ_VAR;
        p->header.visibility = VIS_PUBLIC;
        p->kind = VAR_PARAM;
        p->is_const_binding = true;

        if(peek(l)->kind == TOKEN_UNDERSCORE && peek_next(l)->kind == TOKEN_COLON)
        {
            p->header.loc = peek(l)->loc;
            advance(l);
            advance(l);
        }
        else if(peek(l)->kind == TOKEN_IDENT && peek_next(l)->kind == TOKEN_COLON)
        {
            p->header.loc = peek(l)->loc;
            p->header.symbol = peek(l)->sym;
            advance(l);
            advance(l);
        }
        else if(!allow_unnamed)
            ERROR_AND_RET(false, "Expected parameter name or '_' for unused parameter.");

        if(!parse_type(l, &p->type_loc))
            return false;

        if(allow_unnamed)
            p->header.loc = p->type_loc.loc;

        da_append(&sig->params, p);
    }
    
    da_compact(&sig->params);
    return parse_type(l, &sig->ret_type);
}

static bool parse_global_var_decl(Lexer* l, Visibility vis, AttrDA attrs, bool is_extern)
{
    ObjVar* var = parse_var_declaration(l, VAR_GLOBAL, vis);
    if(var == NULL) return false;
    var->header.attrs = attrs;
    var->is_extern = is_extern;
    da_append(&l->module->vars, var);
    declare_obj(l, &var->header);
    return true;
}

static Visibility parse_visibility(Lexer* l)
{
    switch(peek(l)->kind)
    {
    case TOKEN_PRIV:
        advance(l);
        return VIS_PRIVATE;
    case TOKEN_PUB:
        advance(l);
        return VIS_PUBLIC;
    default:
        return VIS_DEFAULT;
    }
}

static Object* parse_bitfield_decl(UNUSED Lexer* l, UNUSED Visibility vis, UNUSED AttrDA attrs)
{
    SIC_TODO();
}

static Object* parse_enum_decl(Lexer* l, Visibility vis, AttrDA attrs)
{
    advance(l);
    bool is_distinct = try_consume(l, TOKEN_DISTINCT);
    EXPECT_IDENT_OR_RET(NULL);
    ObjEnum* enum_ = CALLOC_STRUCT(ObjEnum);
    enum_->header.symbol = peek(l)->sym;
    enum_->header.loc = peek(l)->loc;
    enum_->header.visibility = vis;
    enum_->header.kind = OBJ_ENUM;
    enum_->header.attrs = attrs;
    enum_->header.module = l->module;
    Type* type = enum_->type_ref = CALLOC_STRUCT(Type);
    type->kind = is_distinct ? TYPE_ENUM_DISTINCT : TYPE_ENUM;
    type->visibility = vis;
    type->enum_ = enum_;
    advance(l);

    if(try_consume(l, TOKEN_COLON) && !parse_type(l, &enum_->underlying))
        return NULL;

    CONSUME_OR_RET(TOKEN_LBRACE, NULL);
    if(tok_equal(l, TOKEN_RBRACE))
    {
        sic_error_at(enum_->header.loc, "Enum declaration is empty.");
        return NULL;
    }

    while(!try_consume(l, TOKEN_RBRACE))
    {
        EXPECT_IDENT_OR_RET(NULL);
        ObjEnumValue* value = CALLOC_STRUCT(ObjEnumValue);
        value->header.symbol = peek(l)->sym;
        value->header.loc = peek(l)->loc;
        value->header.visibility = vis;
        value->header.kind = OBJ_ENUM_VALUE;
        value->enum_type = type;
        advance(l);
        if(try_consume(l, TOKEN_ASSIGN))
            ASSIGN_EXPR_OR_RET(value->raw_value, NULL);
        da_append(&enum_->values, value);
        if(!try_consume(l, TOKEN_COMMA))
            EXPECT_OR_RET(TOKEN_RBRACE, NULL);
    }

    da_compact(&enum_->values);
    return &enum_->header;
}

static bool parse_struct_members(Lexer* l, ObjKind kind, ObjStruct* struct_)
{
    CONSUME_OR_RET(TOKEN_LBRACE, NULL);
    if(tok_equal(l, TOKEN_RBRACE))
    {
        sic_error_at(struct_->header.loc, "%s declaration is empty.", kind == OBJ_STRUCT ? "Struct" : "Union");
        return false;
    }
    while(!tok_equal(l, TOKEN_RBRACE))
    {
        // TODO: Allow anonymous members with _. This will be used for padding.
        EXPECT_IDENT_OR_RET(false);
        ObjVar* member = CALLOC_STRUCT(ObjVar);
        member->header.symbol = peek(l)->sym;
        member->header.loc = peek(l)->loc;
        member->header.visibility = VIS_PUBLIC; // TODO: Add member visibility, should be easy.
        member->header.kind = OBJ_VAR;
        member->kind = VAR_MEMBER;
        da_append(&struct_->members, member);
        advance(l);
        CONSUME_OR_RET(TOKEN_COLON, false);
        if(!parse_type(l, &member->type_loc)) return false;


        if(!try_consume(l, TOKEN_COMMA)) break;
    }
    return consume(l, TOKEN_RBRACE);
}

static Object* parse_struct_decl(Lexer* l, ObjKind kind, Visibility vis, AttrDA attrs, bool nested)
{
    advance(l);
    if(!nested)
    {
        EXPECT_IDENT_OR_RET(NULL);
        advance(l);
    }
    ObjStruct* struct_ = CALLOC_STRUCT(ObjStruct);
    struct_->header.symbol = peek_prev(l)->sym;
    struct_->header.loc = peek_prev(l)->loc;
    struct_->header.visibility = vis;
    struct_->header.kind = kind;
    struct_->header.attrs = attrs;
    struct_->header.module = l->module;
    Type* type = struct_->type_ref = CALLOC_STRUCT(Type);
    type->kind = kind == OBJ_STRUCT ? TYPE_STRUCT : TYPE_UNION;
    type->visibility = vis;
    type->struct_ = struct_;
    type->canonical = type;


    if(!parse_struct_members(l, kind, struct_)) return NULL;

    da_compact(&struct_->members);
    return &struct_->header;
}


static Object* parse_typedef(Lexer* l, Visibility vis, AttrDA attrs)
{
    advance(l);
    bool is_distinct = try_consume(l, TOKEN_DISTINCT);
    EXPECT_IDENT_OR_RET(NULL);
    ObjTypedef* typedef_ = CALLOC_STRUCT(ObjTypedef);
    typedef_->header.symbol = peek(l)->sym;
    typedef_->header.loc = peek(l)->loc;
    typedef_->header.visibility = vis;
    typedef_->header.kind = OBJ_TYPEDEF;
    typedef_->header.attrs = attrs;
    typedef_->header.module = l->module;
    advance(l);

    Type* type = typedef_->type_ref = CALLOC_STRUCT(Type);
    type->kind = is_distinct ? TYPE_ALIAS_DISTINCT : TYPE_ALIAS;
    type->visibility = vis;
    type->typedef_ = typedef_;

    if(!consume(l, TOKEN_ASSIGN) || !parse_type(l, &typedef_->alias) ||
       !consume(l, TOKEN_SEMI))
    {
        typedef_->header.kind = OBJ_INVALID;
        return &typedef_->header;
    }
    return &typedef_->header;
}

static Type* parse_type_internal(Lexer* l)
{
    Type* ty = NULL;
    TypeQualifiers quals = TYPE_QUAL_NONE;
    bool should_copy = false;

RETRY:
    switch(peek(l)->kind)
    {
    case TOKEN_ASTERISK:
        advance(l);
        ty = parse_type_internal(l);
        ty = type_pointer_to(ty);
        break;
    case TOKEN_LBRACKET: {
        advance(l);
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

        ty = parse_type_internal(l);
        ty = type_array_of(ty, size_expr);
        break;
    }
    case TOKEN_CONST:
        if(quals & TYPE_QUAL_CONST)
            sic_diagnostic_at(DIAG_WARNING, peek(l)->loc, "Duplicate const qualifier.");
        quals |= TYPE_QUAL_CONST;
        advance(l);
        goto RETRY;
    case TOKEN_VOID:
    case TOKEN_BOOL:
    case TOKEN_CHAR:
    case TOKEN_CHAR16:
    case TOKEN_CHAR32:
    case TOKEN_BYTE:
    case TOKEN_UBYTE:
    case TOKEN_SHORT:
    case TOKEN_USHORT:
    case TOKEN_INT:
    case TOKEN_UINT:
    case TOKEN_LONG:
    case TOKEN_ULONG:
    case TOKEN_INT128:
    case TOKEN_UINT128:
    case TOKEN_FLOAT:
    case TOKEN_DOUBLE:
    case TOKEN_IPTR:
    case TOKEN_UPTR:
    case TOKEN_ISIZE:
    case TOKEN_USIZE:
        ty = type_from_token(peek(l)->kind);
        advance(l);
        should_copy = true;
        break;
    case TOKEN_FN:
        advance(l);
        CONSUME_OR_RET(TOKEN_LPAREN, false);
        FuncSignature* sig = CALLOC_STRUCT(FuncSignature);
        if(!parse_func_signature(l, sig, true))
            return false;
        ty = type_func_ptr(sig);
        break;
    case TOKEN_CT_TYPEOF:
        advance(l);
        CONSUME_OR_RET(TOKEN_LPAREN, false);
        ty = CALLOC_STRUCT(Type);
        ty->kind = TYPE_TYPEOF;
        ty->canonical = ty;
        ASSIGN_EXPR_OR_RET(ty->type_of, false);
        CONSUME_OR_RET(TOKEN_RPAREN, false);
        break;
    case TOKEN_IDENT:
        ty = CALLOC_STRUCT(Type);
        ty->kind = TYPE_UNRESOLVED_USER;
        if(!parse_module_path(l, &ty->unresolved))
            return false;
        break;
    default:
        ERROR_AND_RET(false, "Expected typename.");
    }

    if(should_copy)
        ty = type_apply_qualifiers(ty, quals);
    else
        ty->qualifiers |= quals;

    return ty;
}

static bool parse_type(Lexer* l, TypeLoc* type_loc)
{
    type_loc->loc = peek(l)->loc;
    type_loc->type = parse_type_internal(l);
    type_loc->loc = extend_loc(type_loc->loc, peek_prev(l)->loc);
    return type_loc->type != NULL;
}

static ObjVar* parse_var_declaration(Lexer* l, VarKind kind, Visibility vis)
{
    advance(l);
    EXPECT_IDENT_OR_RET(NULL);
    ObjVar* var = CALLOC_STRUCT(ObjVar);
    var->header.symbol = peek(l)->sym;
    var->header.loc = peek(l)->loc;
    var->header.kind = OBJ_VAR;
    var->header.visibility = vis;
    var->kind = peek_prev(l)->kind == TOKEN_CT_CONST ? VAR_CT_CONST : kind;
    var->is_const_binding = peek_prev(l)->kind != TOKEN_VAR;
    advance(l);
    if(try_consume(l, TOKEN_COLON) && !parse_type(l, &var->type_loc))
        return NULL;
    if(try_consume(l, TOKEN_ASSIGN))
    {
        if(peek(l)->kind == TOKEN_VOID && peek_next(l)->kind == TOKEN_SEMI)
        {
            advance(l);
            var->uninitialized = true;
        }
        else
            ASSIGN_EXPR_OR_RET(var->initial_val, NULL);
    }

    CONSUME_OR_RET(TOKEN_SEMI, NULL);
    return var;
}


static const TokenKind s_stmt_recover_list[] = { TOKEN_SEMI, TOKEN_RBRACE };

static ASTStmt* parse_stmt(Lexer* l)
{
    ASTStmt* stmt;
    switch(peek(l)->kind)
    {
    case TOKEN_IDENT:
        stmt = peek_next(l)->kind == TOKEN_COLON ? parse_labeled_stmt(l) : parse_expr_stmt(l);
        break;
    case TOKEN_SEMI:
        advance(l);
        return NOP_STMT;
    case TOKEN_LBRACE:
        stmt = parse_stmt_block(l);
        return stmt; // If stmt is invalid, we know we hit the EOF, see parse_stmt_block
    case TOKEN_BREAK:
        stmt = parse_break_continue(l, STMT_BREAK);
        break;
    case TOKEN_CONST:
    case TOKEN_VAR:
    case TOKEN_CT_CONST:
        stmt = parse_declaration(l);
        break;
    case TOKEN_CONTINUE:
        stmt = parse_break_continue(l, STMT_CONTINUE);
        break;
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
    case TOKEN_CT_ASSERT:
        stmt = parse_ct_assert(l);
        break;
    case TOKEN_CT_UNREACHABLE:
        stmt = new_stmt(l, STMT_CT_UNREACHABLE);
        advance(l);
        return stmt;
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
        {
            eof_error(l, TOKEN_RBRACE);
            return BAD_STMT;
        }
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
    if(try_consume(l, TOKEN_IDENT))
    {
        stmt->stmt.break_cont.label.sym = peek_prev(l)->sym;
        stmt->stmt.break_cont.label.loc = peek_prev(l)->loc;
    }
    CONSUME_OR_RET(TOKEN_SEMI, BAD_STMT);
    return stmt;
}

static ASTStmt* parse_for(Lexer* l)
{
    ASTStmt* stmt = new_stmt(l, STMT_FOR);
    ASTFor* for_stmt = &stmt->stmt.for_;
    advance(l);

    TypeLoc type_loc;

    if(!parse_type(l, &type_loc)) return BAD_STMT;
    EXPECT_IDENT_OR_RET(BAD_STMT);

    ObjVar* var = for_stmt->loop_var = CALLOC_STRUCT(ObjVar);
    var->header.symbol = peek(l)->sym;
    var->header.loc = peek(l)->loc;
    var->header.visibility = VIS_PUBLIC;
    var->header.kind = OBJ_VAR;
    var->kind = VAR_LOCAL;
    var->type_loc = type_loc;
    advance(l);

    CONSUME_OR_RET(TOKEN_IN, BAD_STMT);
    ASSIGN_EXPR_OR_RET(for_stmt->collection, BAD_STMT);
    EXPECT_OR_RET(TOKEN_LBRACE, BAD_STMT);
    for_stmt->body = parse_stmt_block(l);
    if(stmt_is_bad(for_stmt->body)) return BAD_STMT;
    return stmt;
}

static ASTStmt* parse_if(Lexer* l)
{
    ASTStmt* stmt = new_stmt(l, STMT_IF);
    ASTIf* if_stmt = &stmt->stmt.if_;
    advance(l);

    ASSIGN_EXPR_OR_RET(if_stmt->cond, BAD_STMT);
    EXPECT_OR_RET(TOKEN_LBRACE, BAD_STMT);
    if_stmt->then_stmt = parse_stmt_block(l);
    if(stmt_is_bad(if_stmt->then_stmt)) return BAD_STMT;
    if(try_consume(l, TOKEN_ELSE))
    {
        if(tok_equal(l, TOKEN_IF))
            if_stmt->else_stmt = parse_if(l);
        else
        {
            EXPECT_OR_RET(TOKEN_LBRACE, BAD_STMT);
            if_stmt->else_stmt = parse_stmt_block(l);
        }
        if(stmt_is_bad(if_stmt->else_stmt)) return BAD_STMT;
    }

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
    ASSIGN_EXPR_OR_RET(stmt->stmt.switch_.expr, BAD_STMT);
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
            eof_error(l, TOKEN_RBRACE);
            return false;
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
    
    ASSIGN_EXPR_OR_RET(while_stmt->cond, BAD_STMT);
    EXPECT_OR_RET(TOKEN_LBRACE, BAD_STMT);
    while_stmt->body = parse_stmt_block(l);
    if(stmt_is_bad(while_stmt->body)) return BAD_STMT;

    return stmt;
}

static ASTStmt* parse_ct_assert(Lexer* l)
{
    ASTStmt* stmt = new_stmt(l, STMT_CT_ASSERT);
    ASTCtAssert* assert_ = &stmt->stmt.ct_assert;
    advance(l);
    CONSUME_OR_RET(TOKEN_LPAREN, BAD_STMT);
    ASSIGN_EXPR_OR_RET(assert_->cond, BAD_STMT);
    CONSUME_OR_RET(TOKEN_COMMA, BAD_STMT);
    ASSIGN_EXPR_OR_RET(assert_->err_msg, BAD_STMT);
    CONSUME_OR_RET(TOKEN_RPAREN, BAD_STMT);
    return stmt;
}

static ASTStmt* parse_labeled_stmt(Lexer* l)
{
    DBG_ASSERT(tok_equal(l, TOKEN_IDENT));
    SymbolLoc label;
    label.sym = peek(l)->sym;
    label.loc = extend_loc(peek(l)->loc, peek_next(l)->loc);
    advance(l);
    advance(l);

    ASTStmt* stmt = parse_stmt(l);
    switch(stmt->kind)
    {
    case STMT_INVALID:
        return BAD_STMT;
    case STMT_FOR:
        stmt->stmt.for_.label = label;
        return stmt;
    case STMT_SWITCH:
        stmt->stmt.switch_.label = label;
        return stmt;
    case STMT_WHILE:
        stmt->stmt.while_.label = label;
        return stmt;
    default:
        sic_error_at(label.loc, "Labels can only be applied to for, switch, and while statements.");
        return BAD_STMT;
    }
}

static ASTStmt* parse_expr_stmt(Lexer* l)
{
    ASTStmt* stmt;
    ASTExpr* expr;
    ASSIGN_EXPR_OR_RET(expr, BAD_STMT);

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


static ASTStmt* parse_declaration(Lexer* l)
{
    ASTStmt* stmt = new_stmt(l, STMT_DECLARATION);
    stmt->stmt.declaration = parse_var_declaration(l, VAR_LOCAL, VIS_PUBLIC);
    if(stmt->stmt.declaration == NULL) return BAD_STMT;
    return stmt;
}

static ASTExpr* parse_expr_with_prec(Lexer* l, OpPrecedence precedence, ASTExpr* left)
{
    if(left == NULL)
    {
        ExprPrefixFunc prefix = expr_rules[peek(l)->kind].prefix;
        if(prefix == NULL)
        {
            sic_diagnostic_after(DIAG_ERROR, peek_prev(l)->loc, NULL, "Expected an expression.");
            return BAD_EXPR;
        }
        left = prefix(l);
    }

    while(!expr_is_bad(left))
    {
        ExprParseRule* rule = expr_rules + peek(l)->kind;
        if(rule->precedence < precedence)
            break;

        DBG_ASSERT(rule->infix != NULL);
        left = rule->infix(l, left);
    }
    return left;
}

static ASTExpr* parse_binary(Lexer* l, ASTExpr* lhs)
{
    TokenKind kind = peek(l)->kind;
    advance(l);

    ASTExpr* rhs;
    OpPrecedence rhs_pref = expr_rules[kind].precedence;
    if(rhs_pref != PREC_ASSIGN)
        rhs_pref++;

    rhs = parse_expr_with_prec(l, rhs_pref, NULL);
    if(expr_is_bad(rhs))
        return BAD_EXPR;

    ASTExpr* binary = new_expr(EXPR_BINARY);
    binary->loc = extend_loc(lhs->loc, rhs->loc);
    binary->expr.binary.lhs = lhs;
    binary->expr.binary.rhs = rhs;
    binary->expr.binary.kind = tok_to_binary_op(kind);
    return binary;
}


static ASTExpr* parse_call(Lexer* l, ASTExpr* func_expr)
{
    ASTExpr* call = new_expr(EXPR_FUNC_CALL);
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

    call->loc = extend_loc(func_expr->loc, peek_prev(l)->loc);

    da_compact(args);
    return call;
}

static ASTExpr* parse_cast(Lexer* l)
{
    ASTExpr* cast = new_expr(EXPR_CAST);
    cast->loc = peek(l)->loc;
    advance(l);

    CONSUME_OR_RET(TOKEN_LT, BAD_EXPR);
    
    TypeLoc type_loc;
    if(!parse_type(l, &type_loc))
        return BAD_EXPR;
    cast->type = type_loc.type;

    CONSUME_OR_RET(TOKEN_GT, BAD_EXPR);
    CONSUME_OR_RET(TOKEN_LPAREN, BAD_EXPR);
    ASSIGN_EXPR_OR_RET(cast->expr.cast.inner, BAD_EXPR);
    CONSUME_OR_RET(TOKEN_RPAREN, BAD_EXPR);
    cast->loc = extend_loc(cast->loc, peek_prev(l)->loc);
    return cast;
}

static ASTExpr* parse_ternary(Lexer* l, ASTExpr* cond)
{
    ASTExpr* tern = new_expr(EXPR_TERNARY);
    advance(l);
    if(tok_equal(l, TOKEN_COLON))
        advance(l);
    else if(expr_is_bad(tern->expr.ternary.then_expr = parse_expr(l)) ||
            !consume(l, TOKEN_COLON))
        return BAD_EXPR;
    
    tern->expr.ternary.else_expr = parse_expr_with_prec(l, PREC_TERNARY, NULL);
    if(expr_is_bad(tern->expr.ternary.else_expr))
        return BAD_EXPR;

    tern->loc = extend_loc(cond->loc, tern->expr.ternary.else_expr->loc);
    tern->expr.ternary.cond_expr = cond;
    return tern;
}

static ASTExpr* parse_array_access(Lexer* l, ASTExpr* array_expr)
{
    ASTExpr* access = new_expr(EXPR_ARRAY_ACCESS);
    access->expr.array_access.array_expr = array_expr;
    advance(l);

    ASTExpr* index_expr = parse_expr(l);
    if(expr_is_bad(index_expr) || !consume(l, TOKEN_RBRACKET))
        return BAD_EXPR;

    access->loc = extend_loc(array_expr->loc, peek_prev(l)->loc);
    access->expr.array_access.index_expr = index_expr;
    return access;
}

static ASTExpr* parse_member_access(Lexer* l, ASTExpr* struct_expr)
{
    ASTExpr* access = new_expr(tok_equal(l, TOKEN_ARROW) ? EXPR_UNRESOLVED_ARROW : EXPR_UNRESOLVED_DOT);
    advance(l);
    access->expr.unresolved_access.parent_expr = struct_expr;
    EXPECT_IDENT_OR_RET(BAD_EXPR);
    access->expr.unresolved_access.member.sym = peek(l)->sym;
    access->expr.unresolved_access.member.loc = peek(l)->loc;
    access->loc = extend_loc(struct_expr->loc, peek(l)->loc);
    advance(l);
    return access;
}

static ASTExpr* parse_incdec_postfix(Lexer* l, ASTExpr* left)
{
    ASTExpr* result = new_expr(EXPR_POSTFIX);
    result->expr.unary.inner = left;
    result->expr.unary.kind = tok_to_unary_op(peek(l)->kind);
    result->loc = extend_loc(left->loc, peek(l)->loc);
    advance(l);
    return result;
}

static ASTExpr* parse_struct_init_list(Lexer* l)
{
    ASTExpr* expr = new_expr(EXPR_STRUCT_INIT_LIST);
    expr->loc = peek(l)->loc;
    StructInitList* list = &expr->expr.struct_init;
    advance(l);
    while(true)
    {
        da_reserve(list, list->size + 1);
        StructInitEntry* entry = list->data + list->size;
        switch(peek(l)->kind)
        {
        case TOKEN_IDENT:
            list->size++;
            if(peek_next(l)->kind != TOKEN_COLON)
                break;
            entry->unresolved_member.sym = peek(l)->sym;
            entry->unresolved_member.loc = peek(l)->loc;
            advance(l);
            advance(l);
            break;
        case TOKEN_RBRACE:
            goto OUTER;
        case TOKEN_EOF:
            eof_error(l, TOKEN_RBRACE);
            return BAD_EXPR;
        default:
            list->size++;
            break;
        }
        ASSIGN_EXPR_OR_RET(entry->init_value, BAD_EXPR);

        if(!try_consume(l, TOKEN_COMMA))
            break;
    }
OUTER:
    da_compact(list);
    CONSUME_OR_RET(TOKEN_RBRACE, BAD_EXPR);
    expr->type = g_type_init_list;
    expr->loc = extend_loc(expr->loc, peek_prev(l)->loc);
    return expr;
}

static ASTExpr* parse_range(Lexer* l, ASTExpr* from)
{
    ASTExpr* expr = new_expr(EXPR_RANGE);
    ASTExprRange* range = &expr->expr.range;
    range->from = from;
    advance(l);
    range->inclusive = try_consume(l, TOKEN_ASSIGN);
    range->to = parse_expr_with_prec(l, PREC_TERNARY, NULL);
    if(expr_is_bad(range->to)) return BAD_EXPR;
    expr->loc = extend_loc(from->loc, range->to->loc);
    return expr;
}

static ASTExpr* parse_array_init_list(Lexer* l)
{
    ASTExpr* expr = new_expr(EXPR_ARRAY_INIT_LIST);
    expr->loc = peek(l)->loc;
    ArrInitList* list = &expr->expr.array_init;
    advance(l);
    while(!tok_equal(l, TOKEN_RBRACKET))
    {
        if(tok_equal(l, TOKEN_EOF))
        {
            eof_error(l, TOKEN_RBRACKET);
            return BAD_EXPR;
        }
        da_reserve(list, list->size + 1);
        ArrInitEntry* entry = list->data + list->size;
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
    expr->loc = extend_loc(expr->loc, peek_prev(l)->loc);
    expr->type = g_type_init_list;
    return expr;

}


static ASTExpr* parse_invalid(Lexer* l)
{
    advance(l);
    return BAD_EXPR;
}

static ASTExpr* parse_identifier_expr(Lexer* l)
{
    ASTExpr* expr = new_expr(EXPR_UNRESOLVED_IDENT);
    expr->loc = peek(l)->loc;
    if(!parse_module_path(l, &expr->expr.pre_sema_ident))
        return BAD_EXPR;
    expr->loc = extend_loc(expr->loc, peek_prev(l)->loc);
    return expr;
}

static ASTExpr* parse_paren_expr(Lexer* l)
{
    DBG_ASSERT(peek(l)->kind == TOKEN_LPAREN);
    SourceLoc start = peek(l)->loc;
    advance(l);
    ASTExpr* expr;
    ASSIGN_EXPR_OR_RET(expr, BAD_EXPR);
    if(tok_equal(l, TOKEN_COMMA))
    {
        ASTExpr* inner = expr;
        expr = new_expr(EXPR_TUPLE);
        expr->type = g_type_init_list;
        da_append(&expr->expr.tuple, inner);
        while(try_consume(l, TOKEN_COMMA))
        {
            ASSIGN_EXPR_OR_RET(inner, BAD_EXPR);
            da_append(&expr->expr.tuple, inner);
        }
        da_compact(&expr->expr.tuple);
    }
    CONSUME_OR_RET(TOKEN_RPAREN, BAD_EXPR);
    expr->loc = extend_loc(start, peek_prev(l)->loc);
    return expr;
}

static ASTExpr* parse_negation(Lexer* l)
{
    if(peek_next(l)->kind == TOKEN_DEC_INT_LITERAL)
        return parse_decimal_literal(l);

    ASTExpr* expr = new_expr(EXPR_UNARY);
    expr->loc = peek(l)->loc;
    expr->expr.unary.kind = UNARY_NEG;
    advance(l);
    ASTExpr* inner = parse_expr_with_prec(l, PREC_PRIMARY_POSTFIX, NULL);
    if(expr_is_bad(inner)) return BAD_EXPR;
    expr->expr.unary.inner = inner;
    return expr;
}

static ASTExpr* parse_unary_prefix(Lexer* l)
{
    ASTExpr* expr = new_expr(EXPR_UNARY);
    expr->loc = peek(l)->loc;
    TokenKind kind = peek(l)->kind;
    advance(l);
    expr->expr.unary.inner = parse_expr_with_prec(l, PREC_PRIMARY_POSTFIX, NULL);
    if(expr_is_bad(expr->expr.unary.inner))
        return BAD_EXPR;
    expr->expr.unary.kind = tok_to_unary_op(kind);
    expr->loc = extend_loc(expr->loc, peek_prev(l)->loc);
    return expr;

}

static ASTExpr* parse_pow_2_int_literal(Lexer* l, BitSize bits_per_digit)
{
    ASTExpr* expr = new_constant(l, CONSTANT_INTEGER);

    const char* src = peek(l)->start;
    Int128 val = (Int128){ 0, 0 };
    for(uint32_t i = 2; i < expr->loc.len; ++i)
    {
        if(src[i] == '_')
            continue;
        if(val.hi > (UINT64_MAX >> bits_per_digit))
            ERROR_AND_RET(BAD_EXPR, "Integer value exceeds maximum supported integer literal value.");
        uint64_t hex_val = g_hex_char_to_val[(uint8_t)src[i]] - 1;
        DBG_ASSERT(hex_val < 16);
        val = i128_add64(i128_shl64(val, bits_per_digit), hex_val);
    }

    expr->expr.constant.i = val;
    expr->type = g_type_pos_int_lit;

    advance(l);
    return expr;

}

static ASTExpr* parse_binary_literal(Lexer* l) { return parse_pow_2_int_literal(l, 1); }
static ASTExpr* parse_octal_literal(Lexer* l) { return parse_pow_2_int_literal(l, 3); }
static ASTExpr* parse_hexadecimal_literal(Lexer* l) { return parse_pow_2_int_literal(l, 4); }

static ASTExpr* parse_decimal_literal(Lexer* l)
{
    ASTExpr* expr = new_constant(l, CONSTANT_INTEGER);
    bool is_neg;
    if(tok_equal(l, TOKEN_SUB))
    {
        is_neg = true;
        advance(l);
        expr->loc = extend_loc(expr->loc, peek(l)->loc);
        expr->type = g_type_neg_int_lit;
    }
    else
    {
        is_neg = false;
        expr->type = g_type_pos_int_lit;
    }


    const char* src = peek(l)->start;
    Int128 val = (Int128){ 0, 0 };
    for(uint32_t i = 0; i < peek(l)->loc.len; ++i)
    {
        if(src[i] == '_')
            continue;
        uint64_t digit = src[i] - '0';
        uint64_t prev = val.hi;
        val = i128_add64(i128_mult64(val, 10), digit);
        if(val.hi < prev)
        {
            if(is_neg)
                sic_error_at(expr->loc, "Integer value is less than the minimum supported integer value.");
            else
                sic_error_at(expr->loc, "Integer value exceeds maximum supported integer value.");
            return BAD_EXPR;
        }
    }

    if(is_neg && i128_ucmp(val, INT128_MIN) > 0)
    {
        sic_error_at(expr->loc, "Integer value is less than the minimum supported integer value.");
        return BAD_EXPR;
    }

    expr->expr.constant.i = val;

    advance(l);
    return expr;
}


static ASTExpr* parse_char_literal(Lexer* l)
{
    ASTExpr* expr = new_constant(l, CONSTANT_CHAR);
    expr->expr.constant.c = peek(l)->chr.val;
    switch(peek(l)->chr.kind)
    {
    case TYPE_CHAR:
        expr->type = g_type_char;
        break;
    case TYPE_CHAR16:
        expr->type = g_type_char16;
        break;
    case TYPE_CHAR32:
        expr->type = g_type_char32;
        break;
    default:
        SIC_UNREACHABLE();
    }
    advance(l);
    return expr;
}

static ASTExpr* parse_float_literal(Lexer* l)
{
    ASTExpr* expr = new_constant(l, CONSTANT_FLOAT);
    const char* src = peek(l)->start;
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
    expr->expr.constant.f = val;
    advance(l);
    return expr;
}

static ASTExpr* parse_string_literal(Lexer* l)
{
    ASTExpr* expr = new_constant(l, CONSTANT_STRING);
    expr->expr.constant.str.val = peek(l)->str.val;
    expr->expr.constant.str.len = peek(l)->str.len;
    expr->expr.constant.str.kind = peek(l)->str.kind;
    expr->type = g_type_str_lit;
    advance(l);
    return expr;
}

static ASTExpr* parse_bool_literal(Lexer* l)
{
    ASTExpr* expr = new_constant(l, CONSTANT_BOOL);
    expr->expr.constant.b = peek(l)->kind == TOKEN_TRUE;
    expr->type = g_type_bool;
    advance(l);
    return expr;
}

static ASTExpr* parse_nullptr(Lexer* l)
{
    ASTExpr* expr = new_constant(l, CONSTANT_POINTER);
    expr->expr.constant.i = (Int128){ 0, 0 };
    expr->type = g_type_voidptr;
    advance(l);
    return expr;
}

static ASTExpr* parse_ct_alignof(Lexer* l)
{
    ASTExpr* expr = new_expr(EXPR_CT_ALIGNOF);
    expr->loc = peek(l)->loc;
    advance(l);
    CONSUME_OR_RET(TOKEN_LPAREN, BAD_EXPR);
    if(!parse_type(l, &expr->expr.ct_alignof))
        return BAD_EXPR;
    CONSUME_OR_RET(TOKEN_RPAREN, BAD_EXPR);
    expr->loc = extend_loc(expr->loc, peek_prev(l)->loc);
    expr->type = g_type_usize;
    return expr;
}

static ASTExpr* parse_ct_offsetof(Lexer* l)
{
    ASTExpr* expr = new_expr(EXPR_CT_ALIGNOF);
    expr->loc = peek(l)->loc;
    advance(l);
    CONSUME_OR_RET(TOKEN_LPAREN, BAD_EXPR);
    if(!parse_type(l, &expr->expr.ct_offsetof.struct_))
        return BAD_EXPR;
    CONSUME_OR_RET(TOKEN_RPAREN, BAD_EXPR);
    expr->loc = extend_loc(expr->loc, peek_prev(l)->loc);
    return expr;

}

static ASTExpr* parse_ct_sizeof(Lexer* l)
{
    ASTExpr* expr = new_expr(EXPR_CT_SIZEOF);
    expr->loc = peek(l)->loc;
    advance(l);
    CONSUME_OR_RET(TOKEN_LPAREN, BAD_EXPR);
    if(!parse_type(l, &expr->expr.ct_sizeof))
        return BAD_EXPR;
    CONSUME_OR_RET(TOKEN_RPAREN, BAD_EXPR);
    expr->loc = extend_loc(expr->loc, peek_prev(l)->loc);
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
    sic_diagnostic_atv(DIAG_ERROR, peek(l)->loc, message, va);
    advance(l);
    va_end(va);
}

static inline void eof_error(Lexer* l, TokenKind expected)
{
    const char* const tok_str = tok_kind_to_str(expected);
    sic_diagnostic_after(DIAG_ERROR, peek_prev(l)->loc, tok_str,
                         "Encountered end-of-file during parsing, expected \'%s\'.",
                         tok_str);
}

static inline bool expect(Lexer* l, TokenKind kind)
{
    // USE expect_ident
    DBG_ASSERT(kind != TOKEN_IDENT);
    if(tok_equal(l, kind))
        return true;

    const char* const tok_str = tok_kind_to_str(kind);
    sic_diagnostic_after(DIAG_ERROR, peek_prev(l)->loc, tok_str, 
                         "Expected \'%s\'.",
                         tok_str);
    return false;
}

static inline bool expect_ident(Lexer* l)
{
    if(tok_equal(l, TOKEN_IDENT))
        return true;

    sic_diagnostic_after(DIAG_ERROR, peek_prev(l)->loc, NULL, "Expected identifier.");
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
    if(expect(l, kind))
    {
        advance(l);
        return true;
    }

    return false;
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

static inline void recover_top_level(Lexer* l, uint32_t prev_col)
{
    advance(l);
    Token* t = peek(l);
    while(true)
    {
        switch(t->kind)
        {
        case TOKEN_EOF:
            return;
        case TOKEN_BITFIELD:
        case TOKEN_CONST:
        case TOKEN_CT_CONST:
        case TOKEN_VAR:
        case TOKEN_ENUM:
        case TOKEN_FN:
        case TOKEN_IMPORT:
        case TOKEN_MODULE:
        case TOKEN_STRUCT:
        case TOKEN_UNION:
        case TOKEN_TYPEDEF:
        case TOKEN_CT_ASSERT:
            if(t->line_col.col <= prev_col) return;
            FALLTHROUGH;
        default:
            advance(l);
            t = peek(l);
            break;
        }
    }
}

static inline ASTStmt* new_stmt(Lexer* l, StmtKind kind)
{
    ASTStmt* stmt = CALLOC_STRUCT(ASTStmt);
    stmt->kind = kind;
    stmt->loc = peek(l)->loc;
    return stmt;
}

static inline ASTExpr* new_expr(ExprKind kind)
{
    ASTExpr* expr = CALLOC_STRUCT(ASTExpr);
    expr->kind = kind;
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

static inline void declare_obj(Lexer* l, Object* global)
{
    HashMap* map = &l->module->symbol_ns;
    Object* other = hashmap_get(map, global->symbol);

    if(other != NULL)
        sic_error_redef(global, other);

    hashmap_put(map, global->symbol, global);
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
    [TOKEN_SUB]             = { parse_negation, parse_binary, PREC_ADD_SUB },
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
    [TOKEN_RANGE]           = { NULL, parse_range, PREC_RANGE },

    [TOKEN_CAST]            = { parse_cast, NULL, PREC_NONE },
    [TOKEN_FALSE]           = { parse_bool_literal, NULL, PREC_NONE },
    [TOKEN_NULLPTR]         = { parse_nullptr, NULL, PREC_NONE },
    [TOKEN_TRUE]            = { parse_bool_literal, NULL, PREC_NONE },

    [TOKEN_CT_ALIGNOF]      = { parse_ct_alignof, NULL, PREC_NONE },
	[TOKEN_CT_OFFSETOF]     = { parse_ct_offsetof, NULL, PREC_NONE },
	[TOKEN_CT_SIZEOF]       = { parse_ct_sizeof, NULL, PREC_NONE },
};
