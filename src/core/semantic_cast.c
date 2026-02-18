#include "semantics.h"

#define CAST_ERROR(...) do { if(!params->silent) sic_error_at(params->cast_loc, __VA_ARGS__); return false; } while(0)

typedef struct CastParams CastParams;
typedef struct CastRule   CastRule;

struct CastParams
{
    ASTExpr*     inner;
    SourceLoc    cast_loc;
    Type*        from;
    Type*        to;
    Type*        fromc; // Equivalent to from->canonical
    Type*        toc;   // Equivalent to to->canonical
    bool         explicit;
    bool         silent;
};

struct CastRule
{
    bool (*able)(const CastParams* const params);
    void (*conversion)(ASTExpr* cast, ASTExpr* inner, Type* from, Type* to);
};

static CastGroup s_type_to_group[__TYPE_COUNT];
static CastRule s_rule_table[__CAST_GROUP_COUNT][__CAST_GROUP_COUNT];

static inline CastRule get_cast_rule(TypeKind from_kind, TypeKind to_kind);

bool analyze_cast(ASTExpr* cast)
{
    ASTExpr* inner = cast->expr.cast.inner;
    if(!resolve_type(&cast->type, RES_ALLOW_VOID, cast->loc, "Cannot cast to type") || 
       !analyze_expr(inner))
        return false;
    CastParams params;
    params.inner     = inner;
    params.cast_loc  = cast->loc;
    params.from      = inner->type;
    params.to        = cast->type;
    params.fromc     = params.from->canonical;
    params.toc       = params.to->canonical;
    params.explicit  = true;
    params.silent    = false;

    if(type_equal(params.from, params.to))
    {
        // Essentially delete the cast expression if it does nothing.
        *cast = *inner;
        return true;
    }


    CastRule rule = get_cast_rule(params.fromc->kind, params.toc->kind);
    if(rule.able == NULL)
    {
        sic_error_at(params.cast_loc, "Casting from %s to %s is not allowed.",
                     type_to_string(params.from), type_to_string(params.to));
        return false;
    }

    if(!rule.able(&params))
    {
        cast->kind = EXPR_INVALID;
        return false;
    }

    rule.conversion(cast, inner, params.fromc, params.toc);
    return true;
}

bool implicit_cast(ASTExpr** expr_to_cast, Type* desired)
{
    DBG_ASSERT(desired->status == STATUS_RESOLVED);
    ASTExpr* prev = *expr_to_cast;
    if(!analyze_expr(prev) || type_is_bad(desired))
        return false;
    CastParams params;
    params.inner     = prev;
    params.cast_loc  = prev->loc;
    params.from      = prev->type;
    params.to        = desired;
    params.fromc     = params.from->canonical;
    params.toc       = params.to->canonical;
    params.explicit  = false;
    params.silent    = false;
    if(type_equal(params.from, params.to))
        return true;

    CastRule rule = get_cast_rule(params.fromc->kind, params.toc->kind);
    if(rule.able == NULL)
    {
        sic_error_at(params.cast_loc, "Casting from %s to %s is not allowed.",
                     type_to_string(params.from), type_to_string(params.to));
        return false;
    }


    if(!rule.able(&params))
    {
        *expr_to_cast = g_bad_expr;
        return false;
    }

    ASTExpr* new_cast = CALLOC_STRUCT(ASTExpr);
    new_cast->kind = EXPR_CAST;
    new_cast->loc = prev->loc;
    new_cast->expr.cast.inner = prev;
    new_cast->evaluated = true;
    new_cast->type = desired;

    rule.conversion(new_cast, prev, params.fromc, params.toc);

    *expr_to_cast = new_cast;
    return true;
}

static bool can_cast_params(CastParams* params)
{
    if(type_equal(params->from, params->to))
        return true;

    CastRule rule = get_cast_rule(params->fromc->kind, params->toc->kind);
    if(rule.able == NULL)
    {
        CAST_ERROR("Casting from %s to %s is not allowed.",
                   type_to_string(params->from), type_to_string(params->to));
    }

    return rule.able(params);
}

bool can_cast(ASTExpr* expr, Type* to)
{
    DBG_ASSERT(to->status == STATUS_RESOLVED && expr->evaluated);

    CastParams params;
    params.inner     = expr;
    params.cast_loc  = expr->loc;
    params.from      = expr->type;
    params.to        = to;
    params.fromc     = params.from->canonical;
    params.toc       = params.to->canonical;
    params.explicit  = false;
    params.silent    = false;
    return can_cast_params(&params);
}


void perform_cast(ASTExpr* cast)
{
    ASTExpr* inner = cast->expr.cast.inner;
    Type* inner_ty = inner->type->canonical;
    Type* cast_ty = cast->type->canonical;
    CastRule rule = get_cast_rule(inner_ty->kind, cast_ty->kind);
    rule.conversion(cast, inner, inner_ty, cast_ty);
}

bool implicit_cast_vararg(ASTExpr** arg)
{
    ASTExpr* expr = *arg;
    if(!analyze_expr(expr)) return false;
    switch(expr->type->kind)
    {
    case TYPE_BOOL:
    case TYPE_CHAR:
    case TYPE_BYTE:
    case TYPE_UBYTE:
    case TYPE_SHORT:
    case TYPE_USHORT:
        implicit_cast_ensured(arg, g_type_int);
        return true;
    case TYPE_FLOAT:
        implicit_cast_ensured(arg, g_type_double);
        return true;
    case TYPE_INIT_LIST:
        sic_error_at(expr->loc, "Cannot pass initializer list as a variadic argument.");
        return false;
    case TYPE_STRING_LITERAL:
        implicit_cast_ensured(arg, type_pointer_to(g_type_char));
        return true;
    default:
        return true;
    }
}


static bool rule_not_defined(UNUSED const CastParams* const params) { SIC_UNREACHABLE(); }
static bool rule_always(UNUSED const CastParams* const params) { return true; }
static bool rule_explicit_only(const CastParams* const params)
{
    if(params->explicit)
        return true;
    CAST_ERROR("%s cannot be implicitly converted to %s.",
               type_to_string(params->from), type_to_string(params->to));
}

static bool rule_size_change(const CastParams* const params)
{
    if(params->explicit)
        return true;

    if(params->toc->kind == TYPE_CHAR && params->fromc->kind != TYPE_CHAR)
    {
        CAST_ERROR("Casting from %s to char requires explicit cast."
                   "Use byte or ubyte for single byte integer.",
                   type_to_string(params->from));
    }

    if(type_size(params->from) <= type_size(params->to))
        return true;

    // TODO: Add bounds checking for warning/error if constant goes out of bounds.
    if(params->inner->kind == EXPR_CONSTANT)
        return true;

    CAST_ERROR("Narrowing integer type %s to %s requires explicit cast.",
               type_to_string(params->from), type_to_string(params->to));
}

static bool rule_ptr_to_ptr(const CastParams* const params)
{
    if(params->explicit)
        return true;

    Type* from_ptr = params->fromc->pointer_base;
    Type* to_ptr   = params->toc->pointer_base;

    // Going from *const T to *T is illegal.
    if((from_ptr->qualifiers & TYPE_QUAL_CONST) && 
       (to_ptr->qualifiers & TYPE_QUAL_CONST) == 0)
    {
        CAST_ERROR("Casting from %s to %s disregards 'const' qualifier.", 
                   type_to_string(params->from), type_to_string(params->to));
    }

    from_ptr = from_ptr->canonical;
    to_ptr = to_ptr->canonical;
    if((params->fromc->kind == TYPE_POINTER && from_ptr->kind == TYPE_VOID) || 
       (params->toc->kind == TYPE_POINTER && to_ptr->kind == TYPE_VOID))
       return true;


    bool from_is_arr = type_is_array(from_ptr);
    if(type_is_array(to_ptr))
    {
        if(from_is_arr)
            goto ERR;
        from_is_arr = true;
        Type* temp = from_ptr;
        from_ptr = to_ptr;
        to_ptr = temp;
    }

    if(from_is_arr && !type_equal(from_ptr->array.elem_type, to_ptr))
        goto ERR;

    return true;
ERR:
    CAST_ERROR("Unable to implicitly cast between pointer types %s and %s.",
               type_to_string(params->from), type_to_string(params->to));
}

static bool rule_str_to_ptr(const CastParams* const params)
{
    TypeKind kind = params->toc->pointer_base->canonical->kind;

    if(params->fromc->kind != TYPE_STRING_LITERAL)
    {
        CAST_ERROR("Casting from %s to %s is not allowed.",
                   type_to_string(params->from), type_to_string(params->to));
    }

    if(kind != TYPE_CHAR && kind != TYPE_UBYTE && kind != TYPE_BYTE)
    {
        CAST_ERROR("String literal can only be casted to char*, ubyte*, or byte*.");
    }
    return true;
}

static bool rule_init_list_to_arr(const CastParams* const params)
{
    if(params->toc->kind == TYPE_RUNTIME_ARRAY)
    {
        // NOTE: This can be improved later on if I add some runtime checks. Don't plan on doing that
        //       atm, but if I do, revisit this.
        CAST_ERROR("Cannot cast to runtime array because its length is not known at compile-time.");
    }
    uint64_t arr_size = params->toc->array.static_len; 
    Type* elem_type = params->toc->array.elem_type->canonical;
    if(params->fromc->kind == TYPE_STRING_LITERAL)
    {
        DBG_ASSERT(params->inner->kind == EXPR_CONSTANT && params->inner->expr.constant.kind == CONSTANT_STRING);
        const ConstString str = params->inner->expr.constant.str;
        if(elem_type->kind != str.kind)
        {
            printf("%d %d\n", elem_type->kind, str.kind);
            CAST_ERROR("String literal cannot be converted to type %s.", type_to_string(params->to));
        }
        if(arr_size < str.len)
        {
            CAST_ERROR("Length of array(%lu) is smaller than length of string literal(%u).",
                       arr_size, str.len);
        }
        return true;
    }

    if(params->inner->kind != EXPR_ARRAY_INIT_LIST)
    {
        CAST_ERROR("Casting from %s to %s is not allowed.",
                   type_to_string(params->from), type_to_string(params->to));
    }

    ArrInitList* list = &params->inner->expr.array_init;
    for(uint32_t i = 0; i < list->size; ++i)
    {
        if(list->data[i].const_index >= arr_size)
        {
            if(!params->silent)
            {
                sic_error_at(list->data[i].init_value->loc, "Index of array constant(%lu) exceeds bounds of array(%lu).",
                             list->data[i].const_index, arr_size);
            }
            return false;
        }

        if(!can_cast(list->data[i].init_value, params->toc->array.elem_type))
            return false;
    }

    return true;
}

static bool rule_init_list_to_struct(const CastParams* const params)
{
    if(params->inner->kind != EXPR_STRUCT_INIT_LIST)
    {
        CAST_ERROR("Casting from %s to %s is not allowed.",
                   type_to_string(params->from), type_to_string(params->to));
    }

    // ObjStruct* struct_ = params->to->canonical->struct_;
    StructInitList* list = &params->inner->expr.struct_init;
    for(uint32_t i = 0; i < list->size; ++i)
    {
        StructInitEntry* entry = list->data + i;
        for(uint32_t j = 0; j < i; ++j)
        {
            if(list->data[j].unresolved_member.sym == entry->unresolved_member.sym)
            {
                sic_error_at(list->data[j].unresolved_member.loc, 
                             "Duplicate assignment of member \'%s\'.", 
                             entry->unresolved_member.sym);
                sic_diagnostic_at(DIAG_NOTE, entry->unresolved_member.loc, "Previous assignment here.");
                return false;
            }
        }
        // if(entry-)
    }

    return true;
}

static bool rule_distinct(const CastParams* const params)
{
    if(!params->explicit)
    {
        CAST_ERROR("Casting from %s to %s requires an explicit cast.",
                   type_to_string(params->from), type_to_string(params->to));
    }

    CastParams new_params;
    new_params.inner     = params->inner;
    new_params.from      = params->from;
    new_params.to        = params->to;
    new_params.fromc     = type_reduce(new_params.from);
    new_params.toc       = type_reduce(new_params.to);
    new_params.explicit  = true;
    new_params.silent    = params->silent;
    return can_cast_params(&new_params);
}

// Casting functions

static void cast_any_to_void(UNUSED ASTExpr* cast, UNUSED ASTExpr* inner,
                             UNUSED Type* from, UNUSED Type* to)
{
    SIC_TODO_MSG("Cast any to void");
}

static void cast_int_to_int(ASTExpr* cast, ASTExpr* inner, Type* from, Type* to)
{
    if(inner->kind == EXPR_CONSTANT)
    {
        convert_to_constant(cast, CONSTANT_INTEGER);
        uint32_t from_bit_width = from->canonical->builtin.bit_size;
        uint32_t to_bit_width = to->canonical->builtin.bit_size;
        Int128 val = inner->expr.constant.i;
        if(to_bit_width == 128)
        {
            cast->expr.constant.i = val;
            return;
        }

        uint32_t shift = 128 - to_bit_width; 
        bool to_signed = type_is_signed(to);
        if(from_bit_width < to_bit_width) // Extending
        {
            if(to_signed || type_is_unsigned(from))
            {
                cast->expr.constant.i = val;
                return;
            }
            cast->expr.constant.i = i128_lshr64(i128_shl64(val, shift), shift);
            return;
        }

        if(to_signed)
            cast->expr.constant.i = i128_ashr64(i128_shl64(val, shift), shift);
        else
            cast->expr.constant.i = i128_lshr64(i128_shl64(val, shift), shift);
        return;
    }

    if(type_size(from) == type_size(to))
    {
        cast->expr.cast.kind = CAST_REINTERPRET;
        return;
    }

    cast->const_eval = inner->const_eval;
    cast->expr.cast.kind = type_is_signed(from) ? 
                            CAST_SINT_EXT_TRUNC : 
                            CAST_UINT_EXT_TRUNC;
}

static void cast_bool_to_int(ASTExpr* cast, ASTExpr* inner, UNUSED Type* from, UNUSED Type* to)
{
    if(inner->kind == EXPR_CONSTANT)
    {
        convert_to_const_int(cast, i128_from_u64(inner->expr.constant.b));
        return;
    }

    cast->const_eval = inner->const_eval;
    cast->expr.cast.kind = CAST_UINT_EXT_TRUNC;
}

static void cast_int_to_bool(ASTExpr* cast, ASTExpr* inner, 
                             UNUSED Type* from, UNUSED Type* to)
{
    if(inner->kind == EXPR_CONSTANT)
    {
        convert_to_const_bool(cast, inner->expr.constant.i.hi != 0 || inner->expr.constant.i.lo != 0);
        return;
    }

    cast->const_eval = inner->const_eval;
    cast->expr.cast.kind = CAST_INT_TO_BOOL;
}

static void cast_int_to_float(ASTExpr* cast, ASTExpr* inner, Type* from, UNUSED Type* to)
{
    if(inner->kind == EXPR_CONSTANT)
    {
        convert_to_const_float(cast, i128_to_float(inner->expr.constant.i, from->kind));
        return;
    }

    cast->const_eval = inner->const_eval;
    cast->expr.cast.kind = type_is_signed(from) ? 
                            CAST_SINT_TO_FLOAT : 
                            CAST_UINT_TO_FLOAT;
}

static void cast_int_to_ptr(ASTExpr* cast, ASTExpr* inner, 
                            UNUSED Type* from, UNUSED Type* to)
{
    if(inner->kind == EXPR_CONSTANT)
    {
        convert_to_const_pointer(cast, inner->expr.constant.i.lo);
        return;
    }

    cast->const_eval = inner->const_eval;
    cast->expr.cast.kind = CAST_INT_TO_PTR;
}

static void cast_float_to_float(ASTExpr* cast, ASTExpr* inner,
                                UNUSED Type* from, UNUSED Type* to)
{
    if(inner->kind == EXPR_CONSTANT)
    {
        convert_to_const_float(cast, inner->expr.constant.f);
        return;
    }

    cast->const_eval = inner->const_eval;
    cast->expr.cast.kind = CAST_FLOAT_EXT_TRUNC;
}

static void cast_float_to_bool(ASTExpr* cast, ASTExpr* inner,
                               UNUSED Type* from, UNUSED Type* to)
{
    if(inner->kind == EXPR_CONSTANT)
    {
        convert_to_const_bool(cast, inner->expr.constant.f != 0.0);
        return;
    }

    cast->const_eval = inner->const_eval;
    cast->expr.cast.kind = CAST_FLOAT_TO_BOOL;
}

static void cast_float_to_int(ASTExpr* cast, ASTExpr* inner, UNUSED Type* from, Type* to)
{
    if(inner->kind == EXPR_CONSTANT)
    {
        convert_to_const_int(cast, i128_from_double(inner->expr.constant.f, to->kind));
        return;
    }

    cast->const_eval = inner->const_eval;
    cast->expr.cast.kind = type_is_signed(to) ? 
                            CAST_FLOAT_TO_SINT : 
                            CAST_FLOAT_TO_UINT;
}

static void cast_ptr_to_bool(ASTExpr* cast, ASTExpr* inner,
                             UNUSED Type* from, UNUSED Type* to)
{
    if(inner->kind == EXPR_CONSTANT)
    {
        convert_to_const_pointer(cast, inner->expr.constant.i.hi != 0 || inner->expr.constant.i.lo != 0);
        return;
    }

    cast->const_eval = inner->const_eval;
    cast->expr.cast.kind = CAST_PTR_TO_BOOL;
}

static void cast_ptr_to_int(ASTExpr* cast, ASTExpr* inner,
                            UNUSED Type* from, UNUSED Type* to)
{
    if(inner->kind == EXPR_CONSTANT)
    {
        convert_to_const_int(cast, inner->expr.constant.i);
        return;
    }

    cast->const_eval = inner->const_eval;
    cast->expr.cast.kind = CAST_PTR_TO_INT;
}

static void cast_ptr_to_ptr(ASTExpr* cast, ASTExpr* inner,
                            UNUSED Type* from, UNUSED Type* to)
{
    if(inner->kind == EXPR_CONSTANT)
    {
        convert_to_const_pointer(cast, inner->expr.constant.i.lo);
        return;
    }

    cast->const_eval = inner->const_eval;
    cast->expr.cast.kind = CAST_REINTERPRET;
}

static void cast_init_list(ASTExpr* cast, ASTExpr* inner,
                           UNUSED Type* from, UNUSED Type* to)
{
    // This is different than the parameter 'to'. This is the unaltered
    // type we are casting to, without any reduction.
    Type* to_type = cast->type;
    *cast = *inner;
    cast->type = to_type;
}

static void cast_distinct(ASTExpr* cast, ASTExpr* inner, Type* from, Type* to)
{
    Type* from_reduced = type_reduce(from);
    Type* to_reduced = type_reduce(to);
    CastRule rule = get_cast_rule(from_reduced->kind, to_reduced->kind);
    rule.conversion(cast, inner, from_reduced, to_reduced);
}

#define NOALLW { NULL                    , NULL }
#define NOTDEF { rule_not_defined        , NULL }
#define TOVOID { rule_explicit_only      , cast_any_to_void }
#define INTINT { rule_size_change        , cast_int_to_int }
#define BOOINT { rule_explicit_only      , cast_bool_to_int }
#define INTBOO { rule_explicit_only      , cast_int_to_bool }
#define INTFLT { rule_always             , cast_int_to_float }
#define INTPTR { rule_explicit_only      , cast_int_to_ptr }
#define FLTFLT { rule_size_change        , cast_float_to_float }
#define FLTBOO { rule_explicit_only      , cast_float_to_bool }
#define FLTINT { rule_explicit_only      , cast_float_to_int }
#define PTRBOO { rule_explicit_only      , cast_ptr_to_bool }
#define PTRINT { rule_explicit_only      , cast_ptr_to_int }
#define PTRPTR { rule_ptr_to_ptr         , cast_ptr_to_ptr }
#define STRPTR { rule_str_to_ptr         , cast_init_list }
#define ILSARR { rule_init_list_to_arr   , cast_init_list }
#define ILSSTU { rule_init_list_to_struct, cast_init_list }
#define DISTIN { rule_distinct           , cast_distinct }

static CastRule s_rule_table[__CAST_GROUP_COUNT][__CAST_GROUP_COUNT] = {
    // FROM              TO:   VOID    BOOL    CHAR    INT     FLOAT   PTR     ARRAY   STRUCT  INITLS  DIST
    [CAST_GROUP_VOID]      = { NOTDEF, NOALLW, NOTDEF, NOALLW, NOALLW, NOALLW, NOALLW, NOALLW, NOTDEF, NOALLW },
    [CAST_GROUP_BOOL]      = { TOVOID, NOTDEF, NOTDEF, BOOINT, NOALLW, NOALLW, NOALLW, NOALLW, NOTDEF, DISTIN },
    [CAST_GROUP_CHAR]      = { TOVOID, NOTDEF, NOTDEF, NOTDEF, NOTDEF, NOTDEF, NOTDEF, NOTDEF, NOTDEF, DISTIN },
    [CAST_GROUP_INT]       = { TOVOID, INTBOO, NOTDEF, INTINT, INTFLT, INTPTR, NOALLW, NOALLW, NOTDEF, DISTIN },
    [CAST_GROUP_FLOAT]     = { TOVOID, FLTBOO, NOTDEF, FLTINT, FLTFLT, NOALLW, NOALLW, NOALLW, NOTDEF, DISTIN },
    [CAST_GROUP_PTR]       = { TOVOID, PTRBOO, NOTDEF, PTRINT, NOALLW, PTRPTR, NOALLW, NOALLW, NOTDEF, DISTIN },
    [CAST_GROUP_ARRAY]     = { TOVOID, NOALLW, NOTDEF, NOALLW, NOALLW, NOALLW, NOALLW, NOALLW, NOTDEF, DISTIN },
    [CAST_GROUP_STRUCT]    = { TOVOID, NOALLW, NOTDEF, NOALLW, NOALLW, NOALLW, NOALLW, NOALLW, NOTDEF, DISTIN },
    [CAST_GROUP_INIT_LIST] = { TOVOID, NOALLW, NOTDEF, NOALLW, NOALLW, STRPTR, ILSARR, ILSSTU, NOTDEF, DISTIN },
    [CAST_GROUP_DISTINCT]  = { TOVOID, DISTIN, DISTIN, DISTIN, DISTIN, DISTIN, DISTIN, DISTIN, DISTIN, DISTIN },
};

// We add +1 so that by default all the unspecified types get 0 which is the INVALID group.
static CastGroup s_type_to_group[__TYPE_COUNT] = {
    [TYPE_VOID]           = CAST_GROUP_VOID + 1,
    [TYPE_BOOL]           = CAST_GROUP_BOOL + 1,
    [TYPE_CHAR]           = CAST_GROUP_CHAR + 1,
    [TYPE_CHAR16]         = CAST_GROUP_CHAR + 1,
    [TYPE_CHAR32]         = CAST_GROUP_CHAR + 1,
    [TYPE_BYTE]           = CAST_GROUP_INT + 1,
    [TYPE_UBYTE]          = CAST_GROUP_INT + 1,
    [TYPE_SHORT]          = CAST_GROUP_INT + 1,
    [TYPE_USHORT]         = CAST_GROUP_INT + 1,
    [TYPE_INT]            = CAST_GROUP_INT + 1,
    [TYPE_UINT]           = CAST_GROUP_INT + 1,
    [TYPE_LONG]           = CAST_GROUP_INT + 1,
    [TYPE_ULONG]          = CAST_GROUP_INT + 1,
    [TYPE_INT128]         = CAST_GROUP_INT + 1,
    [TYPE_UINT128]        = CAST_GROUP_INT + 1,
    [TYPE_FLOAT]          = CAST_GROUP_FLOAT + 1,
    [TYPE_DOUBLE]         = CAST_GROUP_FLOAT + 1,
    [TYPE_POINTER]        = CAST_GROUP_PTR + 1,
    [TYPE_FUNC_PTR]       = CAST_GROUP_PTR + 1,
    [TYPE_STATIC_ARRAY]   = CAST_GROUP_ARRAY + 1,
    [TYPE_RUNTIME_ARRAY]  = CAST_GROUP_ARRAY + 1,
    [TYPE_ALIAS_DISTINCT] = CAST_GROUP_DISTINCT + 1,
    [TYPE_ENUM_DISTINCT]  = CAST_GROUP_DISTINCT + 1,
    [TYPE_STRUCT]         = CAST_GROUP_STRUCT + 1,
    [TYPE_UNION]          = CAST_GROUP_STRUCT + 1,
    [TYPE_INIT_LIST]      = CAST_GROUP_INIT_LIST + 1,
    [TYPE_STRING_LITERAL] = CAST_GROUP_INIT_LIST + 1,
};

static inline CastRule get_cast_rule(TypeKind from_kind, TypeKind to_kind)
{
    CastGroup from = s_type_to_group[from_kind] - 1;
    CastGroup to = s_type_to_group[to_kind] - 1;
    DBG_ASSERT(from != CAST_GROUP_INVALID && to != CAST_GROUP_INVALID);
    return s_rule_table[from][to];
}
