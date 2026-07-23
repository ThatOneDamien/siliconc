#include "semantics.h"

// FIXME: Major bug when resolving types regarding circular definitions.
// Something like var i: #typeof(i) crashes the compiler instead of erroring.
// Right now I am not fixing it because it is somewhat niche but this is in need
// of a fix for sure.
static bool resolve_array(Type* arr_ty, SourceLoc error_loc);
static bool resolve_func_ptr(Type* func_ty);
static bool resolve_multi_pointer(Type* pointer, SourceLoc error_loc);
static bool resolve_single_pointer(Type* pointer, SourceLoc error_loc);
static bool resolve_typeof(Type** type_ref, TypeResFlags flags, SourceLoc error_loc, const char* error_msg);
static bool resolve_user_def(Type** type_ref, TypeResFlags flags, SourceLoc error_loc, const char* error_msg);

static bool analyze_enum(ObjEnum* enum_);
static bool analyze_struct(ObjStruct* struct_);
static bool analyze_typedef(ObjTypedef* typedef_);
static bool analyze_union(ObjStruct* union_);

// Resolve a type based on flags. If the type is marked as STATUS_RESOLVED, this means
// it has a known size/layout (i.e. not void, opaque struct/union, inferred array).
// If it is marked as STATUS_RESOLVING, that means we have resolved names (like UNRESOLVED_USER),
// but we aren't sure if it has a size. STATUS_RESOLVING should only appear if we resolve a type
// under a pointer type, because that is allowed to be a type without a size/layout.
bool resolve_type(Type** type_ref, TypeResFlags flags, SourceLoc error_loc, const char* error_msg)
{
    DBG_ASSERT(type_ref != NULL);
    DBG_ASSERT(*type_ref != NULL);

    Type* type = *type_ref;
    if(type->status == STATUS_RESOLVED) return type->kind != TYPE_INVALID;

    switch(type->kind)
    {
    case TYPE_VOID:
        if(FLAG_IS_NOT_SET(flags, TYPE_RES_ALLOW_VOID))
        {
            sic_error_at_ignore_fmt_warning(error_loc, error_msg, "void");
            break;
        }
        FALLTHROUGH;
    case NUMERIC_TYPES:
        return true;
    case TYPE_POINTER_SINGLE:
        if(!resolve_single_pointer(type, error_loc)) break;
        return true;
    case TYPE_POINTER_MULTI_STATIC:
    case TYPE_POINTER_MULTI_UNKNOWN:
        if(!resolve_multi_pointer(type, error_loc)) break;
        return true;
    case TYPE_FUNC_PTR:
        if(!resolve_func_ptr(type)) break;
        return true;
    case TYPE_STATIC_ARRAY:
        if(!resolve_array(type, error_loc)) break;
        return true;
    case TYPE_INFERRED_ARRAY: {
        TypeArray* arr = &type->array;
        if(type->status == STATUS_UNRESOLVED)
        {
            if(!resolve_type(&arr->elem_type, TYPE_RES_NORMAL, error_loc, "Arrays cannot have elements of type %s.")) break;

            type->visibility = arr->elem_type->visibility;
            type->status = STATUS_RESOLVING;
        }

        if(FLAG_IS_NOT_SET(flags, TYPE_RES_ALLOW_AUTO_ARRAY))
        {
            sic_error_at_ignore_fmt_warning(error_loc, error_msg, str_format("[*]%s", type_to_string(arr->elem_type)));
            break;

        }
        return true;
    }
    case TYPE_SLICE: {
        bool prev = g_sema.type_res_allow_unresolved;
        g_sema.type_res_allow_unresolved = true;
        bool valid = resolve_type(&type->slice.base, TYPE_RES_NORMAL, error_loc, "Slice of type %s is not allowed.");
        g_sema.type_res_allow_unresolved = prev;
        if(!valid) break;
        type->status = STATUS_RESOLVED;
        type->visibility = type->pointer.base->visibility;
        return true;
    }
    case TYPE_OPTIONAL:
        if(!resolve_type(&type->optional.base, TYPE_RES_NORMAL, error_loc, "You cannot have optionals of type %s.")) break;
        type->status = type->optional.base->status;
        type->visibility = type->optional.base->visibility;
        return true;
    case TYPE_ALIAS:
    case TYPE_ALIAS_DISTINCT: {
        DBG_ASSERT(type->status == STATUS_RESOLVING);
        if(!analyze_type_obj(type->user_def)) break;
        Type* alias = type->typedef_->alias.type;
        if(!resolve_type(&alias, flags, error_loc, error_msg)) break;
        type->status = alias->status;
        return true;
    }
    case TYPE_STRUCT:
    case TYPE_UNION: {
        DBG_ASSERT(type->status == STATUS_RESOLVING);
        if(g_sema.type_res_allow_unresolved) return true;
        if(!analyze_type_obj(type->user_def)) break;
        type->status = STATUS_RESOLVED;
        return true;
    }
    case TYPE_UNRESOLVED_USER:
        DBG_ASSERT(type->status == STATUS_UNRESOLVED);
        if(!resolve_user_def(type_ref, flags, error_loc, error_msg)) break;
        return true;
    case TYPE_TYPEOF:
        DBG_ASSERT(type->status == STATUS_UNRESOLVED);
        if(!resolve_typeof(type_ref, flags, error_loc, error_msg)) break;
        return true;
    case TYPE_INVALID:
    case TYPE_ENUM:
    case TYPE_ENUM_DISTINCT:
    case TYPE_INIT_LIST:
    case TYPE_STRING_LITERAL:
    case TYPE_NULL:
    case __TYPE_COUNT:
        SIC_UNREACHABLE();
    }
    *type_ref = g_type_invalid;
    return false;
}

bool type_has_default_value(const Type* type, SourceLoc error_loc, bool silent)
{
    DBG_ASSERT(type != NULL);
    DBG_ASSERT(type->status != STATUS_UNRESOLVED);
    const Type* t = type;
RETRY:
    switch(t->kind)
    {
    case TYPE_BOOL:
    case CHAR_TYPES:
    case INT_TYPES:
    case FLOAT_TYPES:
    case TYPE_OPTIONAL:
    case TYPE_UNION:
        return true;
    case POINTER_TYPES:
    case TYPE_SLICE: {
        const char* type_str = type_to_string(type);
        if(!silent)
            sic_error_at(error_loc, "Type %s has no default value. If you wish to have null use ?%s.", type_str, type_str);
        return false;
    }
    case TYPE_STATIC_ARRAY:
        t = t->array.elem_type;
        goto RETRY;
    case TYPE_ALIAS:
    case TYPE_ALIAS_DISTINCT:
        // TODO: Let distinct types have default values
        t = t->typedef_->alias.type;
        goto RETRY;
    case TYPE_ENUM:
    case TYPE_ENUM_DISTINCT:
        // TODO: Let distinct types have default values
        t = t->enum_->underlying.type;
        goto RETRY;
    case TYPE_STRUCT: {
        const ObjVarDA members = t->struct_->members;
        for(uint32_t i = 0; i < members.size; ++i)
        {
            ObjVar* member = members.data[i];
            if(member->uninitialized || (member->initial_val == NULL && !type_has_default_value(member->type_loc.type, LOC_NULL, true)))
            {
                if(!silent)
                    sic_error_at(error_loc, "Member %s of struct-type %s has no default value.", member->header.sym, type_to_string(type));
                return false;
            }
        }
        return true;
    }
    case TYPE_INVALID:
    case TYPE_VOID:
    case SEMA_ONLY_TYPES:
        break;
    }
    SIC_UNREACHABLE();
}

static bool resolve_array(Type* type, SourceLoc error_loc)
{
    if(!resolve_type(&type->array.elem_type, TYPE_RES_NORMAL, error_loc, "Arrays cannot have elements of type %s.")) return false;

    ResolveStatus prev_status = type->status;
    type->status = type->array.elem_type->status;
    if(prev_status == STATUS_RESOLVING) return true;

    type->visibility = type->array.elem_type->visibility;
    Expr* size_expr = type->array.size_expr;
    if(!implicit_cast(size_expr, g_type_usize))
        return false;

    if(size_expr->kind != EXPR_CONSTANT)
    {
        sic_error_at(error_loc, "Length of array type must be constant.");
        return false;
    }

    DBG_ASSERT(size_expr->expr.constant.kind == CONSTANT_INTEGER);
    ArrayLength length = size_expr->expr.constant.i.lo;
    if(length == 0)
    {
        sic_error_at(error_loc, "Length of array type must be greater than 0.");
        return false;
    }
    type->array.static_len = length;
    return true;
}

static bool resolve_func_ptr(Type* type)
{
    FuncSignature* sig = type->func_ptr;
    if(!resolve_type(&sig->ret_type.type, TYPE_RES_ALLOW_VOID, sig->ret_type.loc, "Function pointer type cannot have return type"))
        return false;

    type->visibility = sig->ret_type.type->visibility; 

    bool valid = true;
    bool prev = g_sema.type_res_allow_unresolved;
    g_sema.type_res_allow_unresolved = true;
    for(uint32_t i = 0; i < sig->params.size; ++i)
    {
        ObjVar* param = sig->params.data[i];
        if(!resolve_type(&param->type_loc.type, TYPE_RES_NORMAL, param->type_loc.loc, "Parameter cannot be of type"))
            valid = false;
        if(param->type_loc.type->visibility < type->visibility)
            type->visibility = param->type_loc.type->visibility;
    }
    g_sema.type_res_allow_unresolved = prev;
    type->status = STATUS_RESOLVED;
    return valid;
}

static bool resolve_multi_pointer(Type* pointer, SourceLoc error_loc)
{
    bool valid;
    bool prev = g_sema.type_res_allow_unresolved;
    g_sema.type_res_allow_unresolved = true;
    valid = resolve_type(&pointer->pointer.base, TYPE_RES_NORMAL, error_loc, 
                         "Pointer to multiple elements of type %s is not allowed.");
    g_sema.type_res_allow_unresolved = prev;

    Expr* size_expr = pointer->pointer.size_expr;

    if(size_expr != NULL)
    {
        if(implicit_cast(size_expr, g_type_usize))
        {
            if(size_expr->kind == EXPR_CONSTANT)
            {
                DBG_ASSERT(size_expr->expr.constant.kind == CONSTANT_INTEGER);
                ArrayLength length = size_expr->expr.constant.i.lo;
                pointer->pointer.static_len = length;
                if(length == 0)
                {
                    sic_error_at(error_loc, "Length of multi-pointer must be greater than 0.");
                    valid = false;
                }
            }
            else
            {
                sic_error_at(error_loc, "Length of multi-pointer must be constant if supplied. Use *[*]T for pointer to any number of elements.");
                valid = false;
            }
        }
        else 
            valid = false;
    }

    pointer->status = STATUS_RESOLVED;
    pointer->visibility = pointer->pointer.base->visibility;
    return valid;

}

static bool resolve_single_pointer(Type* type, SourceLoc error_loc)
{
    bool valid;
    bool prev = g_sema.type_res_allow_unresolved;
    g_sema.type_res_allow_unresolved = true;
    valid = resolve_type(&type->pointer.base, TYPE_RES_ALLOW_VOID, error_loc, 
            "Pointer to %s is not allowed.");
    g_sema.type_res_allow_unresolved = prev;

    type->status = STATUS_RESOLVED;
    type->visibility = type->pointer.base->visibility;
    return valid;

}

static bool resolve_typeof(Type** type_ref, TypeResFlags flags, SourceLoc error_loc, const char* error_msg)
{
    Type* type = *type_ref;
    TypeQualifiers qualifiers = type->qualifiers;
    Expr* inner = type->type_of;
    if(!analyze_expr(inner))
        return false;

    Type* inner_ty = inner->type;
    if(type_is_int_literal(inner_ty))
    {
        sic_error_at(inner->loc, "Integer literals have no default type.");
        return false;
    }
    else if(inner_ty->kind == TYPE_INIT_LIST)
    {
        sic_error_at(inner->loc, "Cannot get type of array/struct initializer list.");
        return false;
    }
    else if(inner_ty->kind == TYPE_STRING_LITERAL)
    {
        DBG_ASSERT(inner->expr.constant.kind == CONSTANT_STRING);
        *type_ref = type_apply_qualifiers(type_pointer_to_multi_unknown(g_type_char), qualifiers);
        return true;
    }
    *type_ref = type_apply_qualifiers(inner_ty, qualifiers);
    return resolve_type(type_ref, flags, error_loc, error_msg);
}

static bool resolve_user_def(Type** type_ref, TypeResFlags flags, SourceLoc error_loc, const char* error_msg)
{
    Type* user_ty = *type_ref;
    Object* type_header = find_obj(&user_ty->unresolved);
    if(type_header == NULL) return false;
    TypeQualifiers qualifiers = user_ty->qualifiers;
    ObjType* type_obj = obj_as_type(type_header);
    user_ty = type_obj->type_ref;
    switch(type_header->kind)
    {
    case OBJ_INVALID:
        return false;
    case OBJ_BITFIELD:
        SIC_TODO();
    case OBJ_ENUM: { // Enums always have to be resolved
        ObjEnum* enum_ = obj_as_enum(type_header);
        if(!analyze_enum_underlying(enum_)) return false;
        break;
    }
    case OBJ_STRUCT:
        if(!resolve_type(&user_ty, flags, error_loc, error_msg)) return false;
        break;
    case OBJ_TYPEDEF:
        if(!resolve_type(&user_ty, flags, error_loc, error_msg)) return false;
        break;
    case OBJ_UNION:
        if(!resolve_type(&user_ty, flags, error_loc, error_msg)) return false;
        break;
    default:
        sic_error_at(user_ty->unresolved.data[user_ty->unresolved.size - 1].loc, "Symbol does not refer to a typename.");
        return false;
    }

    *type_ref = type_apply_qualifiers(user_ty, qualifiers);
    return true;
}

bool analyze_type_obj(ObjType* type_obj)
{
    DBG_ASSERT(type_obj != NULL);
    Object* type_header = &type_obj->header;
    switch(type_header->status)
    {
    case STATUS_UNRESOLVED:
        break;
    case STATUS_RESOLVING:
        set_circular_def(type_header);
        return false;
    case STATUS_RESOLVED:
        return !obj_is_bad(type_header);
    }

    bool prev = g_sema.in_global_init;
    g_sema.in_global_init = true;

    ObjModule* prev_mod = g_sema.module;
    g_sema.module = type_header->module;

    bool success;

    switch(type_header->kind)
    {
    case OBJ_BITFIELD:
        SIC_TODO();
    case OBJ_ENUM:
        success = analyze_enum(obj_as_enum(type_header));
        break;
    case OBJ_STRUCT:
        success = analyze_struct(obj_as_struct(type_header));
        break;
    case OBJ_TYPEDEF:
        success = analyze_typedef(obj_as_typedef(type_header));
        break;
    case OBJ_UNION:
        success = analyze_union(obj_as_struct(type_header));
        break;
    case OBJ_INVALID:
        success = false;
        break;
    case OBJ_ENUM_VALUE:
    case OBJ_FUNC:
    case OBJ_IMPORT:
    case OBJ_MODULE:
    case OBJ_VAR:
        SIC_UNREACHABLE();
    }
    g_sema.in_global_init = prev;
    g_sema.module = prev_mod;
    return success;
}

bool analyze_enum_underlying(ObjEnum* enum_)
{
    Type* const type = enum_->header.type_ref;
    if(type->status == STATUS_RESOLVED) return !type_is_bad(type);
    if(!resolve_type(&enum_->underlying.type, TYPE_RES_NORMAL, enum_->underlying.loc, "An enum's underlying type cannot be of type"))
        goto ERR;

    enum_->underlying.type = enum_->underlying.type->canonical;
    if(!type_is_integer(enum_->underlying.type))
    {
        sic_error_at(enum_->underlying.loc, "Underlying type for enums must be an integer type.");
        goto ERR;
    }

    type->status = STATUS_RESOLVED;
    type->canonical = type->kind == TYPE_ENUM ? enum_->underlying.type : type;
    return true;
ERR:
    type->status = STATUS_RESOLVED;
    type->kind = TYPE_INVALID;
    return false;
}

static bool analyze_enum(ObjEnum* enum_)
{
    Object* enum_obj = &enum_->header.header;
    enum_obj->status = STATUS_RESOLVING;
    analyze_attributes(enum_obj);

    bool valid = analyze_enum_underlying(enum_);

    DBG_ASSERT(enum_->values.size > 0);
    TypeKind underlying_kind = enum_->underlying.type->kind;
    Int128 min_value;
    Int128 max_value;
    if(!analyze_enum_value(enum_, 0)) valid = false;
    min_value = enum_->values.data[0]->const_value;
    max_value = min_value;

    for(uint32_t i = 1; i < enum_->values.size; ++i)
    {
        ObjEnumValue* value = enum_->values.data[i];
        for(uint32_t j = 0; j < i; ++j)
        {
            ObjEnumValue* other = enum_->values.data[j];
            if(value->header.sym == other->header.sym)
            {
                sic_error_redef(&value->header, &other->header, "enum value");
                valid = false;
                goto NEXT;
            }
        }
        if(!analyze_enum_value(enum_, i))
        {
            valid = false;
            goto NEXT;
        }
        if(i128_cmp(value->const_value, min_value, underlying_kind) < 0)
        {
            min_value = value->const_value;
            enum_->min_idx = i;
        }
        else if(i128_cmp(value->const_value, max_value, underlying_kind) > 0)
        {
            max_value = value->const_value;
            enum_->max_idx = i;
        }

    NEXT:;
    }

    enum_obj->status = STATUS_RESOLVED;

    if(!valid)
    {
        invalidate_obj(enum_obj);
        return false;
    }
    return true;
}

bool analyze_enum_value(ObjEnum* parent, uint32_t index)
{
    DBG_ASSERT(index < parent->values.size);
    ObjEnumValue* value = parent->values.data[index];
    switch(value->header.status)
    {
    case STATUS_UNRESOLVED:
        break;
    case STATUS_RESOLVING:
        set_circular_def(&value->header);
        return false;
    case STATUS_RESOLVED:
        return !obj_is_bad(&value->header);
    }

    bool enum_valid = analyze_enum_underlying(parent);
    Type* underlying = parent->underlying.type;
    Expr* value_expr = value->raw_value;

    value->header.status = STATUS_RESOLVING;


    if(value_expr != NULL) 
    {
        ObjEnum* prev = g_sema.cur_enum;
        g_sema.cur_enum = parent;
        bool valid = analyze_rvalue(value_expr);
        g_sema.cur_enum = prev;
        if(!valid) goto ERR;
        if(value_expr->kind != EXPR_CONSTANT || value_expr->expr.constant.kind != CONSTANT_INTEGER)
        {
            sic_error_at(value->header.loc, "Enum value must be assigned a compile-time constant integer value.");
            goto ERR;
        }

        if(!enum_valid) goto ERR;
        TypeKind from_kind = type_reduce(value_expr->type)->kind;
        value->const_value = value_expr->expr.constant.i;
        if(!i128_fits(value->const_value, from_kind, underlying->kind))
        {
            sic_error_at(value->header.loc, "Enum value(%s) is not representable by underlying type.",
                         i128_to_string(value->const_value, type_kind_is_signed(from_kind)));
            goto ERR;
        }
    }
    else
    {
        if(index == 0)
        {
            value->const_value = UINT128_MIN;
        }
        else if(!analyze_enum_value(parent, index - 1)) goto ERR;
        else
        {
            Int128 last = parent->values.data[index - 1]->const_value;
            if(i128_cmp(last, int_max(underlying->kind), underlying->kind) >= 0)
            {
                sic_error_at(value->header.loc, "Enum value exceeds the maximum of underlying type.");
                goto ERR;
            }
            value->const_value = i128_add64(last, 1);
        }
    }

    value->header.status = STATUS_RESOLVED;
    return true;
ERR:
    invalidate_obj(&value->header);
    return false;
}

static bool analyze_struct(ObjStruct* struct_)
{
    Object* struct_obj = &struct_->header.header;
    struct_obj->status = STATUS_RESOLVING;
    analyze_attributes(struct_obj);
    bool packed = has_builtin_attr(struct_obj, ATTR_PACKED);
    if(packed)
        struct_->align = 1;
    for(uint32_t i = 0; i < struct_->members.size; ++i)
    {
        ObjVar* member = struct_->members.data[i];
        if(!resolve_type(&member->type_loc.type, TYPE_RES_ALLOW_AUTO_ARRAY, member->type_loc.loc, "Struct member cannot have type %s."))
        {
            check_circular_def(struct_obj, struct_obj->loc);
            return false;
        }
        for(uint32_t j = 0; j < i; ++j)
        {
            Object* other = &struct_->members.data[j]->header;
            if(member->header.sym == other->sym)
                sic_error_redef(&member->header, other, "member");
        }
        Type* type = member->type_loc.type;

        if(type->visibility < struct_obj->visibility)
        {
            // TODO: Make this error print the actual visibility of both.
            sic_error_at(member->header.loc, "Member has type with less visibility than parent.");
            goto ERR;
        }

        ByteSize size;
        ByteSize align;
        if(type->kind == TYPE_INFERRED_ARRAY)
        {
            if(i != struct_->members.size - 1)
            {
                sic_error_at(member->header.loc, "Inferred array is only allowed as the last member of a struct.");
                goto ERR;
            }
            size = 0;
            align = type_alignment(type->array.elem_type);
            SIC_TODO_MSG("Inferred array members not working properly.");
        }
        else
        {
            size = type_size(type);
            align = type_alignment(type);
        }

        if(packed)
        {
            struct_->size += size;
        }
        else
        {
            DBG_ASSERT(is_pow_of_2(align));
            struct_->size = ALIGN_UP(struct_->size, align) + size;
            struct_->align = MAX(struct_->align, align);
        }
    }

    struct_->size = ALIGN_UP(struct_->size, struct_->align);
    struct_obj->status = STATUS_RESOLVED;
    struct_->header.type_ref->status = STATUS_RESOLVED;
    return true;

ERR:
    invalidate_obj(struct_obj);
    return false;
}

static bool analyze_typedef(ObjTypedef* typedef_)
{
    Object* typedef_obj = &typedef_->header.header;
    typedef_obj->status = STATUS_RESOLVING;
    analyze_attributes(typedef_obj);
    bool prev = g_sema.type_res_allow_unresolved;
    g_sema.type_res_allow_unresolved = true;
    bool success = resolve_type(&typedef_->alias.type, TYPE_RES_ALLOW_VOID, typedef_->alias.loc, "You cannot define a type equal to %s.");
    g_sema.type_res_allow_unresolved = prev;
    if(!success)
    {
        check_circular_def(typedef_obj, typedef_obj->loc);
        return false;
    }
    typedef_obj->status = STATUS_RESOLVED;
    Type* type_ref = typedef_->header.type_ref;
    type_ref->status = STATUS_RESOLVED;
    type_ref->canonical = type_ref->kind == TYPE_ALIAS ?
                              typedef_->alias.type->canonical :
                              type_ref;
    typedef_->alias.type = type_reduce(typedef_->alias.type);
    return true;
}

static bool analyze_union(ObjStruct* union_)
{
    Object* union_obj = &union_->header.header;
    union_obj->status = STATUS_RESOLVING;
    analyze_attributes(union_obj);
    uint32_t largest_size = 0;

    for(uint32_t i = 0; i < union_->members.size; ++i)
    {
        ObjVar* member = union_->members.data[i];
        if(!resolve_type(&member->type_loc.type, TYPE_RES_NORMAL, member->type_loc.loc, "Union member cannot have type %s."))
        {
            check_circular_def(union_obj, union_obj->loc);
            return false;
        }
        for(uint32_t j = 0; j < i; ++j)
        {
            Object* other = &union_->members.data[j]->header;
            if(member->header.sym == other->sym)
                sic_error_redef(&member->header, other, "member");
        }
        if(member->type_loc.type->visibility < union_obj->visibility)
        {
            // TODO: Make this error print the actual visibility of both.
            sic_error_at(member->header.loc, "Member has type with less visibility than parent.");
            invalidate_obj(union_obj);
            return false;
        }
        uint32_t next_size = type_size(member->type_loc.type);
        if(largest_size < next_size)
        {
            largest_size = next_size;
            union_->largest_type = member->type_loc.type;
        }
    }
    union_obj->status = STATUS_RESOLVED;
    union_->header.type_ref->status = STATUS_RESOLVED;

    return true;
}

Object* find_type_member(ObjType* parent, Symbol sym)
{
    DBG_ASSERT(parent != NULL);
    DBG_ASSERT(sym != NULL);
    TypeKind kind = parent->type_ref->kind;
    if(kind == TYPE_INVALID || kind == TYPE_ALIAS || kind == TYPE_ENUM)
        return NULL;

    switch(kind)
    {
    case TYPE_INVALID:
    case TYPE_ALIAS:
        return NULL;
    case TYPE_ALIAS_DISTINCT:
        break;
    case TYPE_ENUM:
    case TYPE_ENUM_DISTINCT: {
        ObjEnum* enum_ = (ObjEnum*)parent;
        const ObjEnumValueDA values = enum_->values;
        for(uint32_t i = 0; i < values.size; ++i)
        {
            if(values.data[i]->header.sym == sym)
                return &values.data[i]->header;
        }
        if(kind == TYPE_ENUM) return NULL;
        break;
    }
    case TYPE_STRUCT:
    case TYPE_UNION: {
        ObjStruct* struct_ = (ObjStruct*)parent;
        for(uint32_t i = 0; i < struct_->members.size; ++i)
        {
            if(struct_->members.data[i]->header.sym == sym)
                return &struct_->members.data[i]->header;
        }
        break;
    }
    default:
        SIC_UNREACHABLE();
    }

    for(uint32_t i = 0; i < parent->methods.size; ++i)
    {
        if(parent->methods.data[i]->header.sym == sym)
            return &parent->methods.data[i]->header;
    }

    return NULL;
}
