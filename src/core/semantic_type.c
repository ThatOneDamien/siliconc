#include "semantics.h"

static bool resolve_array(Type* arr_ty, SourceLoc error_loc);
static bool resolve_func_ptr(Type* func_ty);
static bool resolve_multi_pointer(Type* pointer, SourceLoc error_loc);
static bool resolve_single_pointer(Type* pointer, SourceLoc error_loc);
static bool resolve_typeof(Type** type_ref, TypeResFlags flags, SourceLoc error_loc, const char* error_msg);
static bool resolve_user_def(Type** type_ref, TypeResFlags flags, SourceLoc error_loc, const char* error_msg);

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
    case TYPE_POINTER_MULTI:
        if(!resolve_multi_pointer(type, error_loc)) break;
        return true;
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
    case TYPE_ALIAS:
    case TYPE_ALIAS_DISTINCT: {
        DBG_ASSERT(type->status == STATUS_RESOLVING);
        if(!analyze_type_obj(type->user_def)) break;
        Type* alias = obj_as_typedef(type->user_def)->alias.type;
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
    case __TYPE_COUNT:
        SIC_UNREACHABLE();
    }
    *type_ref = g_type_invalid;
    return false;
}

static bool resolve_array(Type* type, SourceLoc error_loc)
{
    bool valid = resolve_type(&type->array.elem_type, TYPE_RES_NORMAL, error_loc, "Arrays cannot have elements of type %s.");

    ResolveStatus prev_status = type->status;
    type->status = type->array.elem_type->status;
    if(prev_status == STATUS_RESOLVING) return valid;

    type->visibility = type->array.elem_type->visibility;
    ASTExpr* size_expr = type->array.size_expr;
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

    ASTExpr* size_expr = pointer->pointer.size_expr;

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
    ASTExpr* inner = type->type_of;
    if(!analyze_expr(inner))
        return false;

    Type* inner_ty = inner->type;
    if(inner_ty->kind == TYPE_INIT_LIST)
    {
        sic_error_at(inner->loc, "Cannot get type of array/struct initializer list.");
        return false;
    }
    if(inner_ty->kind == TYPE_STRING_LITERAL)
    {
        DBG_ASSERT(inner->expr.constant.kind == CONSTANT_STRING);
        *type_ref = type_apply_qualifiers(type_pointer_to_multi(g_type_char, NULL), qualifiers);
        return true;
    }
    *type_ref = type_apply_qualifiers(inner_ty, qualifiers);
    return resolve_type(type_ref, flags, error_loc, error_msg);
}

static bool resolve_user_def(Type** type_ref, TypeResFlags flags, SourceLoc error_loc, const char* error_msg)
{
    Type* user_ty = *type_ref;
    Object* type_obj = find_obj(&user_ty->unresolved);
    if(type_obj == NULL) return false;
    TypeQualifiers qualifiers = user_ty->qualifiers;
    switch(type_obj->kind)
    {
    case OBJ_INVALID:
        return false;
    case OBJ_BITFIELD:
        SIC_TODO();
    case OBJ_ENUM: { // Enums always have to be resolved
        ObjEnum* enum_ = obj_as_enum(type_obj);
        if(!analyze_enum_underlying(enum_)) return false;
        user_ty = enum_->type_ref;
        user_ty->status = STATUS_RESOLVED;
        break;
    }
    case OBJ_STRUCT:
        user_ty = obj_as_struct(type_obj)->type_ref;
        if(!resolve_type(&user_ty, flags, error_loc, error_msg)) return false;
        break;
    case OBJ_TYPEDEF:
        user_ty = obj_as_typedef(type_obj)->type_ref;
        if(!resolve_type(&user_ty, flags, error_loc, error_msg)) return false;
        break;
    case OBJ_UNION:
        user_ty = obj_as_struct(type_obj)->type_ref;
        if(!resolve_type(&user_ty, flags, error_loc, error_msg)) return false;
        break;
    default:
        sic_error_at(user_ty->unresolved.data[user_ty->unresolved.size - 1].loc, "Symbol does not refer to a typename.");
        return false;
    }

    *type_ref = type_apply_qualifiers(user_ty, qualifiers);
    return true;
}

