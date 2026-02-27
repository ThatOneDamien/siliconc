#include "semantics.h"

static bool resolve_array(Type* arr_ty, TypeResFlags flags, SourceLoc error_loc, const char* error_msg);
static bool resolve_func_ptr(Type* func_ty);
static bool resolve_typeof(Type** type_ref, TypeResFlags flags, SourceLoc error_loc, const char* error_msg);
static bool resolve_user_def(Type** type_ref, Object* type_obj, TypeResFlags flags,
                             SourceLoc error_loc, const char* error_msg);

bool resolve_type(Type** type_ref, TypeResFlags flags, SourceLoc error_loc, const char* error_msg)
{
    DBG_ASSERT(type_ref != NULL);
    DBG_ASSERT(*type_ref != NULL);

    Type* type = *type_ref;
    if(type->status == STATUS_RESOLVED) return type->kind != TYPE_INVALID;
    DBG_ASSERT(type->status != STATUS_RESOLVING);
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
        DBG_ASSERT(type->status == STATUS_RESOLVED);
        return true;
    case TYPE_POINTER:
    case TYPE_SLICE: {
        if(type->status == STATUS_RESOLVED) return true;
        bool prev = g_sema->type_res_allow_unresolved;
        g_sema->type_res_allow_unresolved = true;
        if(!resolve_type(&type->pointer_base, TYPE_RES_ALLOW_VOID | TYPE_RES_ALLOW_AUTO_ARRAY, 
                         error_loc, "Pointer to %s is not allowed.")) 
            break;
        g_sema->type_res_allow_unresolved = prev;
        type->status = STATUS_RESOLVED;
        type->visibility = type->pointer_base->visibility;
        return true;
    }
    case TYPE_FUNC_PTR:
        if(type->status == STATUS_RESOLVED) return true;
        if(!resolve_func_ptr(type)) break;
        type->status = STATUS_RESOLVED;
        return true;
    case TYPE_STATIC_ARRAY:
    case TYPE_RUNTIME_ARRAY:
        if(type->status == STATUS_RESOLVED) return true;
        if(!resolve_type(&type->array.elem_type, TYPE_RES_NORMAL, error_loc, "Arrays cannot have elements of type %s."))
            break;
        type->status = type->array.elem_type->status;
        return true;
    case TYPE_INFERRED_ARRAY:
        SIC_TODO();
    case TYPE_ALIAS:
    case TYPE_ALIAS_DISTINCT:
        if(!resolve_user_def(type_ref, &type->typedef_->header, flags, error_loc, error_msg)) break;
        return true;
    case TYPE_ENUM:
    case TYPE_ENUM_DISTINCT:
        if(!resolve_user_def(type_ref, &type->enum_->header, flags, error_loc, error_msg)) break;
        return true;
    case TYPE_STRUCT:
    case TYPE_UNION:
        if(!resolve_user_def(type_ref, &type->struct_->header, flags, error_loc, error_msg)) break;
        return true;
    case TYPE_UNRESOLVED_ARRAY:
        if(!resolve_array(type, flags, error_loc, error_msg)) break;
        return true;
    case TYPE_UNRESOLVED_USER: {
        Object* type_obj = find_obj(&type->unresolved);
        if(type_obj == NULL) break;
        if(!resolve_user_def(type_ref, type_obj, flags, error_loc, error_msg)) break;
        return true;
    }
    case TYPE_TYPEOF:
        if(!resolve_typeof(type_ref, flags, error_loc, error_msg)) break;
        return true;
    case TYPE_INVALID:
    case TYPE_INIT_LIST:
    case TYPE_STRING_LITERAL:
    case __TYPE_COUNT:
        SIC_UNREACHABLE();
    }
    *type_ref = g_type_invalid;
    return false;
}


static bool resolve_array(Type* arr_ty, TypeResFlags flags, SourceLoc error_loc, const char* error_msg)
{
    TypeArray* arr = &arr_ty->array;
    if(!resolve_type(&arr->elem_type, TYPE_RES_NORMAL, error_loc, "Arrays cannot have elements of type %s."))
        return false;

    arr_ty->visibility = arr->elem_type->visibility;
    arr_ty->status = arr->elem_type->status;

    if(arr_ty->kind == TYPE_INFERRED_ARRAY)
    {
        if(FLAG_IS_SET(flags, TYPE_RES_ALLOW_AUTO_ARRAY))
            return true;

        sic_error_at_ignore_fmt_warning(error_loc, error_msg, str_format("[*]%s", type_to_string(arr->elem_type)));
        return false;
    }

    if(!analyze_expr(arr->size_expr))
        return false;

    if(!implicit_cast(arr->size_expr, g_type_usize))
        return false;


    if(arr->size_expr->kind == EXPR_CONSTANT)
    {
        DBG_ASSERT(arr->size_expr->expr.constant.kind == CONSTANT_INTEGER);
        uint64_t length = arr->size_expr->expr.constant.i.lo;
        arr_ty->kind = TYPE_STATIC_ARRAY;
        arr->static_len = length;
        return true;
    }

    arr_ty->kind = TYPE_RUNTIME_ARRAY;
    return true;
}

static bool resolve_func_ptr(Type* func_ty)
{
    FuncSignature* sig = func_ty->func_ptr;
    if(!resolve_type(&sig->ret_type.type, TYPE_RES_ALLOW_VOID, sig->ret_type.loc, "Function pointer type cannot have return type"))
        return false;

    func_ty->visibility = sig->ret_type.type->visibility; 

    for(uint32_t i = 0; i < sig->params.size; ++i)
    {
        ObjVar* param = sig->params.data[i];
        if(!resolve_type(&param->type_loc.type, TYPE_RES_NORMAL, param->type_loc.loc, "Parameter cannot be of type"))
            return false;
        if(param->type_loc.type->visibility < func_ty->visibility)
            func_ty->visibility = param->type_loc.type->visibility;
    }

    return true;
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
        *type_ref = type_apply_qualifiers(type_pointer_to(g_type_char), qualifiers);
        return true;
    }
    *type_ref = type_apply_qualifiers(inner_ty, qualifiers);
    return resolve_type(type_ref, flags, error_loc, error_msg);
}

static bool resolve_user_def(Type** type_ref, Object* type_obj, TypeResFlags flags, 
                             SourceLoc error_loc, const char* error_msg)
{
    DBG_ASSERT(type_obj != NULL);
    Type* user_ty = *type_ref;
    TypeQualifiers qualifiers = user_ty->qualifiers;
    switch(type_obj->kind)
    {
    case OBJ_INVALID:
        return false;
    case OBJ_BITFIELD:
        SIC_TODO();
    case OBJ_ENUM:
        if(!analyze_type_obj(type_obj)) return false;
        user_ty = obj_as_enum(type_obj)->type_ref;
        break;
    case OBJ_STRUCT:
        if(g_sema->type_res_allow_unresolved)
        {
            user_ty->kind = TYPE_STRUCT;
            user_ty->struct_ = obj_as_struct(type_obj);
            return true;
        }
        if(!analyze_type_obj(type_obj)) return false;
        user_ty = obj_as_struct(type_obj)->type_ref;
        break;
    case OBJ_TYPEDEF:
        if(!analyze_type_obj(type_obj)) return false;
        *type_ref = type_apply_qualifiers(obj_as_typedef(type_obj)->type_ref, qualifiers);
        return resolve_type(type_ref, flags, error_loc, error_msg);
    case OBJ_UNION:
        if(g_sema->type_res_allow_unresolved)
        {
            user_ty->kind = TYPE_UNION;
            user_ty->struct_ = obj_as_struct(type_obj);
            return true;
        }
        if(!analyze_type_obj(type_obj)) return false;
        user_ty = obj_as_struct(type_obj)->type_ref;
        break;
    default:
        sic_error_at(user_ty->unresolved.data[user_ty->unresolved.size - 1].loc, "Symbol does not refer to a typename.");
        return false;
    }
    *type_ref = type_apply_qualifiers(user_ty, qualifiers);
    DBG_ASSERT((*type_ref)->status == STATUS_RESOLVED);
    return true;
}

