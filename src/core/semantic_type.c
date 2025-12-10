#include "semantics.h"

static bool resolve_array(Type* arr_ty, ResolutionFlags flags,
                          SourceLoc err_loc, const char* err_str);
static bool resolve_func_ptr(Type* func_ty, SourceLoc err_loc);
static bool resolve_typeof(Type** type_ref, Type* typeof);
static bool resolve_user(Type** type_ref, ResolutionFlags flags,
                         SourceLoc err_loc, const char* err_str);

bool resolve_type(Type** type_ref, ResolutionFlags flags,
                  SourceLoc err_loc, const char* err_str)
{
    SIC_ASSERT(type_ref != NULL);
    SIC_ASSERT(*type_ref != NULL);

    Type* type = *type_ref;
    SIC_ASSERT(type->status != STATUS_RESOLVING);
    switch(type->kind)
    {
    case TYPE_INVALID:
        return false;
    case TYPE_VOID:
        if(BIT_IS_UNSET(flags, RES_ALLOW_VOID))
        {
            sic_error_at(err_loc, "%s 'void'.", err_str);
            break;
        }
        FALLTHROUGH;
    case INT_TYPES:
    case FLOAT_TYPES:
    case TYPE_ANON_ARRAY:
        SIC_ASSERT(type->status == STATUS_RESOLVED);
        return true;
    case TYPE_POINTER: {
        if(type->status == STATUS_RESOLVED) return true;
        type->status = STATUS_RESOLVED;
        bool prev = g_sema->in_ptr;
        g_sema->in_ptr = true;
        if(!resolve_type(&type->pointer_base, RES_ALLOW_VOID, err_loc, "Cannot have pointer to type")) break;
        g_sema->in_ptr = prev;
        type->visibility = type->pointer_base->visibility;
        return true;
    }
    case TYPE_FUNC_PTR:
        if(type->status == STATUS_RESOLVED) return true;
        if(!resolve_func_ptr(type, err_loc)) break;
        type->status = STATUS_RESOLVED;
        return true;
    case TYPE_STATIC_ARRAY:
    case TYPE_RUNTIME_ARRAY:
        if(type->status == STATUS_RESOLVED) return true;
        if(!resolve_type(&type->array.elem_type, RES_NORMAL, err_loc, "")) break;
        type->status = type->array.elem_type->status;
        return true;
    case TYPE_ALIAS:
    case TYPE_ALIAS_DISTINCT:
    case TYPE_ENUM:
    case TYPE_ENUM_DISTINCT:
    case TYPE_STRUCT:
    case TYPE_UNION:
        if(type->status == STATUS_RESOLVED) return true;
        if(!analyze_type_obj(type->user_def, type_ref, flags, err_loc, err_str)) break;
        return true;
    case TYPE_AUTO:
        if(BIT_IS_UNSET(flags, RES_ALLOW_AUTO))
        {
            sic_error_at(err_loc, "%s 'auto'.", err_str);
            break;
        }
        return true;
    case TYPE_PS_ARRAY:
        if(!resolve_array(type, flags, err_loc, err_str)) break;
        return true;
    case TYPE_PS_USER:
        SIC_ASSERT(type->status != STATUS_RESOLVED);
        if(!resolve_user(type_ref, flags, err_loc, err_str)) break;
        return true;
    case TYPE_TYPEOF:
        if(type->status == STATUS_RESOLVED) return true;
        type->status = STATUS_RESOLVED;
        if(!resolve_typeof(type_ref, type)) break;
        return true;
    case __TYPE_COUNT:
        SIC_UNREACHABLE();
    }
    *type_ref = g_type_invalid;
    return false;
}


static bool resolve_array(Type* arr_ty, ResolutionFlags flags, SourceLoc err_loc, const char* err_str)
{
    TypeArray* arr = &arr_ty->array;
    if(!resolve_type(&arr->elem_type, RES_NORMAL, err_loc, "Arrays cannot have elements of type"))
        return false;

    arr_ty->visibility = arr->elem_type->visibility;
    arr_ty->status = arr->elem_type->status;

    if(arr->size_expr == NULL)
    {
        if(BIT_IS_SET(flags, RES_ALLOW_AUTO_ARRAY))
            return true;

        sic_error_at(err_loc, "%s auto-sized array \'%s\'.", err_str, type_to_string(arr_ty));
        return false;
    }

    if(!analyze_expr(&arr->size_expr))
        return false;

    bool was_signed = type_is_signed(arr->size_expr->type->canonical);
    if(!implicit_cast(&arr->size_expr, g_type_ulong))
        return false;


    if(arr->size_expr->kind == EXPR_CONSTANT)
    {
        SIC_ASSERT(arr->size_expr->expr.constant.kind == CONSTANT_INTEGER);
        uint64_t length = arr->size_expr->expr.constant.val.i;
        if(was_signed && (int64_t)length < 0)
        {
            sic_error_at(err_loc, "Array declared with a negative length(%ld).", (int64_t)length);
            return false;
        }
        arr_ty->kind = TYPE_STATIC_ARRAY;
        arr->static_len = length;
        return true;
    }

    arr_ty->kind = TYPE_RUNTIME_ARRAY;
    return true;
}

static bool resolve_func_ptr(Type* func_ty, SourceLoc err_loc)
{
    FuncSignature* sig = func_ty->func_ptr;
    if(!resolve_type(&sig->ret_type, RES_ALLOW_VOID, err_loc, "Function pointer type cannot have return type"))
        return false;

    func_ty->visibility = sig->ret_type->visibility; 

    for(uint32_t i = 0; i < sig->params.size; ++i)
    {
        Object* param = sig->params.data[i];
        if(!resolve_type(&param->type, RES_NORMAL, param->loc, "Parameter cannot be of type"))
            return false;
        if(param->type->visibility < func_ty->visibility)
            func_ty->visibility = param->type->visibility;
    }

    return true;
}

static bool resolve_typeof(Type** type_ref, Type* typeof)
{
    g_sema->ident_mask = IDENT_FUNC;
    if(!analyze_expr(&typeof->type_of))
        return false;
    *type_ref = typeof->type_of->type;
    return true;
}

static bool resolve_user(Type** type_ref, ResolutionFlags flags,
                         SourceLoc err_loc, const char* err_str)
{
    Type* user_ty = *type_ref;
    Object* type_obj = find_obj(user_ty->unresolved.sym);
    if(type_obj == NULL)
    {
        sic_error_at(user_ty->unresolved.loc, "Unknown typename.");
        return false;
    }
    if(!obj_is_type(type_obj))
    {
        sic_error_at(user_ty->unresolved.loc, "Symbol does not refer to a typename.");
        return false;
    }
    user_ty->visibility = type_obj->visibility;
    return analyze_type_obj(type_obj, type_ref, flags, err_loc, err_str);
}

