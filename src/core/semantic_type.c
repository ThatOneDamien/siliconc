#include "semantics.h"

static bool resolve_array(Type* arr_ty, SourceLoc err_loc);
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
    case TYPE_STRING_LITERAL:
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
    case TYPE_ENUM:
    case TYPE_STRUCT:
    case TYPE_TYPEDEF:
    case TYPE_UNION:
        if(type->status == STATUS_RESOLVED) return true;
        if(!analyze_type_obj(type->user_def, type_ref, flags, err_loc, err_str)) break;
        return true;
    case TYPE_PRE_SEMA_ARRAY:
        SIC_ASSERT(type->status != STATUS_RESOLVED);
        if(!resolve_array(type, err_loc)) break;
        return true;
    case TYPE_PRE_SEMA_USER:
        SIC_ASSERT(type->status != STATUS_RESOLVED);
        if(!resolve_user(type_ref, flags, err_loc, err_str)) break;
        return true;
    case TYPE_AUTO:
        // TODO: Utilize less memory now that we take in the location and error string.
        sic_error_at(type->auto_loc, "Type 'auto' not allowed in this context.");
        break;
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


static bool resolve_array(Type* arr_ty, SourceLoc err_loc)
{
    TypeArray* arr = &arr_ty->array;
    if(!resolve_type(&arr->elem_type, RES_NORMAL, err_loc, "Arrays cannot have elements of type"))
        return false;

    // TODO: Check for negative size expr, that is BAD.
    //       For now negative sizes are implicitly casted to ulong, which
    //       means ubyte[-1] tries to allocate quintillions of bytes!! :)
    if(!analyze_expr(&arr->size_expr) || !implicit_cast(&arr->size_expr, g_type_ulong))
        return false;

    arr_ty->visibility = arr->elem_type->visibility;
    arr_ty->status = arr->elem_type->status;

    if(arr->size_expr->kind == EXPR_CONSTANT)
    {
        SIC_ASSERT(arr->size_expr->expr.constant.kind == CONSTANT_INTEGER &&
                   arr->size_expr->type->kind == TYPE_ULONG);
        arr_ty->kind = TYPE_STATIC_ARRAY;
        arr->ss_size = arr->size_expr->expr.constant.val.i;
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

