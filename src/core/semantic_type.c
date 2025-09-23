#include "semantics.h"

static bool resolve_array(SemaContext* c, Type* arr_ty, SourceLoc err_loc);
static bool resolve_typeof(SemaContext* c, Type** type_ref, Type* typeof);
static bool resolve_user(SemaContext* c, Type** type_ref, ResolutionFlags flags,
                         SourceLoc err_loc, const char* err_str);

bool resolve_type(SemaContext* c, Type** type_ref, ResolutionFlags flags,
                  SourceLoc err_loc, const char* err_str)
{
    SIC_ASSERT(c != NULL);
    SIC_ASSERT(type_ref != NULL);
    SIC_ASSERT(*type_ref != NULL);

    Type* type = *type_ref;
    SIC_ASSERT(type->status != STATUS_RESOLVING);
    switch(type->kind)
    {
    case TYPE_VOID:
        if(BIT_IS_UNSET(flags, RES_ALLOW_VOID))
        {
            sic_error_at(err_loc, "%s 'void'.", err_str);
            // Return false here because this is g_void and we don't want
            // to cascade errors by setting the type kind to TYPE_INVALID.
            return false;
        }
        FALLTHROUGH;
    case TYPE_BOOL:
    case TYPE_BYTE:
    case TYPE_UBYTE:
    case TYPE_SHORT:
    case TYPE_USHORT:
    case TYPE_INT:
    case TYPE_UINT:
    case TYPE_LONG:
    case TYPE_ULONG:
    case TYPE_FLOAT:
    case TYPE_DOUBLE:
        SIC_ASSERT(type->status == STATUS_RESOLVED);
        return true;
    case TYPE_POINTER: {
        if(type->status == STATUS_RESOLVED)
            return !type_is_bad(type);
        type->status = STATUS_RESOLVED;
        bool prev = c->in_ptr;
        c->in_ptr = true;
        if(!resolve_type(c, &type->pointer_base, RES_ALLOW_VOID, err_loc, "Cannot have pointer to type")) break;
        c->in_ptr = prev;
        type->visibility = type->pointer_base->visibility;
        return true;
    }
    case TYPE_STATIC_ARRAY:
    case TYPE_RUNTIME_ARRAY:
        if(type->status == STATUS_RESOLVED)
            return !type_is_bad(type);
        if(!resolve_type(c, &type->array.elem_type, RES_NORMAL, err_loc, "")) break;
        type->status = type->array.elem_type->status;
        return true;
    case TYPE_ENUM:
    case TYPE_STRUCT:
    case TYPE_TYPEDEF:
    case TYPE_UNION:
        if(type->status == STATUS_RESOLVED)
            return !type_is_bad(type);
        if(!analyze_type_obj(c, type->user_def, type_ref, flags, err_loc, err_str)) break;
        return true;
    case TYPE_PRE_SEMA_ARRAY:
        if(type->status == STATUS_RESOLVED)
            return !type_is_bad(type);
        if(!resolve_array(c, type, err_loc)) break;
        return true;
    case TYPE_PRE_SEMA_USER:
        if(type->status == STATUS_RESOLVED)
            return !type_is_bad(type);
        if(!resolve_user(c, type_ref, flags, err_loc, err_str)) break;
        return true;
    case TYPE_AUTO:
        // TODO: Utilize less memory now that we take in the location and error string.
        sic_error_at(type->auto_loc, "Type 'auto' not allowed in this context.");
        break;
    case TYPE_TYPEOF:
        if(type->status == STATUS_RESOLVED)
            return !type_is_bad(type);
        type->status = STATUS_RESOLVED;
        if(!resolve_typeof(c, type_ref, type)) break;
        return true;
    default:
        SIC_UNREACHABLE();
    }
    (*type_ref)->kind = TYPE_INVALID;
    (*type_ref)->status = STATUS_RESOLVED;
    return false;
}


static bool resolve_array(SemaContext* c, Type* arr_ty, SourceLoc err_loc)
{
    TypeArray* arr = &arr_ty->array;
    if(!resolve_type(c, &arr->elem_type, RES_NORMAL, err_loc, "Arrays cannot have elements of type"))
        return false;

    // TODO: Check for negative size expr, that is BAD.
    //       For now negative sizes are implicitly casted to ulong, which
    //       means ubyte[-1] tries to allocate quintillions of bytes!! :)
    if(!analyze_expr(c, &arr->size_expr) || !implicit_cast(c, &arr->size_expr, g_type_ulong))
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

static bool resolve_typeof(SemaContext* c, Type** type_ref, Type* typeof)
{
    if(!analyze_expr(c, &typeof->type_of))
        return false;
    *type_ref = typeof->type_of->type;
    return true;
}

static bool resolve_user(SemaContext* c, Type** type_ref, ResolutionFlags flags,
                         SourceLoc err_loc, const char* err_str)
{
    Type* user_ty = *type_ref;
    Object* type_obj = find_obj(c, user_ty->unresolved.sym);
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
    return analyze_type_obj(c, type_obj, type_ref, flags, err_loc, err_str);
}

