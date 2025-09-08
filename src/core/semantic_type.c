#include "semantics.h"

static bool resolve_array(SemaContext* c, Type* arr_ty);
static bool resolve_typeof(SemaContext* c, Type** type_ref, Type* typeof);
static bool resolve_user(SemaContext* c, Type* user_ty, bool is_pointer);

bool resolve_type_or_ptr(SemaContext* c, Type** type_ref, bool is_pointer)
{
    SIC_ASSERT(c != NULL);
    SIC_ASSERT(type_ref != NULL);
    SIC_ASSERT(*type_ref != NULL);
    Type* type = *type_ref;
    if(type->status == STATUS_RESOLVED)
        return type->kind != TYPE_INVALID;
    if(type->status == STATUS_RESOLVING)
        SIC_UNREACHABLE();
    switch(type->kind)
    {
    case TYPE_AUTO:
        sic_error_at(type->auto_loc, "Type 'auto' not allowed in this context.");
        return false;
    case TYPE_POINTER:
        type->status = STATUS_RESOLVED;
        if(!resolve_type_or_ptr(c, &type->pointer_base, true))
            return false;
        type->visibility = type->pointer_base->visibility;
        return true;
    case TYPE_PRE_SEMA_ARRAY:
        return resolve_array(c, type);
    case TYPE_STRUCT:
        return resolve_user(c, type, is_pointer);
    case TYPE_TYPEOF:
        return resolve_typeof(c, type_ref, type);
    default:
        SIC_UNREACHABLE();
    }
}


static bool resolve_array(SemaContext* c, Type* arr_ty)
{
    TypeArray* arr = &arr_ty->array;
    if(!resolve_type(c, &arr->elem_type))
        return false;

    // TODO: Check for negative size expr, that is BAD.
    //       For now negative sizes are implicitly casted to ulong, which
    //       means ubyte[-1] tries to allocate quintillions of bytes!! :)
    if(!analyze_expr(c, arr->size_expr) || !type_is_integer(arr->size_expr->type) ||
       !implicit_cast(c, &arr->size_expr, g_type_ulong))
        return false;

    arr_ty->status = STATUS_RESOLVED;
    arr_ty->visibility = arr->elem_type->visibility;

    if(arr->size_expr->kind == EXPR_CONSTANT)
    {
        SIC_ASSERT(arr->size_expr->expr.constant.kind == CONSTANT_INTEGER &&
                   arr->size_expr->type->kind == TYPE_ULONG);
        arr_ty->kind = TYPE_SS_ARRAY;
        arr_ty->array.ss_size = arr->size_expr->expr.constant.val.i;
        return true;
    }

    arr_ty->kind = TYPE_DS_ARRAY;
    return true;
}

static bool resolve_typeof(SemaContext* c, Type** type_ref, Type* typeof)
{
    ASTExpr* expr = typeof->type_of;
    if(!analyze_expr(c, expr))
        return false;
    *type_ref = expr->type;
    return true;
}

static bool resolve_user(SemaContext* c, Type* user_ty, bool is_pointer)
{
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
    analyze_type_obj(c, type_obj, is_pointer);
    user_ty->user_def = type_obj;
    user_ty->status = STATUS_RESOLVED;
    user_ty->visibility = type_obj->visibility;
    if(type_obj->kind == OBJ_INVALID)
        return false;
    switch(type_obj->kind)
    {
    case OBJ_BITFIELD:
        SIC_TODO();
    case OBJ_ENUM:
        user_ty->kind = TYPE_ENUM;
        return true;
    case OBJ_STRUCT:
        user_ty->kind = TYPE_STRUCT;
        return true;
    case OBJ_TYPE_ALIAS:
        memcpy(user_ty, type_obj->type_alias, sizeof(Type));
        return true;
    case OBJ_TYPE_DISTINCT:
        SIC_TODO();
    case OBJ_UNION:
        user_ty->kind = TYPE_UNION;
        return true;
    case OBJ_ALIAS_EXPR:
    case OBJ_FUNC:
    case OBJ_VAR:
    case OBJ_ENUM_VALUE:
    case OBJ_INVALID:
        break;
    }
    SIC_UNREACHABLE();
}

