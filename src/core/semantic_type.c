#include "semantics.h"

static bool resolve_array(SemaContext* c, Type* arr_ty);
static bool resolve_user(SemaContext* c, Type* user_ty, bool is_pointer);

bool resolve_type_or_ptr(SemaContext* c, Type* type, bool is_pointer)
{
    SIC_ASSERT(c != NULL);
    SIC_ASSERT(type != NULL);
    if(type->status == STATUS_RESOLVED)
        return type->kind != TYPE_INVALID;
    if(type->status == STATUS_RESOLVING)
        SIC_UNREACHABLE();
    switch(type->kind)
    {
    case TYPE_POINTER:
        type->status = STATUS_RESOLVED;
        if(!resolve_type_or_ptr(c, type->pointer_base, true))
            return false;
        type->visibility = type->pointer_base->visibility;
        return true;
    case TYPE_PRE_SEMA_ARRAY:
        return resolve_array(c, type);
    case TYPE_STRUCT:
        return resolve_user(c, type, is_pointer);
    case TYPE_ENUM:
    case TYPE_TYPEDEF:
    case TYPE_UNION:
    default:
        SIC_UNREACHABLE();
    }
}


static bool resolve_array(SemaContext* c, Type* arr_ty)
{
    Type* elem_type = arr_ty->array.elem_type;
    ASTExpr* size_expr = arr_ty->array.size_expr;
    if(!resolve_type(c, elem_type))
        return false;

    analyze_expr(c, size_expr);
    // TODO: Check for negative size expr, that is BAD.
    //       For now negative sizes are implicitly casted to ulong, which
    //       means ubyte[-1] tries to allocate quintillions of bytes!! :)
    if(expr_is_bad(size_expr) || !type_is_integer(size_expr->type) ||
       !implicit_cast(c, &arr_ty->array.size_expr, g_type_ulong))
        return false;

    arr_ty->status = STATUS_RESOLVED;
    arr_ty->visibility = elem_type->visibility;

    if(size_expr->kind == EXPR_CONSTANT)
    {
        SIC_ASSERT(size_expr->expr.constant.kind == CONSTANT_INTEGER &&
                   size_expr->type->kind == TYPE_ULONG);
        arr_ty->kind = TYPE_SS_ARRAY;
        arr_ty->array.ss_size = size_expr->expr.constant.val.i;
        return true;
    }

    arr_ty->kind = TYPE_DS_ARRAY;
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

