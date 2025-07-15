#include "semantics.h"

static bool resolve_array(SemaContext* c, Type* arr_ty);
static bool resolve_user(SemaContext* c, Type* user_ty);

bool resolve_type(SemaContext* c, Type* type)
{
    if(type->status == STATUS_RESOLVED)
        return true;
    if(type->status == STATUS_RESOLVING)
        SIC_UNREACHABLE();
    switch(type->kind)
    {
    case TYPE_POINTER:
        return resolve_type(c, type->pointer_base);
    case TYPE_PRE_SEMA_ARRAY:
        return resolve_array(c, type);
    case TYPE_USER_DEF:
        return resolve_user(c, type);
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
    if(size_expr->kind == EXPR_INVALID || !type_is_integer(size_expr->type) ||
       !implicit_cast(c, size_expr, g_type_ulong))
        return false;

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

static bool resolve_user(SemaContext* c, Type* user_ty)
{
    Object* type_obj = find_obj(c, &user_ty->unresolved);
    if(type_obj == NULL)
    {
        sema_error(c, &user_ty->unresolved, "Unknown typename.");
        return false;
    }

    switch(type_obj->kind)
    {
    case OBJ_BITFIELD:
    case OBJ_ENUM:
    case OBJ_STRUCT:
    case OBJ_TYPEDEF:
    case OBJ_UNION:
        user_ty->user_def = type_obj;
        user_ty->status = STATUS_RESOLVED;
        return true;
    case OBJ_FUNC:
    case OBJ_VAR:
    case OBJ_ENUM_VALUE:
        // TODO: Fix this one too!
        sema_error(c, &user_ty->unresolved, "Symbol does not refer to a type.");
        return false;
    default:
        SIC_UNREACHABLE();
    }
}
