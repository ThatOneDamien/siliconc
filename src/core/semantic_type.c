#include "semantics.h"

static bool resolve_array(SemaContext* c, Type* arr_ty);

bool resolve_type(SemaContext* c, Type* type)
{
    switch(type->kind)
    {
    case TYPE_VOID:
    case TYPE_BOOL:
    case TYPE_UBYTE:
    case TYPE_USHORT:
    case TYPE_UINT:
    case TYPE_ULONG:
    case TYPE_BYTE:
    case TYPE_SHORT:
    case TYPE_INT:
    case TYPE_LONG:
    case TYPE_FLOAT:
    case TYPE_DOUBLE:
    case TYPE_SS_ARRAY:
    case TYPE_DS_ARRAY:
        return true;
    case TYPE_POINTER:
        return resolve_type(c, type->pointer_base);
    case TYPE_PRE_SEMA_ARRAY:
        return resolve_array(c, type);
    default:
        SIC_UNREACHABLE();
    }
}

static bool resolve_array(SemaContext* c, Type* arr_ty)
{
    Type* elem_type = arr_ty->pre_sema_array.elem_type;
    ASTExpr* size_expr = arr_ty->pre_sema_array.size_expr;
    if(!resolve_type(c, elem_type))
        return false;

    analyze_expr(c, size_expr);
    // TODO: Check for negative size expr, that is BAD.
    //       For now negative sizes are implicitly casted to ulong, which
    //       means ubyte[-1] tries to allocate quintillions of bytes!! :)
    if(!implicit_cast(c, size_expr, g_type_ulong))
        return false;

    if(size_expr->kind == EXPR_CONSTANT)
    {
        SIC_ASSERT(size_expr->expr.constant.kind == CONSTANT_INTEGER &&
                   size_expr->type->kind == TYPE_ULONG);
        arr_ty->kind = TYPE_SS_ARRAY;
        arr_ty->ss_array.elem_type = elem_type;
        arr_ty->ss_array.elem_cnt = size_expr->expr.constant.val.i;
        return true;
    }

    arr_ty->kind = TYPE_DS_ARRAY;
    return true;
}
