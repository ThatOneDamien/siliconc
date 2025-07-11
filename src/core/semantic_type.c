#include "semantics.h"

static bool resolve_array(SemaContext* c, Type* arr_ty);

bool resolve_type(SemaContext* c, Type* type)
{
    switch(type->kind)
    {
    case TYPE_VOID:
    case TYPE_BOOL:
    case TYPE_U8:
    case TYPE_U16:
    case TYPE_U32:
    case TYPE_U64:
    case TYPE_S8:
    case TYPE_S16:
    case TYPE_S32:
    case TYPE_S64:
    case TYPE_F32:
    case TYPE_F64:
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
    //       For now negative sizes are implicitly casted to u64, which
    //       means u8[-1] tries to allocate quintillions of bytes!! :)
    if(!implicit_cast(c, size_expr, g_type_u64))
        return false;

    if(size_expr->kind == EXPR_CONSTANT)
    {
        SIC_ASSERT(size_expr->expr.constant.kind == CONSTANT_INTEGER &&
                   size_expr->type->kind == TYPE_U64);
        arr_ty->kind = TYPE_SS_ARRAY;
        arr_ty->ss_array.elem_type = elem_type;
        arr_ty->ss_array.elem_cnt = size_expr->expr.constant.val.i;
        return true;
    }

    arr_ty->kind = TYPE_DS_ARRAY;
    return true;
}
