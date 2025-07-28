#include "semantics.h"

static bool resolve_array(SemaContext* c, Type* arr_ty);
static bool resolve_user(SemaContext* c, Type* user_ty, bool is_pointer);

bool resolve_type(SemaContext* c, Type* type, bool is_pointer)
{
    SIC_ASSERT(c != NULL);
    SIC_ASSERT(type != NULL);
    if(type->status == STATUS_RESOLVED)
        return true;
    if(type->status == STATUS_RESOLVING)
        SIC_UNREACHABLE();
    switch(type->kind)
    {
    case TYPE_POINTER:
        type->status = STATUS_RESOLVED;
        return resolve_type(c, type->pointer_base, true);
    case TYPE_PRE_SEMA_ARRAY:
        return resolve_array(c, type);
    case TYPE_ENUM:
    case TYPE_TYPEDEF:
        SIC_TODO();
    case TYPE_STRUCT:
    case TYPE_UNION:
        return resolve_user(c, type, is_pointer);
    default:
        SIC_UNREACHABLE();
    }
}

bool resolve_struct_type(SemaContext* c, Object* obj, bool is_pointer)
{
    SIC_ASSERT(c != NULL);
    SIC_ASSERT(obj != NULL);

    ObjStruct* struct_ = &obj->struct_;
    if(struct_->status == STATUS_RESOLVED)
        return true;
    if(struct_->status == STATUS_RESOLVING)
    {
        if(is_pointer)
            return true;
        // TODO: This error message can be improved greatly
        sema_error(c, &obj->loc, "Recursive structure definition.");
        return false;
    }

    switch(obj->kind)
    {
    case OBJ_BITFIELD:
    case OBJ_ENUM:
        SIC_TODO();
    case OBJ_STRUCT: {
        struct_->status = STATUS_RESOLVING;
        bool success = true;
        for(size_t i = 0; i < struct_->members.size; ++i)
        {
            Type* next_ty = struct_->members.data[i]->var.type;
            if(!resolve_type(c, next_ty, false))
            {
                success = false;
                continue;
            }
            uint32_t align = type_alignment(next_ty);
            SIC_ASSERT(is_pow_of_2(align));
            struct_->size = ALIGN_UP(struct_->size, align) + type_size(next_ty);
            struct_->align = MAX(struct_->align, align);

        }
        struct_->status = STATUS_RESOLVED;
        return success;
    }
    case OBJ_TYPEDEF:
        SIC_TODO();
    case OBJ_UNION: {
        struct_->status = STATUS_RESOLVING;
        uint32_t largest_size = 0;
        bool success = true;
        for(size_t i = 0; i < struct_->members.size; ++i)
        {
            Type* next_ty = struct_->members.data[i]->var.type;
            if(!resolve_type(c, next_ty, false))
            {
                success = false;
                continue;
            }
            uint32_t next_size = type_size(next_ty);
            if(largest_size < next_size)
            {
                largest_size = next_size;
                struct_->largest_type = next_ty;
            }
        }
        struct_->status = STATUS_RESOLVED;
        return success;
    }
    case OBJ_ENUM_VALUE:
    case OBJ_FUNC:
    case OBJ_INVALID:
    case OBJ_VAR:
        SIC_UNREACHABLE();
    }
    SIC_UNREACHABLE();
}

static bool resolve_array(SemaContext* c, Type* arr_ty)
{
    Type* elem_type = arr_ty->array.elem_type;
    ASTExpr* size_expr = arr_ty->array.size_expr;
    if(!resolve_type(c, elem_type, false))
        return false;

    analyze_expr(c, size_expr);
    // TODO: Check for negative size expr, that is BAD.
    //       For now negative sizes are implicitly casted to ulong, which
    //       means ubyte[-1] tries to allocate quintillions of bytes!! :)
    if(size_expr->kind == EXPR_INVALID || !type_is_integer(size_expr->type) ||
       !implicit_cast(c, size_expr, g_type_ulong))
        return false;

    arr_ty->status = STATUS_RESOLVED;

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
        sema_error(c, &user_ty->unresolved.loc, "Unknown typename.");
        return false;
    }
    switch(type_obj->kind)
    {
    case OBJ_BITFIELD:
    case OBJ_ENUM:
    case OBJ_TYPEDEF:
    case OBJ_STRUCT:
        user_ty->kind = TYPE_STRUCT;
USER_DEF:
        user_ty->user_def = type_obj;
        user_ty->status = STATUS_RESOLVED;
        return resolve_struct_type(c, type_obj, is_pointer);
    case OBJ_UNION:
        user_ty->kind = TYPE_UNION;
        goto USER_DEF;
    case OBJ_FUNC:
    case OBJ_VAR:
    case OBJ_ENUM_VALUE:
        sema_error(c, &user_ty->unresolved.loc, "Symbol does not refer to a type.");
        return false;
    case OBJ_INVALID:
        break;
    }
    SIC_UNREACHABLE();
}
