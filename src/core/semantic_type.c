#include "semantics.h"

static bool resolve_array(SemaContext* c, Type* arr_ty);
static bool resolve_struct(SemaContext* c, Object* type_obj, bool is_pointer);
static bool resolve_union(SemaContext* c, Object* type_obj, bool is_pointer);
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
    if(!resolve_type(c, elem_type, false))
        return false;

    analyze_expr(c, size_expr);
    // TODO: Check for negative size expr, that is BAD.
    //       For now negative sizes are implicitly casted to ulong, which
    //       means ubyte[-1] tries to allocate quintillions of bytes!! :)
    if(expr_is_bad(size_expr) || !type_is_integer(size_expr->type) ||
       !implicit_cast(c, &arr_ty->array.size_expr, g_type_ulong))
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

bool resolve_enum(SemaContext* c, Object* type_obj)
{
    (void)c;
    ObjEnum* enum_ = &type_obj->enum_;
    if(enum_->status == STATUS_RESOLVED)
        return true;
    if(enum_->status == STATUS_RESOLVING)
    {
        sic_error_at(type_obj->loc, "Circular enum definition.");
        return false;
    }

    enum_->status = STATUS_RESOLVING;
    if(enum_->underlying == NULL)
        enum_->underlying = g_type_int;
    else if(!resolve_type(c, enum_->underlying, false) ||
            !type_is_integer(enum_->underlying))
    {
        sic_error_at(type_obj->loc, "Expected integral underlying type for enum.");
        return false;
    }

    bool success = true;
    push_scope(c);
    uint64_t last_value = -1;
    for(uint32_t i = 0; i < enum_->values.size; ++i)
    {
        Object* value = enum_->values.data[i];
        value->enum_val.type = enum_->underlying;
        ASTExpr* val_expr = value->enum_val.value;
        if(val_expr == NULL)
            value->enum_val.const_val = ++last_value;
        else if(!analyze_expr(c, val_expr))
            continue;
        else if(val_expr->kind != EXPR_CONSTANT)
        {
            sic_error_at(value->loc, "Enum value must be assigned a constant integer expression.");
            continue;
        }
        else if(!implicit_cast(c, &value->enum_val.value, enum_->underlying))
            continue;
        else
            last_value = value->enum_val.const_val = val_expr->expr.constant.val.i;
        declare_obj(c, value);
    }
    pop_scope(c);
    enum_->status = STATUS_RESOLVED;
    return success;
}

static bool resolve_struct(SemaContext* c, Object* type_obj, bool is_pointer)
{
    ObjStruct* struct_ = &type_obj->struct_;
    if(struct_->status == STATUS_RESOLVED)
        return true;
    if(struct_->status == STATUS_RESOLVING)
    {
        if(is_pointer)
            return true;
        sic_error_at(type_obj->loc, "Circular structure definition.");
        return false;
    }

    struct_->status = STATUS_RESOLVING;
    bool success = true;
    for(uint32_t i = 0; i < struct_->members.size; ++i)
    {
        Type* next_ty = struct_->members.data[i]->type;
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

static bool resolve_union(SemaContext* c, Object* type_obj, bool is_pointer)
{
    ObjStruct* struct_ = &type_obj->struct_;
    if(struct_->status == STATUS_RESOLVED)
        return true;
    if(struct_->status == STATUS_RESOLVING)
    {
        if(is_pointer)
            return true;
        sic_error_at(type_obj->loc, "Recursive structure definition.");
        return false;
    }

    struct_->status = STATUS_RESOLVING;
    uint32_t largest_size = 0;
    bool success = true;
    for(size_t i = 0; i < struct_->members.size; ++i)
    {
        Type* next_ty = struct_->members.data[i]->type;
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

static bool resolve_user(SemaContext* c, Type* user_ty, bool is_pointer)
{
    Object* type_obj = find_obj(c, user_ty->unresolved.sym);
    if(type_obj == NULL)
    {
        sic_error_at(user_ty->unresolved.loc, "Unknown typename.");
        return false;
    }
    user_ty->user_def = type_obj;
    switch(type_obj->kind)
    {
    case OBJ_BITFIELD:
        SIC_TODO();
    case OBJ_ENUM:
        user_ty->kind = TYPE_ENUM;
        user_ty->status = STATUS_RESOLVED;
        return resolve_enum(c, type_obj);
    case OBJ_TYPEDEF:
    case OBJ_STRUCT:
        user_ty->kind = TYPE_STRUCT;
        user_ty->status = STATUS_RESOLVED;
        return resolve_struct(c, type_obj, is_pointer);
    case OBJ_UNION:
        user_ty->kind = TYPE_UNION;
        user_ty->status = STATUS_RESOLVED;
        return resolve_union(c, type_obj, is_pointer);
    case OBJ_FUNC:
    case OBJ_VAR:
    case OBJ_ENUM_VALUE:
        sic_error_at(user_ty->unresolved.loc, "Symbol does not refer to a type.");
        return false;
    case OBJ_INVALID:
        break;
    }
    SIC_UNREACHABLE();
}
