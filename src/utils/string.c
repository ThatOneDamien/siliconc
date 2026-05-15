#include "lib.h"
#include "../core/internal.h"
#include <stdarg.h>

ScratchBuffer g_scratch = {{0}, 0};

void scratch_appendf(const char* fmt, ...)
{
    va_list va;
    va_start(va, fmt);
    int size = vsnprintf(g_scratch.data + g_scratch.len, SCRATCH_SIZE - g_scratch.len, fmt, va);
    if(size + g_scratch.len > SCRATCH_SIZE)
        sic_fatal_error("Ran out of space in the scratch buffer. This shouldn't happen.");
    g_scratch.len += size;
    va_end(va);
}

static ObjModuleDA module_path_to_stack(ObjModule* module)
{
    ObjModuleDA path = {0};
    while(module != NULL)
    {
        da_append(&path, module);
        module = module->parent;
    }
    return path;
}

void scratch_append_module_path(const ObjModule* module)
{
    if(module == &g_compiler.top_module)
    {
        scratch_append("root");
        return;
    }

    struct {
        const ObjModule** data;
        uint32_t size;
        uint32_t capacity;
    } path = {0};
    while(module != NULL && module != &g_compiler.top_module)
    {
        da_append(&path, module);
        module = module->parent;
    }

    if(path.size > 0)
        scratch_append(path.data[path.size - 1]->header.sym);
    for(uint32_t i = path.size - 2; i < path.size; --i)
    {
        scratch_appendn("::", 2);
        scratch_append(path.data[i]->header.sym);
    }
}

void scratch_append_obj_link_name(Object* obj)
{
    scratch_append("_SI");
    if(obj->kind == OBJ_FUNC)
    {
        scratch_appendc(obj_as_func(obj)->is_method ? 'M' : 'F');
    }
    else if(obj->kind == OBJ_VAR)
    {
        scratch_appendc('G');
    }
    else
        SIC_UNREACHABLE();

    ObjModuleDA path = module_path_to_stack(obj->module);
    // TODO: When we are making a library, we want the name of the library to
    //       be appended. For an executable it doesn't matter.
    // if(path.data[path.size - 1] == &g_compiler.top_module && IS_LIBRARY)
    size_t len;
    for(uint32_t i = path.size - 2; i < path.size; --i)
    {
        len = strlen(path.data[i]->header.sym);
        scratch_appendf("%zu", len);
        scratch_appendn(path.data[i]->header.sym, len);
    }

    if(obj->kind == OBJ_FUNC)
    {
        ObjFunc* func = obj_as_func(obj);
        if(func->is_method)
        {
            len = strlen(func->method_parent.sym);
            scratch_appendf("%zu", len);
            scratch_appendn(func->method_parent.sym, len);
        }
    }

    len = strlen(obj->sym);
    scratch_appendf("%zu", len);
    scratch_appendn(obj->sym, len);
}

void scratch_append_typename(const Type* type)
{
    DBG_ASSERT(type != NULL);
    DBG_ASSERT(type->status != STATUS_UNRESOLVED);
    switch(type->kind)
    {
    case TYPE_VOID:
    case NUMERIC_TYPES:
        static_assert(TYPE_INT - TYPE_VOID + TOKEN_VOID == TOKEN_INT, "Check enum conversion");
        if(type->qualifiers & TYPE_QUAL_CONST) scratch_append("const ");
        scratch_append(tok_kind_to_str(type->kind - TYPE_VOID + TOKEN_VOID)); // Convert type enum to token enum
        return;
    case TYPE_POINTER_SINGLE:
        if(type->qualifiers & TYPE_QUAL_CONST) scratch_append("const ");
        scratch_appendc('*');
        scratch_append_typename(type->pointer.base);
        return;
    case TYPE_POINTER_MULTI_STATIC:
        if(type->qualifiers & TYPE_QUAL_CONST) scratch_append("const ");
        scratch_appendf("*[%lu]", type->pointer.static_len);
        scratch_append_typename(type->pointer.base);
        return;
    case TYPE_POINTER_MULTI_UNKNOWN:
        if(type->qualifiers & TYPE_QUAL_CONST) scratch_append("const ");
        scratch_append("*[*]");
        scratch_append_typename(type->pointer.base);
        return;
    case TYPE_FUNC_PTR: {
        if(type->qualifiers & TYPE_QUAL_CONST) scratch_append("const ");
        FuncSignature* sig = type->func_ptr;
        scratch_append("fn (");
        if(sig->params.size > 0)
            scratch_append_typename(sig->params.data[0]->type_loc.type);
        for(uint32_t i = 1; i < sig->params.size; ++i)
        {
            scratch_appendc(',');
            scratch_append_typename(sig->params.data[i]->type_loc.type);
        }
        scratch_append(") -> ");
        scratch_append_typename(sig->ret_type.type);
        return;
    }
    case TYPE_SLICE:
        if(type->qualifiers & TYPE_QUAL_CONST) scratch_append("const ");
        scratch_append("[]");
        scratch_append_typename(type->slice.base);
        return;
    case TYPE_OPTIONAL:
        if(type->qualifiers & TYPE_QUAL_CONST) scratch_append("const ");
        scratch_appendc('?');
        scratch_append_typename(type->optional.base);
        return;
    case TYPE_STATIC_ARRAY:
        // Dont print qualifiers for array, they shouldn't have any
        scratch_appendf("[%lu]", type->array.static_len);
        scratch_append_typename(type->array.elem_type);
        return;
    case TYPE_ALIAS:
        if(type->qualifiers & TYPE_QUAL_CONST) scratch_append("const ");
        // TODO: Probably needs changing. This isnt a very good way to print the type, but I want to
        // somehow express that it is an alias and not a distinct type.
        scratch_append(obj_as_typedef(type->user_def)->header.sym);
        scratch_append(" a.k.a (");
        scratch_append_typename(type->canonical);
        scratch_appendc(')');
        return;
    case TYPE_ALIAS_DISTINCT:
        if(type->qualifiers & TYPE_QUAL_CONST) scratch_append("const ");
        scratch_append(obj_as_typedef(type->user_def)->header.sym);
        return;
    case TYPE_ENUM:
    case TYPE_ENUM_DISTINCT:
        if(type->qualifiers & TYPE_QUAL_CONST) scratch_append("const ");
        scratch_append(obj_as_enum(type->user_def)->header.sym);
        return;
    case TYPE_STRUCT: {
        if(type->qualifiers & TYPE_QUAL_CONST) scratch_append("const ");
        Symbol s = obj_as_struct(type->user_def)->header.sym;
        scratch_append(s ? s : "anonymous struct");
        return;
    }
    case TYPE_UNION: {
        if(type->qualifiers & TYPE_QUAL_CONST) scratch_append("const ");
        Symbol s = obj_as_struct(type->user_def)->header.sym;
        scratch_append(s ? s : "anonymous union");
        return;
    }
    case TYPE_INIT_LIST:
        scratch_append("initializer list");
        return;
    case TYPE_STRING_LITERAL:
        scratch_append("string");
        return;
    case TYPE_NULL:
        scratch_append("null");
        return;
    case TYPE_INVALID:
    case __TYPE_COUNT:
    case TYPE_INFERRED_ARRAY:
    case TYPE_TYPEOF:
    case TYPE_UNRESOLVED_USER:
        break;
    }
    SIC_UNREACHABLE();
}

char* str_format(const char* fmt, ...)
{
    va_list va;
    va_start(va, fmt);
    int size = vsnprintf(NULL, 0, fmt, va);
    va_end(va);

    char* buf = MALLOC(size + 1, sizeof(char));

    va_start(va, fmt);
    vsnprintf(buf, size + 1, fmt, va);
    va_end(va);
    buf[size] = '\0';
    return buf;
}

char* str_dupn(const char* str, size_t len)
{
    DBG_ASSERT(str != NULL);
    // TODO: Replace this with dedicated string arena allocation
    //       to reduce fragmentation.
    char* res = MALLOC(len + 1, 1);
    memcpy(res, str, len);
    res[len] = '\0';
    return res;
}
