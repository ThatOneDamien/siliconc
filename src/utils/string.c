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
        g_scratch.len = 0;
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
        scratch_append(path.data[path.size - 1]->header.symbol);
    for(uint32_t i = path.size - 2; i < path.size; --i)
    {
        scratch_appendn("::", 2);
        scratch_append(path.data[i]->header.symbol);
    }
}

void scratch_append_obj_link_name(Object* obj)
{
    if(obj->link_name)
    {
        scratch_append(obj->link_name);
        return;
    }

    scratch_append("_SI");
    if(obj->kind == OBJ_FUNC)
    {
        scratch_appendc('F');
    }
    else if(obj->kind == OBJ_VAR)
    {
        scratch_appendc('G');
    }
    else
        SIC_TODO();

    ObjModuleDA path = module_path_to_stack(obj->module);
    // TODO: When we are making a library, we want the name of the library to
    //       be appended. For an executable it doesn't matter.
    // if(path.data[path.size - 1] == &g_compiler.top_module && IS_LIBRARY)
    for(uint32_t i = path.size - 2; i < path.size; --i)
    {
        size_t len = strlen(path.data[i]->header.symbol);
        scratch_appendf("%zu", len);
        scratch_appendn(path.data[i]->header.symbol, len);
    }

    size_t len = strlen(obj->symbol);
    scratch_appendf("%zu", len);
    scratch_appendn(obj->symbol, len);
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
    DBG_ASSERT(len != 0);
    // TODO: Replace this with dedicated string arena allocation
    //       to reduce fragmentation.
    char* res = MALLOC(len + 1, 1);
    memcpy(res, str, len);
    res[len] = '\0';
    return res;
}
