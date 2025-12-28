#include "semantics.h"

ObjStack g_obj_stack = { 
    .stack_top = OBJ_STACK_SIZE, 
    .stack_bottom = OBJ_STACK_SIZE,
};

void push_obj(Object* obj)
{
    if(obj->symbol == NULL) return;
    for(uint32_t i = g_obj_stack.stack_bottom; i < g_obj_stack.stack_top; ++i)
    {
        Object* other = g_obj_stack.data[i];
        if(other->symbol == obj->symbol)
        {
            sic_error_redef(obj, other);
            return;
        }
    }
    if(g_obj_stack.stack_bottom <= 0)
    {
        sic_fatal_error("Object count in scope has exceeded the capacity of the object stack (%d).", OBJ_STACK_SIZE);
        return;
    }
    g_obj_stack.data[--g_obj_stack.stack_bottom] = obj;
}

Object* find_obj(ModulePath* path)
{
    Module* mod = g_sema->module;
    Object* o;
    SymbolLoc last = path->data[path->size - 1];
    if(!g_sema->in_global_init && path->size == 1)
    {
        for(uint32_t i = g_obj_stack.stack_bottom; i < OBJ_STACK_SIZE; ++i)
        {
            o = g_obj_stack.data[i];
            if(o->symbol == last.sym)
                return o;
        }
    }
    else
    {
        for(uint32_t i = 0; i < path->size - 1; ++i)
        {
            Object* next = hashmap_get(&mod->module_ns, path->data[i].sym);
            if(next == NULL)
            {
                // TODO: Make the message display the full path of the module.
                sic_error_at(path->data[i].loc, "Module \'%s\' does not exist in the current module.",
                             mod->name);
                return NULL;
            }
            if(next->visibility == VIS_PRIVATE)
            {
                // TODO: Make the message display the full path of the module.
                sic_error_at(path->data[i].loc, "Module \'%s\' is marked as private and is not accessible from current module.",
                             path->data[i].sym);
                return NULL;
            }
            mod = next->kind == OBJ_IMPORT ? next->import->module : next->module;
        }
    }
    o = hashmap_get(&mod->symbol_ns, last.sym);
    if(o == NULL)
    {
        sic_error_at(last.loc, "Reference to undefined symbol \'%s\'.", last.sym);
        return NULL;
    }
    // TODO: Check visibility rules.
    return o->kind == OBJ_IMPORT ? o->import : o;
}

uint32_t push_scope()
{
    uint32_t prev = g_obj_stack.stack_top;
    g_obj_stack.stack_top = g_obj_stack.stack_bottom;
    return prev;
}

void pop_scope(uint32_t old)
{
    g_obj_stack.stack_bottom = g_obj_stack.stack_top;
    g_obj_stack.stack_top = old;
}
