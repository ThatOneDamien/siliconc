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

ObjModule* find_module(ObjModule* start, SymbolLoc symloc, bool allow_private)
{
    Object* next = hashmap_get(&start->module_ns, symloc.sym);
    if(next == NULL)
    {
        SIC_ERROR_DBG("Here");
        // TODO: Make the message display the full path of the module.
        sic_error_at(symloc.loc, "Module \'%s\' does not exist in module \'%s\'.",
                     symloc.sym, start->header.symbol);
        return NULL;
    }
    if(!allow_private && next->visibility == VIS_PRIVATE)
    {
        // TODO: Make the message display the full path of the module.
        sic_error_at(symloc.loc, "Module \'%s\' is marked as private and is not accessible from module \'%s\'.",
                     symloc.sym, start->header.symbol);
        return NULL;
    }
    return obj_as_module(next->kind == OBJ_IMPORT ? obj_as_import(next)->resolved : next);
}

Object* find_obj(ModulePath* path)
{
    ObjModule* mod = g_sema->module;
    Object* o;
    SymbolLoc last = path->data[path->size - 1];
    bool allow_private;
    if(path->size == 1)
    {
        allow_private = true;
        if(!g_sema->in_global_init)
        {
            for(uint32_t i = g_obj_stack.stack_bottom; i < OBJ_STACK_SIZE; ++i)
            {
                o = g_obj_stack.data[i];
                if(o->symbol == last.sym)
                    return o;
            }
        }
    }
    else
    {
        allow_private = false;
        if((mod = find_module(mod, path->data[0], true)) == NULL)
            return NULL;
        for(uint32_t i = 1; i < path->size - 1; ++i)
        {
            if((mod = find_module(mod, path->data[0], false)) == NULL)
                return NULL;
        }
    }
    o = hashmap_get(&mod->symbol_ns, last.sym);
    if(o == NULL)
    {
        sic_error_at(last.loc, "Reference to undefined symbol \'%s\'.", last.sym);
        return NULL;
    }

    if(!allow_private && o->visibility == VIS_PRIVATE)
    {
        sic_error_at(last.loc, "Symbol \'%s\' is marked private. Mark it as public if "
                               "you wish to use it externally.", last.sym);
        return NULL;
    }
    // TODO: Check visibility rules.
    return o->kind == OBJ_IMPORT ? obj_as_import(o)->resolved : o;
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
