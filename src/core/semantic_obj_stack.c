#include "semantics.h"

// I do minus 1 here to keep the stack_top and stack_bottom members
// on the same page as the end of the data array so that there will
// be less cache misses hopefully.
#define OBJ_STACK_SIZE ((1 << 16) - 1)

typedef struct ObjStack ObjStack;
struct ObjStack
{
    Object*  data[OBJ_STACK_SIZE];
    uint32_t stack_top;
    uint32_t stack_bottom;
};

static ObjStack s_obj_stack = { 
    .stack_top = OBJ_STACK_SIZE, 
    .stack_bottom = OBJ_STACK_SIZE,
};

void push_obj(Object* obj)
{
    if(obj->symbol == NULL) return;
    for(uint32_t i = s_obj_stack.stack_bottom; i < s_obj_stack.stack_top; ++i)
    {
        Object* other = s_obj_stack.data[i];
        if(other->symbol == obj->symbol)
        {
            sic_error_redef(obj, other);
            return;
        }
    }
    if(s_obj_stack.stack_bottom <= 0)
    {
        sic_fatal_error("Exceeded maximum defined object count at one time.");
        return;
    }
    s_obj_stack.data[--s_obj_stack.stack_bottom] = obj;
}

Object* find_obj(ModulePath* path)
{
    Module* mod = g_sema->module;
    Object* o;
    Symbol actual;
    if(path->size == 1)
    {
        actual = path->data[0].sym;
        for(uint32_t i = s_obj_stack.stack_bottom; i < OBJ_STACK_SIZE; ++i)
        {
            o = s_obj_stack.data[i];
            if(o->symbol == actual)
                return o;
        }
    }
    else
    {
        actual = path->data[path->size - 1].sym;
        for(uint32_t i = 0; i < path->size - 1; ++i)
        {
            for(uint32_t j = 0; j < mod->submodules.size; ++j)
            {
                Module* other = mod->submodules.data[j];
                if(other->name == path->data[i].sym)
                {
                    mod = other;
                    goto NEXT;
                }
            }
            sic_error_at(path->data[i].loc, "Module \'%s\' does not contain submodule \'%s\'.",
                         mod->name, path->data[i].sym);
            return NULL;
        NEXT:;
        }
        module_declare_all(mod);
    }
    return hashmap_get(&mod->symbol_map, actual);
}

uint32_t push_scope()
{
    uint32_t prev = s_obj_stack.stack_top;
    s_obj_stack.stack_top = s_obj_stack.stack_bottom;
    return prev;
}

void pop_scope(uint32_t old)
{
    s_obj_stack.stack_bottom = s_obj_stack.stack_top;
    s_obj_stack.stack_top = old;
}
