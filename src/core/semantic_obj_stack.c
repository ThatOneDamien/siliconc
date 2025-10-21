#include "semantics.h"

#define OBJ_STACK_SIZE (1 << 16)

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
    if(obj->symbol == NULL)
        return;
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

Object* find_obj(Symbol symbol)
{
    Object* o;
    for(uint32_t i = s_obj_stack.stack_bottom; i < OBJ_STACK_SIZE; ++i)
    {
        o = s_obj_stack.data[i];
        if(o->symbol == symbol)
            return o;
    }
    o = hashmap_get(&g_sema.unit->priv_symbols, symbol);
    if(o != NULL)
        return o;
    return hashmap_get(&g_sema.unit->module->public_symbols, symbol);
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
