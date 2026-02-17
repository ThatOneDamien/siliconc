#include "semantics.h"

#define OBJ_STACK_SIZE ((1 << 16) - 1)
#define LABEL_STACK_SIZE ((1 << 8) - 1)

typedef struct
{
    Object* data[OBJ_STACK_SIZE];
    uint32_t top;
    uint32_t bottom;
} ObjStack;

static ObjStack s_obj_stack = { 
    .top = OBJ_STACK_SIZE, 
    .bottom = OBJ_STACK_SIZE,
};

typedef struct
{
    SymbolLoc label;
    ASTStmt*  stmt;
} LabeledStmt;

typedef struct
{
    LabeledStmt data[LABEL_STACK_SIZE];
    uint32_t bottom;
} LabelStack;

static LabelStack s_label_stack = {
    .bottom = LABEL_STACK_SIZE,
};


void push_obj(Object* obj)
{
    if(obj->symbol == NULL) return;
    for(uint32_t i = s_obj_stack.bottom; i < s_obj_stack.top; ++i)
    {
        Object* other = s_obj_stack.data[i];
        if(other->symbol == obj->symbol)
        {
            sic_error_redef(obj, other);
            return;
        }
    }
    if(s_obj_stack.bottom == 0)
    {
        sic_fatal_error("Object count in scope has exceeded the capacity of the object stack (%d).", OBJ_STACK_SIZE);
        return;
    }
    s_obj_stack.data[--s_obj_stack.bottom] = obj;
}

ObjModule* find_module(ObjModule* start, SymbolLoc symloc, bool allow_private)
{
    Object* o = hashmap_get(&start->module_ns, symloc.sym);
    if(o == NULL)
    {
        scratch_clear();
        scratch_appendf("Module \'%s\' does not exist in ", symloc.sym);
        if(start == &g_compiler.top_module)
        {
            scratch_append("the root module.");
        }
        else
        {
            scratch_append("module \'");
            scratch_append_module_path(start);
            scratch_append("\'.");
        }
        sic_error_at(symloc.loc, "%s", scratch_string());
        return NULL;
    }

    if(!allow_private && o->visibility == VIS_PRIVATE)
    {
        scratch_clear();
        scratch_appendf("Module \'%s\' is marked as private, and is not accessible from ", symloc.sym);
        if(start == &g_compiler.top_module)
        {
            scratch_append("the root module.");
        }
        else
        {
            scratch_append("module \'");
            scratch_append_module_path(start);
            scratch_append("\'.");
        }
        sic_error_at(symloc.loc, "%s", scratch_string());
        return NULL;
    }
    return obj_as_module(o->kind == OBJ_IMPORT ? obj_as_import(o)->resolved : o);
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
            for(uint32_t i = s_obj_stack.bottom; i < OBJ_STACK_SIZE; ++i)
            {
                o = s_obj_stack.data[i];
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
            if((mod = find_module(mod, path->data[i], false)) == NULL)
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
    uint32_t prev = s_obj_stack.top;
    s_obj_stack.top = s_obj_stack.bottom;
    return prev;
}

void pop_scope(uint32_t old)
{
    DBG_ASSERT(old >= s_obj_stack.top && old <= OBJ_STACK_SIZE);
    s_obj_stack.bottom = s_obj_stack.top;
    s_obj_stack.top = old;
}

static inline LabeledStmt* internal_find_labeled(Symbol label)
{
    for(uint32_t i = s_label_stack.bottom; i < LABEL_STACK_SIZE; ++i)
    {
        if(s_label_stack.data[i].label.sym == label)
            return s_label_stack.data + i;
    }
    return NULL;
}

void push_labeled_stmt(ASTStmt* stmt, SymbolLoc label)
{
    if(label.sym == NULL) return;
    LabeledStmt* prev = internal_find_labeled(label.sym);
    if(prev != NULL)
    {
        sic_diagnostic_at(DIAG_ERROR, label.loc, "Redefinition of label \'%s\'.", label.sym);
        sic_diagnostic_at(DIAG_NOTE, prev->label.loc, "Previous definition here.");
    }
    if(s_label_stack.bottom == 0)
    {
        sic_fatal_error("Nested label count has exceeded the capacity of the label stack (%d).", LABEL_STACK_SIZE);
        return;
    }
    s_label_stack.data[--s_label_stack.bottom] = (LabeledStmt) {
        .label = label,
        .stmt = stmt,
    };
}

void pop_labeled_stmt(ASTStmt* stmt, SymbolLoc label)
{
    if(label.sym == NULL) return;
    DBG_ASSERT(s_label_stack.bottom < LABEL_STACK_SIZE);
    DBG_ASSERT(s_label_stack.data[s_label_stack.bottom].stmt == stmt);
    s_label_stack.bottom++;
}

ASTStmt* find_labeled_stmt(SymbolLoc label)
{
    DBG_ASSERT(label.sym != NULL);
    LabeledStmt* stmt = internal_find_labeled(label.sym);

    if(stmt == NULL)
        sic_error_at(label.loc, "Reference to undefined label \'%s\'.", label.sym);
    return stmt->stmt;
}

void set_object_link_name(Object* obj)
{
    if(obj->link_name != NULL) return;
    scratch_clear();
    scratch_append_obj_link_name(obj);
    obj->link_name = scratch_copy();
}
