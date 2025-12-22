#include "internal.h"

static void declare_global_obj(Module* module, Object* global);
static bool analyze_main(Object* main);

void module_declare_all(Module* module)
{
    SIC_ASSERT(module != NULL);
    if(module->has_declared) return;
    for(uint32_t i = 0; i < module->types.size; ++i)
        declare_global_obj(module, module->types.data[i]);

    for(uint32_t i = 0; i < module->vars.size; ++i)
        declare_global_obj(module, module->vars.data[i]);

    for(uint32_t i = 0; i < module->funcs.size; ++i)
        declare_global_obj(module, module->funcs.data[i]);

    module->has_declared = true;
}

Module* module_add_submodule(Module* parent, Symbol name, SourceLoc loc, bool is_inline)
{
    for(uint32_t i = 0; i < parent->submodules.size; ++i)
    {
        Module* other = parent->submodules.data[i];
        if(other->name == name)
        {
            sic_diagnostic_at(loc, DIAG_ERROR, "Module with name \'%s\' already exists.", name);
            sic_diagnostic_at(other->loc, DIAG_NOTE, "Previous definition here.");
        }
    }

    Module* new_mod = CALLOC_STRUCT(Module);
    new_mod->name = name;
    new_mod->loc = loc;
    new_mod->parent = parent;
    new_mod->is_inline = is_inline;
    da_append(&parent->submodules, new_mod);
    return new_mod;
}

static void declare_global_obj(Module* module, Object* global)
{
    HashMap* map = &module->symbol_map;
    Object* other = hashmap_get(map, global->symbol);

    if(other != NULL)
        sic_error_redef(global, other);

    hashmap_put(map, global->symbol, global);

    if(global->symbol == g_sym_main && !analyze_main(global))
    {
        global->status = STATUS_RESOLVED;
        global->kind = OBJ_INVALID;
    }
}

static bool analyze_main(Object* main)
{
    if(main->kind != OBJ_FUNC)
    {
        sic_error_at(main->loc, "Symbol 'main' is reserved for entry function.");
        return false;
    }

    if(g_compiler.main_function != NULL)
    {
        sic_error_redef(main, g_compiler.main_function);
        return false;
    }

    FuncSignature* sig = main->func.signature;
    TypeKind rt_kind = sig->ret_type->kind;
    if(rt_kind != TYPE_INT && rt_kind != TYPE_VOID)
        goto BAD_SIG;

    if(sig->params.size >= 1 && sig->params.data[0]->type->kind != TYPE_INT)
        goto BAD_SIG;

    Type* second;
    if(sig->params.size >= 2)
    {
        second = sig->params.data[1]->type;
        if(second->kind != TYPE_POINTER)
            goto BAD_SIG;
        second = second->pointer_base;
        if(second->kind != TYPE_POINTER)
            goto BAD_SIG;
        second = second->pointer_base;
        if(second->kind != TYPE_CHAR)
            goto BAD_SIG;
    }

    g_compiler.main_function = main;
    return true;

BAD_SIG:
    sic_error_at(main->loc, "The signature of the main function is invalid. "
                            "The return type should be 'int' or 'void', with "
                            "optional parameters 'int, char**'.");
    return false;
}

