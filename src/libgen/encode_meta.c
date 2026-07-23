#include "./libgen.h"

#define ROOT_MODULE_INDEX ((uint32_t)-1)
#define INVALID_OFFSET ((uint32_t)-2)

static void generate_module_meta(SiMetaGenContext* c, const ObjModule* module);
static void generate_module_content_meta(SiMetaGenContext* c, const ObjModule* module);
static void generate_function_meta(SiMetaGenContext* c, const ObjFunc* func);

void generate_meta_file(const char* filename)
{
    DBG_ASSERT(filename != NULL);

    size_t old_base = g_global_arena.allocated;

    SiMetaGenContext context = {0};
    da_init(&context.export_table, 0x100);
    da_init(&context.string_table, 0x10000);
    da_init(&context.export_data, 0x10000);

    context.current_module_index = ROOT_MODULE_INDEX;
    generate_module_content_meta(&context, &g_compiler.top_module);

    SiMetaHeader header = {
        .magic = { 'S', 'M', 'E', 'T' },
        .simeta_version = SIMETA_VERSION,
        .string_table_size = context.string_table.size,
        .dependency_table_size = 0,
        .export_table_size = sizeof(SiMetaExportTblHeader) + sizeof(SiMetaExportEntry) * context.export_table.size,
    };

    FILE* fp = fopen(filename, "wb");
    if(fp == NULL)
        sic_fatal_error("Unable to open meta file at path '%s'.", filename);

    fwrite(&header, sizeof(SiMetaHeader), 1, fp);
    fwrite(context.string_table.data, context.string_table.size, 1, fp);
    // TODO: Write dependency table
    SiMetaExportTblHeader export_tbl_header = {
        .export_data_offset = header.export_table_size,
        .count = context.export_table.size,
    };
    fwrite(&export_tbl_header, sizeof(SiMetaExportTblHeader), 1, fp);
    fwrite(context.export_table.data, sizeof(SiMetaExportEntry), context.export_table.size, fp);

    fwrite(context.export_data.data, context.export_data.size, 1, fp);

    fclose(fp);

    g_global_arena.allocated = old_base;
}

static SiMetaExportKind obj_kind_to_meta_kind(ObjKind kind)
{
    switch(kind)
    {
    case OBJ_FUNC:
        return SI_META_EXPORT_FUNCTION;
    case OBJ_VAR:
        return SI_META_EXPORT_GLOBAL;
    case OBJ_IMPORT:
        return SI_META_EXPORT_IMPORT;
    case OBJ_MODULE:
        return SI_META_EXPORT_MODULE;
    case OBJ_BITFIELD:
        return SI_META_EXPORT_BITFIELD;
    case OBJ_ENUM:
        return SI_META_EXPORT_ENUM;
    case OBJ_STRUCT:
        return SI_META_EXPORT_STRUCT;
    case OBJ_TYPEDEF:
        return SI_META_EXPORT_TYPEDEF;
    case OBJ_UNION:
        return SI_META_EXPORT_UNION;
    case OBJ_ENUM_VALUE:
    case OBJ_INVALID:
        break;
    }
    SIC_UNREACHABLE();
}

static void generate_export_meta(SiMetaGenContext* c, const Object* export, uint32_t data_offset)
{
    DBG_ASSERT(export->status == STATUS_RESOLVED);
    if(export->visibility != VIS_PUBLIC) return;

    da_reserve(&c->export_table, c->export_table.size + 1);

    SiMetaExportEntry* entry = c->export_table.data + c->export_table.size;
    entry->kind = obj_kind_to_meta_kind(export->kind);
    entry->parent_module = c->current_module_index;
    entry->data_offset = data_offset;

    entry->name_offset = c->string_table.size;
    da_append_arr(&c->string_table, export->sym, strlen(export->sym) + 1);

    // TODO: Do attributes.

    c->export_table.size++;
}

static void generate_module_meta(SiMetaGenContext* c, const ObjModule* module)
{
    if(module->header.visibility != VIS_PUBLIC) return;
    generate_export_meta(c, &module->header, INVALID_OFFSET);
    DBG_ASSERT(c->export_table.size > 0);
    c->current_module_index = c->export_table.size - 1;
    generate_module_content_meta(c, module);
}

static void generate_module_content_meta(SiMetaGenContext* c, const ObjModule* module)
{
    for(uint32_t i = 0; i < module->funcs.size; ++i)
        generate_function_meta(c, module->funcs.data[i]);

    for(uint32_t i = 0; i < module->submodules.size; ++i)
        generate_module_meta(c, module->submodules.data[i]);
}


static void generate_function_meta(SiMetaGenContext* c, const ObjFunc* func)
{
    if(func->header.visibility != VIS_PUBLIC) return;

    generate_export_meta(c, &func->header, 0);

}
