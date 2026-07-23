#include "core/sicompiler.h"

#define SILIB_VERSION  1
#define SIMETA_VERSION 1

typedef struct SiMetaHeader          SiMetaHeader;
typedef struct SiMetaExportEntry     SiMetaExportEntry;
typedef struct SiMetaExportEntryDA   SiMetaExportEntryDA;
typedef struct SiMetaExportTblHeader SiMetaExportTblHeader;
typedef struct SiMetaGenContext      SiMetaGenContext;
typedef struct SiLibEntry            SiLibEntry;
typedef struct SiLibHeader           SiLibHeader;

typedef enum : uint8_t
{
    SI_META_EXPORT_FUNCTION,
    SI_META_EXPORT_GLOBAL,
    SI_META_EXPORT_MODULE,
    SI_META_EXPORT_IMPORT,

    SI_META_EXPORT_BITFIELD,
    SI_META_EXPORT_ENUM,
    SI_META_EXPORT_STRUCT,
    SI_META_EXPORT_TYPEDEF,
    SI_META_EXPORT_UNION,
} SiMetaExportKind;

struct PACKED SiMetaHeader
{
    char     magic[4]; // SMET
    uint16_t simeta_version;
    uint16_t flags;

    uint32_t string_table_size;
    uint32_t dependency_table_size;
    uint32_t export_table_size;
};

static_assert(sizeof(SiMetaHeader) == 20, "Bad header size.");

struct PACKED SiMetaExportEntry
{
    SiMetaExportKind kind;
    uint8_t          __padding[3];
    uint32_t         parent_module;    // Module index into the export table.
    uint32_t         name_offset;      // As it is exposed in the library
    uint32_t         data_offset;      // Offset INTO export_data_offset of the header.
    uint32_t         attribute_offset; // Offset INTO export_data_offset of the header.
};

struct SiMetaExportEntryDA
{
    SiMetaExportEntry* data;
    uint32_t size;
    uint32_t capacity;
};

struct PACKED SiMetaExportTblHeader
{
    uint32_t count;
    uint32_t export_data_offset; // Offset from tbl header in bytes
    SiMetaExportEntry entries[];
};

struct SiMetaGenContext
{
    SiMetaExportEntryDA export_table;
    StringBuilder       string_table;
    RawDataDA           export_data;
    uint32_t            current_module_index;
};

struct PACKED SiLibEntry
{
    uint32_t name_offset;
    uint32_t name_length;
    uint64_t data_offset;
    uint64_t data_size;
};

struct PACKED SiLibHeader
{
    char       magic[4]; // SLIB
    uint16_t   silib_version;
    uint16_t   library_version;
    uint16_t   file_count;
    uint16_t   metadata_entry_index;
    uint32_t   data_start_offset;
    SiLibEntry entries[];
};

static_assert(sizeof(SiLibHeader) == 16, "Bad header size.");

void generate_meta_file(const char* filename);
void decode_meta_file(const char* filename);
