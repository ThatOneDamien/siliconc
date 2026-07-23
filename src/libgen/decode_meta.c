#include "./libgen.h"

void NORETURN invalid_meta_file(const char* filename, const char* reason)
{
    if(reason != NULL)
        sic_fatal_error("Invalid meta file '%s'.", filename);
    else
        sic_fatal_error("Invalid meta file '%s': %s", filename, reason);
}

void decode_meta_file(const char* filename)
{
    FILE* fp = fopen(filename, "rb");
    if(fp == NULL)
        sic_fatal_error("Unable to open meta file at path '%s'.", filename);

    SiMetaHeader header;
    fread(&header, sizeof(SiMetaHeader), 1, fp);

    if(header.simeta_version < SIMETA_VERSION)
        sic_fatal_error("Meta file '%s' is out of date.", filename);
    else if(header.simeta_version > SIMETA_VERSION)
        sic_fatal_error("Meta file '%s' has invalid version.", filename);

    if(memcmp(header.magic, "SMET", 4) != 0)
        invalid_meta_file(filename, "Bad header.");

    char* string_table = MALLOC(header.string_table_size, 1);
    fread(string_table, header.string_table_size, 1, fp);

    fclose(fp);
}
