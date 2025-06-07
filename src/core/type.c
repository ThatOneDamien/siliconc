#include "internal.h"

Type* g_type_void = &(Type){ TYPE_VOID,  1,  1 };
Type* g_type_u8   = &(Type){ TYPE_U8  ,  1,  1 };
Type* g_type_s8   = &(Type){ TYPE_S8  ,  1,  1 };
Type* g_type_u16  = &(Type){ TYPE_U16 ,  2,  2 };
Type* g_type_s16  = &(Type){ TYPE_S16 ,  2,  2 };
Type* g_type_u32  = &(Type){ TYPE_U32 ,  4,  4 };
Type* g_type_s32  = &(Type){ TYPE_S32 ,  4,  4 };
Type* g_type_u64  = &(Type){ TYPE_U64 ,  8,  8 };
Type* g_type_s64  = &(Type){ TYPE_S64 ,  8,  8 };
Type* g_type_f32  = &(Type){ TYPE_F32 ,  4,  4 };
Type* g_type_f64  = &(Type){ TYPE_F64 ,  8,  8 };
