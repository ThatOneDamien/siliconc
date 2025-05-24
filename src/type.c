#include "type.h"

Type* t_void = &(Type){ TYPE_VOID,  1,  1 };
Type* t_u8   = &(Type){ TYPE_U8  ,  1,  1 };
Type* t_s8   = &(Type){ TYPE_S8  ,  1,  1 };
Type* t_u16  = &(Type){ TYPE_U16 ,  2,  2 };
Type* t_s16  = &(Type){ TYPE_S16 ,  2,  2 };
Type* t_u32  = &(Type){ TYPE_U32 ,  4,  4 };
Type* t_s32  = &(Type){ TYPE_S32 ,  4,  4 };
Type* t_u64  = &(Type){ TYPE_U64 ,  8,  8 };
Type* t_s64  = &(Type){ TYPE_S64 ,  8,  8 };
Type* t_f32  = &(Type){ TYPE_F32 ,  4,  4 };
Type* t_f64  = &(Type){ TYPE_F64 ,  8,  8 };
Type* t_f128 = &(Type){ TYPE_F128, 16, 16 };
