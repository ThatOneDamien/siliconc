#pragma once

typedef enum
{
    x86_RAX,
    x86_RBX,
    x86_RCX,
    x86_RDX,
    x86_RSI,
    x86_RDI,
    x86_RSP,
    x86_RBP,
} x86Reg;

extern const char* x86_64_64bitreg[];
extern const char* x86_64_32bitreg[];
extern const char* x86_64_16bitreg[];
extern const char* x86_64_8bithireg[];
extern const char* x86_64_8bitloreg[];
