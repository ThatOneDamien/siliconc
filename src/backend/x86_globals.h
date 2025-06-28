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
    x86_R8,
    x86_R9,
    x86_R10,
    x86_R11,
    x86_R12,
    x86_R13,
    x86_R14,
    x86_R15,
} x86Reg;

extern const char* x86_64_64bitreg[];
extern const char* x86_64_32bitreg[];
extern const char* x86_64_16bitreg[];
extern const char* x86_64_8bitloreg[];
extern x86Reg      x86_64_param_regs[];
