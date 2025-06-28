#include "x86_globals.h"

const char* x86_64_64bitreg[] = {
    [x86_RAX] = "rax",
    [x86_RBX] = "rbx",
    [x86_RCX] = "rcx",
    [x86_RDX] = "rdx",
    [x86_RSI] = "rsi",
    [x86_RDI] = "rdi",
    [x86_RSP] = "rsp",
    [x86_RBP] = "rbp",
    [x86_R8]  = "r8",
    [x86_R9]  = "r9",
    [x86_R10] = "r10",
    [x86_R11] = "r11",
    [x86_R12] = "r12",
    [x86_R13] = "r13",
    [x86_R14] = "r14",
    [x86_R15] = "r15",
};
const char* x86_64_32bitreg[] = {
    [x86_RAX] = "eax",
    [x86_RBX] = "ebx",
    [x86_RCX] = "ecx",
    [x86_RDX] = "edx",
    [x86_RSI] = "esi",
    [x86_RDI] = "edi",
    [x86_RSP] = "esp",
    [x86_RBP] = "ebp",
    [x86_R8]  = "r8d",
    [x86_R9]  = "r9d",
    [x86_R10] = "r10d",
    [x86_R11] = "r11d",
    [x86_R12] = "r12d",
    [x86_R13] = "r13d",
    [x86_R14] = "r14d",
    [x86_R15] = "r15d",
};
const char* x86_64_16bitreg[] = {
    [x86_RAX] = "ax",
    [x86_RBX] = "bx",
    [x86_RCX] = "cx",
    [x86_RDX] = "dx",
    [x86_RSI] = "si",
    [x86_RDI] = "di",
    [x86_RSP] = "sp",
    [x86_RBP] = "bp",
    [x86_R8]  = "r8w",
    [x86_R9]  = "r9w",
    [x86_R10] = "r10w",
    [x86_R11] = "r11w",
    [x86_R12] = "r12w",
    [x86_R13] = "r13w",
    [x86_R14] = "r14w",
    [x86_R15] = "r15w",
};

const char* x86_64_8bitloreg[] = {
    [x86_RAX] = "al",
    [x86_RBX] = "bl",
    [x86_RCX] = "cl",
    [x86_RDX] = "dl",
    [x86_RSI] = "sil",
    [x86_RDI] = "dil",
    [x86_RSP] = "spl",
    [x86_RBP] = "bpl",
    [x86_R8]  = "r8b",
    [x86_R9]  = "r9b",
    [x86_R10] = "r10b",
    [x86_R11] = "r11b",
    [x86_R12] = "r12b",
    [x86_R13] = "r13b",
    [x86_R14] = "r14b",
    [x86_R15] = "r15b",
};

x86Reg x86_64_param_regs[] = {
    x86_RDI, x86_RSI, x86_RDX, x86_RCX, x86_R8, x86_R9
};
