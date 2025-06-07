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
};
const char* x86_64_8bithireg[] = {
    [x86_RAX] = "ah",
    [x86_RBX] = "bh",
    [x86_RCX] = "ch",
    [x86_RDX] = "dh",
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
};
