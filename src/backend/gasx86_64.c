#include "codegen-internal.h"
#include "x86_globals.h"
#include "core/core.h"
#include "utils/error.h"

#include <stdarg.h>

#define align_up(x, alignment) (((x) + (alignment) - 1) / (alignment) * (alignment))
#define INT_PARAM_MAX 6 // rdi, rsi, rdx, rcx, r8, r9
#define SSE_PARAM_MAX 8 // xmm0 to xmm7

static FILE*    s_output;
static Object*  s_cur_func;
static ASTNode* s_last_node;

ATTR_PRINTF(1, 2) static void add_line(const char* restrict format, ...);
static void generate_func(Object* program);
static void generate_statement(ASTNode* node);
static void generate_expr(ASTExpr* expr);
static void generate_binary(ASTExpr* expr);
static void assign_offsets(Object* func);
// static const char* register_name(x86Reg reg, uint32_t byte_size);
// static char gpr_suffix(uint32_t byte_size);

void gasx86_64_codegen(const CompilationUnit* unit, FILE* out_file)
{
    SIC_ASSERT(out_file != NULL);

    s_output = out_file;
    add_line("\t.file\t\"%s\"", unit->file.file_name);

    for(size_t i = 0; i < unit->funcs.size; ++i)
    {
        Object* program = unit->funcs.data[i];
        if(program->kind == OBJ_FUNC)
            generate_func(program);
    }

    return;
}

void gasx86_64_assemble(const char* input_path, const char* out_path)
{
    SIC_ASSERT(out_path != NULL);
    SIC_ASSERT(input_path != NULL);
    char *cmd[] = { "as", "--64", "-c", (char*)input_path, "-o", (char*)out_path, NULL };
    run_subprocess(cmd);
}

static void add_line(const char* restrict format, ...)
{
    va_list va;
    va_start(va, format);
    vfprintf(s_output, format, va);
    putc('\n', s_output);
    va_end(va);
}

static void generate_func(Object* func)
{
    s_cur_func = func;
    assign_offsets(func);
    int sym_len = (int)func->symbol.len;
    const char* sym = func->symbol.start;
    ObjFunc* comps = &func->func;
    // Metadata + header
    add_line("\t.text");
    add_line("\t.global\t%.*s", sym_len, sym);
    add_line("\t.type\t%.*s, @function", sym_len, sym);
    add_line("%.*s:", sym_len, sym);

    // Function Prologue
    add_line("\tpushq\t%%rbp");
    add_line("\tmovq\t%%rsp, %%rbp");
    // TODO: Let the function use the red zone in compliance with the AMD64 ABI,
    //       which would save the instructions that modify the stack pointer if
    //       the function is a leaf.
    if(comps->stack_size > 0)
        add_line("\tsubq\t$%d, %%rsp", comps->stack_size);

    int int_cnt = 0;
    // int sse_cnt = 0;
    for(size_t i = 0; i < comps->params.size; ++i)
    {
        ObjVar* param = &comps->params.data[i]->var;
        switch(param->type->kind)
        {
        case TYPE_S8:
        case TYPE_U8:
        case TYPE_S16:
        case TYPE_U16:
            SIC_TODO();
        case TYPE_S32:
        case TYPE_U32:
            if(int_cnt < INT_PARAM_MAX)
            {
                x86Reg reg = x86_64_param_regs[int_cnt];
                add_line("\tmovl\t%%%s, %d(%%rbp)", x86_64_32bitreg[reg], param->offset);
                ++int_cnt;
            }
            break;
        case TYPE_S64:
        case TYPE_U64:
        case TYPE_POINTER:
            if(int_cnt < INT_PARAM_MAX)
            {
                x86Reg reg = x86_64_param_regs[int_cnt];
                add_line("\tmovq\t%%%s, %d(%%rbp)", x86_64_64bitreg[reg], param->offset);
                ++int_cnt;
            }
            break;
        case TYPE_F32:
        case TYPE_F64:
            SIC_TODO();
            break;
        default:
            SIC_UNREACHABLE();
        }
    }

    // Function Epilogue

    for(ASTNode* b = comps->body; b != NULL; b = b->next)
        generate_statement(b);

    add_line(".L.return.%.*s:", sym_len, sym);
    if(comps->stack_size > 0)
        add_line("\tmovq\t%%rbp, %%rsp");
    add_line("\tpopq\t%%rbp");
    add_line("\tret");
    add_line("\t.size\t%.*s, .-%.*s", sym_len, sym, sym_len, sym);
}

static void generate_statement(ASTNode* node)
{
    s_last_node = node;
    switch(node->kind)
    {
    case NODE_BLOCK:
        for(ASTNode* b = node->stmt.block.body; b != NULL; b = b->next)
            generate_statement(b);
        break;
    case NODE_SINGLE_DECL:
        if(node->stmt.single_decl.init_expr == NULL)
            break;
        SIC_TODO();
        break;
    case NODE_MULTI_DECL:
        for(size_t i = 0; i < node->stmt.multi_decl.size; ++i)
        {
            if(node->stmt.single_decl.init_expr == NULL)
                continue;
            SIC_TODO();
        }
        break;
    case NODE_EXPR_STMT:
        SIC_TODO();
        break;
    case NODE_RETURN: {
        if(node->stmt.return_.ret_expr)
            generate_expr(node->stmt.return_.ret_expr);
        add_line("\tjmp\t.L.return.%.*s", (int)s_cur_func->symbol.len, s_cur_func->symbol.start);
        break;
    }
    default:
        SIC_UNREACHABLE();
    }
}

static void generate_expr(ASTExpr* expr)
{
    switch(expr->kind)
    {
    case EXPR_BINARY:
        generate_binary(expr);
        break;
    case EXPR_CAST:
        SIC_TODO();
        break;
    case EXPR_CONSTANT:
        add_line("\tmovq\t$%ld, %%rax", (int64_t)expr->expr.constant.val.i);
        break;
    case EXPR_FUNC_CALL:
        SIC_TODO();
        break;
    case EXPR_IDENT:
        add_line("\tmovq\t%d(%%rbp), %%rax", expr->expr.ident.obj->var.offset);
        break;
    case EXPR_NOP:
        break;
    case EXPR_TERNARY:
        SIC_TODO();
        break;
    case EXPR_UNARY:
        SIC_TODO();
        break;
    default:
        SIC_UNREACHABLE();
    }
}

static void generate_binary(ASTExpr* expr)
{
    ASTExprBinary* binary = &expr->expr.binary;
    switch(binary->kind)
    {
    case BINARY_ADD:
        generate_expr(binary->lhs);
        add_line("\tmovq\t%%rax, %%rdx");
        generate_expr(binary->rhs);
        add_line("\taddq\t%%rdx, %%rax");
        break;
    case BINARY_SUB:
        SIC_TODO();
        break;
    case BINARY_MUL:
        SIC_TODO();
        break;
    case BINARY_DIV:
        SIC_TODO();
        break;
    case BINARY_MOD:
        SIC_TODO();
        break;
    case BINARY_LOG_OR:
        SIC_TODO();
        break;
    case BINARY_LOG_AND:
        SIC_TODO();
        break;
    case BINARY_EQ:
        SIC_TODO();
        break;
    case BINARY_NE:
        SIC_TODO();
        break;
    case BINARY_LT:
        SIC_TODO();
        break;
    case BINARY_LE:
        SIC_TODO();
        break;
    case BINARY_GT:
        SIC_TODO();
        break;
    case BINARY_GE:
        SIC_TODO();
        break;
    case BINARY_SHL:
        SIC_TODO();
        break;
    case BINARY_SHR:
        SIC_TODO();
        break;
    case BINARY_BIT_OR:
        SIC_TODO();
        break;
    case BINARY_BIT_XOR:
        SIC_TODO();
        break;
    case BINARY_BIT_AND:
        SIC_TODO();
        break;
    case BINARY_ASSIGN:
        SIC_TODO();
        break;
    case BINARY_INVALID:
        SIC_UNREACHABLE();
    default:
        SIC_TODO();
    }
}

static void assign_offsets(Object* func)
{
    ObjFunc* comps = &func->func;
    int stack_size = 0;
    for(size_t i = 0; i < comps->local_objs.size; ++i)
    {
        ObjVar* lvar = &comps->local_objs.data[i]->var;
        uint32_t size = type_size(lvar->type);
        stack_size = align_up(stack_size + size, size);
        lvar->offset = -stack_size;
    }
    stack_size = align_up(stack_size, 16);

    int mem_params = 16;
    int int_cnt = 0;
    // int sse_cnt = 0;
    for(size_t i = 0; i < comps->params.size; ++i)
    {
        ObjVar* param = &comps->params.data[i]->var;
        uint32_t size = type_size(param->type);
        switch(param->type->kind)
        {
        case TYPE_S8:
        case TYPE_U8:
        case TYPE_S16:
        case TYPE_U16:
            size = 4;
            FALLTHROUGH;
        case TYPE_S32:
        case TYPE_U32:
        case TYPE_S64:
        case TYPE_U64:
        case TYPE_POINTER:
            if(int_cnt < INT_PARAM_MAX)
            {
                stack_size = align_up(stack_size + size, size);
                param->offset = -stack_size;
            }
            else
            {
                param->offset = mem_params;
                mem_params += 8;
            }
            ++int_cnt;
            break;
        case TYPE_F32:
        case TYPE_F64:
            SIC_TODO();
            break;
        default:
            SIC_UNREACHABLE();
        }
    }
    comps->stack_size = align_up(stack_size, 16);
}

// static const char* register_name(x86Reg reg, uint32_t byte_size)
// {
//     switch(byte_size)
//     {
//     case 1:
//         return x86_64_8bitloreg[reg];
//     case 2:
//         return x86_64_16bitreg[reg];
//     case 4:
//         return x86_64_32bitreg[reg];
//     case 8:
//         return x86_64_64bitreg[reg];
//     default:
//         SIC_UNREACHABLE();
//     }
// }
//
// static char gpr_suffix(uint32_t byte_size)
// {
//     switch(byte_size)
//     {
//     case 1:
//         return 'b';
//     case 2:
//         return 'w';
//     case 4:
//         return 'l';
//     case 8:
//         return 'q';
//     default:
//         SIC_UNREACHABLE();
//     }
// }
