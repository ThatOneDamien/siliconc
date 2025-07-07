#include "codegen-internal.h"

#include <llvm-c/Analysis.h>
#include <llvm-c/Core.h>
#include <llvm-c/Target.h>
#include <llvm-c/TargetMachine.h>

typedef struct CodegenContext CodegenContext;
struct CodegenContext
{
    const Module*          module;
    const CompilationUnit* unit;
    const char*            llvm_filename;
    const char*            asm_filename;
    const char*            obj_filename;
    Object*                cur_func;

    LLVMContextRef         global_context;
    LLVMTargetMachineRef   target_machine;
    LLVMModuleRef          module_ref;
    LLVMBuilderRef         builder;
    LLVMValueRef           alloca_ref;
    LLVMBasicBlockRef      cur_bb;

    LLVMTypeRef            ptr_type;
};

static void gen_module(CodegenContext* c, Module* module);
static void gen_unit(CodegenContext* c, CompilationUnit* unit);
static void emit_llvm_ir(CodegenContext* c);
static void emit_file(CodegenContext* c, const char* out_path, LLVMCodeGenFileType llvm_file_type);
static void decl_function(CodegenContext* c, Object* function);
static void emit_function_body(CodegenContext* c, Object* function);
static void emit_stmt(CodegenContext* c, ASTNode* stmt);
static LLVMValueRef emit_expr(CodegenContext* c, ASTExpr* expr, bool is_lval);
static LLVMValueRef emit_binary(CodegenContext* c, ASTExpr* expr);
static LLVMValueRef emit_constant(CodegenContext* c, ASTExpr* expr);
static LLVMValueRef emit_function_call(CodegenContext* c, ASTExpr* expr);
static void emit_var_alloca(CodegenContext* c, Object* obj);
static LLVMValueRef emit_alloca(CodegenContext* c, const char* name, LLVMTypeRef type, uint32_t align);
static LLVMBasicBlockRef emit_basic_block(CodegenContext* c, const char* label);

static LLVMTypeRef   get_llvm_type(CodegenContext* c, Type* type);
static LLVMTargetRef get_llvm_target(const char* triple);
static void llvm_diag_handler(LLVMDiagnosticInfoRef ref, void *context);

static bool s_initialized = false;

void llvm_initialize()
{
    SIC_ASSERT(!s_initialized);
    LLVMInitializeX86AsmParser();
    LLVMInitializeX86AsmPrinter();
    LLVMInitializeX86TargetInfo();
    LLVMInitializeX86Target();
    LLVMInitializeX86TargetMC();
    LLVMContextSetDiagnosticHandler(LLVMGetGlobalContext(), llvm_diag_handler, NULL);
    s_initialized = true;
}


void llvm_codegen(ModulePTRDA* modules)
{
    SIC_ASSERT(s_initialized);
    SIC_ASSERT(modules != NULL);
    SIC_ASSERT(modules->size != 0);
    // TODO: Temporary, remove this line
    g_compiler.top_module.name = "default";
    CodegenContext c;
    c.global_context = LLVMGetGlobalContext();
    c.ptr_type = LLVMPointerType(LLVMInt8Type(), 0);
    c.builder = LLVMCreateBuilder();
    c.cur_bb = NULL;

    for(size_t i = 0; i < modules->size; ++i)
        gen_module(&c, modules->data[i]);

    LLVMDisposeBuilder(c.builder);
}

static void gen_module(CodegenContext* c, Module* module)
{
    c->module = module;
    // TODO: Change this from being hardcoded.
    const char* target_triple = "x86_64-pc-linux-gnu";
    LLVMTargetRef target = get_llvm_target(target_triple);
    // No optimization for now, this will be changed later along with properly setting up
    // target detection.
    c->target_machine = LLVMCreateTargetMachine(target, target_triple, "generic", "", LLVMCodeGenLevelNone, LLVMRelocPIC, LLVMCodeModelDefault);
    if(c->target_machine == NULL)
        sic_error_fatal("LLVM failed to create target machine, maybe check target triple?");

    c->module_ref = LLVMModuleCreateWithNameInContext(module->name, c->global_context);
    LLVMSetModuleDataLayout(c->module_ref, LLVMCreateTargetDataLayout(c->target_machine));
    LLVMSetTarget(c->module_ref, target_triple);

    for(size_t i = 0; i < module->units.size; ++i)
        gen_unit(c, module->units.data[i]);

    if(g_args.emit_ir)
    {
        c->llvm_filename = convert_ext_to(module->name, FT_LLVM_IR);
        emit_llvm_ir(c);
    }

    if(g_args.mode == MODE_COMPILE)
    {
        c->asm_filename = g_args.output_file == NULL ? 
                            convert_ext_to(module->name, FT_ASM) : 
                            g_args.output_file;
        emit_file(c, c->asm_filename, LLVMAssemblyFile);
        return;
    }

    if(g_args.emit_asm)
    {
        c->asm_filename = convert_ext_to(module->name, FT_ASM);
        emit_file(c, c->asm_filename, LLVMAssemblyFile);
    }

    if(g_args.mode > MODE_ASSEMBLE)
        c->obj_filename = create_tempfile(FT_OBJ).full_path;
    else if(g_args.mode == MODE_ASSEMBLE)
        c->obj_filename = g_args.output_file == NULL ? 
                            convert_ext_to(module->name, FT_OBJ) : 
                            g_args.output_file;

    emit_file(c, c->obj_filename, LLVMObjectFile);
    da_append(&g_compiler.linker_inputs, sifile_new(c->obj_filename));
    LLVMDisposeTargetMachine(c->target_machine);
    LLVMDisposeModule(c->module_ref);

}

static void gen_unit(CodegenContext* c, CompilationUnit* unit)
{
    for(size_t i = 0; i < unit->funcs.size; ++i)
        decl_function(c, unit->funcs.data[i]);

    for(size_t i = 0; i < unit->funcs.size; ++i)
        emit_function_body(c, unit->funcs.data[i]);
}

static void decl_function(CodegenContext* c, Object* function)
{
    // Emit parameters
    LLVMTypeRef* param_types = NULL;
    if(function->func.params.size != 0)
        param_types = MALLOC(sizeof(LLVMTypeRef) * function->func.params.size);
    for(size_t i = 0; i < function->func.params.size; ++i)
        param_types[i] = get_llvm_type(c, function->func.params.data[i]->var.type);
    LLVMTypeRef func_type = LLVMFunctionType(get_llvm_type(c, function->func.ret_type), param_types, function->func.params.size, false);

    scratch_clear();
    scratch_appendn(function->symbol.start, function->symbol.len);
    LLVMValueRef func = LLVMAddFunction(c->module_ref, scratch_string(), func_type);
    function->llvm_ref = func;
    function->func.llvm_func_type = func_type;
}

static void emit_function_body(CodegenContext* c, Object* function)
{
    SIC_ASSERT(function->llvm_ref);
    if(function->func.body == NULL)
        return;
    c->cur_func = function;
    emit_basic_block(c, "");
    c->alloca_ref = LLVMBuildAlloca(c->builder, LLVMInt32Type(), "__alloca_ptr");

    for(size_t i = 0; i < function->func.params.size; ++i)
    {
        Object* param = function->func.params.data[i];
        scratch_clear();
        scratch_appendn(param->symbol.start, param->symbol.len);
        param->llvm_ref = emit_alloca(c, scratch_string(), get_llvm_type(c, param->var.type), type_alignment(param->var.type));
    }

    ASTNode* body = function->func.body;
    while(body != NULL)
    {
        emit_stmt(c, body);
        body = body->next;
    }

    if(c->cur_bb != NULL)
    {
        SIC_ASSERT(function->func.ret_type->kind == TYPE_VOID);
        LLVMBuildRetVoid(c->builder);
    }

    if(LLVMGetInstructionParent(c->alloca_ref))
    {
        LLVMInstructionEraseFromParent(c->alloca_ref);
        c->alloca_ref = NULL;
    }

    if(LLVMVerifyFunction(function->llvm_ref, LLVMPrintMessageAction))
    {
        fprintf(stderr, "\n");
        sic_error_fatal("Failed.");
    }
}

static void emit_stmt(CodegenContext* c, ASTNode* stmt)
{
    switch(stmt->kind)
    {
    case NODE_BLOCK: {
        ASTNode* body = stmt->stmt.block.body;
        while(body != NULL)
        {
            emit_stmt(c, body);
            body = body->next;
        }
        break;
    }
    case NODE_SINGLE_DECL: {
        ASTDeclaration* decl = &stmt->stmt.single_decl;
        emit_var_alloca(c, decl->obj);
        if(decl->init_expr != NULL)
            LLVMBuildStore(c->builder, emit_expr(c, decl->init_expr, false), decl->obj->llvm_ref);
        break;
    }
    case NODE_MULTI_DECL:
        for(size_t i = 0; i < stmt->stmt.multi_decl.size; ++i)
        {
            ASTDeclaration* decl = stmt->stmt.multi_decl.data + i;
            emit_var_alloca(c, decl->obj);
            if(decl->init_expr != NULL)
                LLVMBuildStore(c->builder, emit_expr(c, decl->init_expr, false), decl->obj->llvm_ref);
        }
        break;
    case NODE_EXPR_STMT:
        emit_expr(c, stmt->stmt.expr, false);
        break;
    case NODE_RETURN:
        if(c->cur_func->func.ret_type->kind == TYPE_VOID)
            LLVMBuildRetVoid(c->builder);
        else
            LLVMBuildRet(c->builder, emit_expr(c, stmt->stmt.return_.ret_expr, false));
        c->cur_bb = NULL;
        break;
    default:
        SIC_UNREACHABLE();
    }
}

static LLVMValueRef emit_expr(CodegenContext* c, ASTExpr* expr, bool is_lval)
{
    switch(expr->kind)
    {
    case EXPR_BINARY:
        return emit_binary(c, expr);
    case EXPR_CAST:
        SIC_TODO();
    case EXPR_CONSTANT:
        return emit_constant(c, expr);
    case EXPR_FUNC_CALL:
        return emit_function_call(c, expr);
    case EXPR_IDENT: {
        Object* var = expr->expr.ident;
        SIC_ASSERT(var->llvm_ref != NULL);
        return is_lval ? var->llvm_ref : LLVMBuildLoad2(c->builder, get_llvm_type(c, var->var.type), var->llvm_ref, "");
    }
    case EXPR_NOP:
        return NULL;
    case EXPR_TERNARY:
        SIC_TODO();
    case EXPR_UNARY:
        SIC_TODO();
    default:
        SIC_UNREACHABLE();
    }
}

static LLVMValueRef emit_binary(CodegenContext* c, ASTExpr* expr)
{
    ASTExprBinary* binary = &expr->expr.binary;
    LLVMValueRef lhs = emit_expr(c, binary->lhs, binary->kind == BINARY_ASSIGN);
    LLVMValueRef rhs = emit_expr(c, binary->rhs, false);
    switch(binary->kind)
    {
    case BINARY_ADD:
        return LLVMBuildAdd(c->builder, lhs, rhs, "");
    case BINARY_SUB:
        SIC_TODO();
    case BINARY_MUL:
        SIC_TODO();
    case BINARY_DIV:
        SIC_TODO();
    case BINARY_MOD:
        SIC_TODO();
    case BINARY_LOG_OR:
        SIC_TODO();
    case BINARY_LOG_AND:
        SIC_TODO();
    case BINARY_EQ:
        SIC_TODO();
    case BINARY_NE:
        SIC_TODO();
    case BINARY_LT:
        SIC_TODO();
    case BINARY_LE:
        SIC_TODO();
    case BINARY_GT:
        SIC_TODO();
    case BINARY_GE:
        SIC_TODO();
    case BINARY_SHL:
        SIC_TODO();
    case BINARY_SHR:
        SIC_TODO();
    case BINARY_BIT_OR:
        SIC_TODO();
    case BINARY_BIT_XOR:
        SIC_TODO();
    case BINARY_BIT_AND:
        SIC_TODO();
    case BINARY_ASSIGN:
        // TODO: Temporary, replace this with a proper method of finding the l-value ref for
        //       the lhs.
        SIC_ASSERT(binary->lhs->kind == EXPR_IDENT);
        return LLVMBuildStore(c->builder, rhs, lhs);
    default:
        SIC_UNREACHABLE();
    }
}

static LLVMValueRef emit_constant(CodegenContext* c, ASTExpr* expr)
{
    ASTExprConstant* constant = &expr->expr.constant;
    switch(constant->kind)
    {
    case CONSTANT_INTEGER:
        return LLVMConstInt(get_llvm_type(c, expr->type), constant->val.i, false);
    case CONSTANT_FLOAT:
        SIC_TODO();
    case CONSTANT_STRING:
        SIC_TODO();
    default:
        SIC_UNREACHABLE();
    }
}

static LLVMValueRef emit_function_call(CodegenContext* c, ASTExpr* expr)
{
    // TODO: Remove temporary
    ASTExprCall* call = &expr->expr.call;
    Object* function = call->func_expr->expr.ident;
    SIC_ASSERT(call->func_expr->kind == EXPR_IDENT);
    SIC_ASSERT(function->llvm_ref != NULL);

    LLVMValueRef* args = NULL;
    if(call->args.size > 0)
        args = MALLOC(sizeof(LLVMValueRef) * call->args.size);
    for(size_t i = 0; i < call->args.size; ++i)
        args[i] = emit_expr(c, call->args.data[i], false);
    return LLVMBuildCall2(c->builder, function->func.llvm_func_type, function->llvm_ref, args, call->args.size, "");
}

static void emit_var_alloca(CodegenContext* c, Object* obj)
{
    SIC_ASSERT(obj->kind == OBJ_VAR);
    scratch_clear();
    scratch_appendn(obj->symbol.start, obj->symbol.len);
    obj->llvm_ref = emit_alloca(c, scratch_string(), get_llvm_type(c, obj->var.type), type_alignment(obj->var.type));
}

static LLVMValueRef emit_alloca(CodegenContext* c, const char* name, LLVMTypeRef type, uint32_t align)
{
    LLVMBasicBlockRef bb = LLVMGetInsertBlock(c->builder);
    LLVMPositionBuilderBefore(c->builder, c->alloca_ref);
    LLVMValueRef new_alloca = LLVMBuildAlloca(c->builder, type, name);
    LLVMSetAlignment(new_alloca, align);
    LLVMPositionBuilderAtEnd(c->builder, bb);
    return new_alloca;
}

static LLVMBasicBlockRef emit_basic_block(CodegenContext* c, const char* label)
{
    SIC_ASSERT(c->cur_bb == NULL);
    c->cur_bb = LLVMAppendBasicBlock(c->cur_func->llvm_ref, label);
    LLVMPositionBuilderAtEnd(c->builder, c->cur_bb);
    return c->cur_bb;
}

static void emit_llvm_ir(CodegenContext* c)
{
    char* error;
    if(LLVMVerifyModule(c->module_ref, LLVMPrintMessageAction, &error))
    {
        if(error != NULL && *error != '\0')
            sic_error_fatal("Failed to verify LLVM-IR module: %s", error);
        else
            sic_error_fatal("Failed to verify LLVM-IR module. No error supplied.");
    }
    if(LLVMPrintModuleToFile(c->module_ref, c->llvm_filename, &error))
        sic_error_fatal("Failed to emit LLVM IR: \'%s\'", error);
}

static void emit_file(CodegenContext* c, const char* out_path, LLVMCodeGenFileType llvm_file_type)
{
    char* error;
    if(LLVMTargetMachineEmitToFile(c->target_machine, c->module_ref, out_path, llvm_file_type, &error))
        sic_error_fatal("LLVM failed to emit file \'%s\': %s", out_path, error);
}

static LLVMTypeRef get_llvm_type(CodegenContext* c, Type* type)
{
    if(type->llvm_ref != NULL)
        return type->llvm_ref;
    switch(type->kind)
    {
    case TYPE_VOID:
        return type->llvm_ref = LLVMVoidType();
    case TYPE_S8:
    case TYPE_U8:
    case TYPE_S16:
    case TYPE_U16:
    case TYPE_S32:
    case TYPE_U32:
    case TYPE_S64:
    case TYPE_U64:
        return type->llvm_ref = LLVMIntType(type->builtin.size * 8);
        break;
    case TYPE_F32:
        return type->llvm_ref = LLVMFloatType();
    case TYPE_F64:
        return type->llvm_ref = LLVMDoubleType();
    case TYPE_POINTER:
        return type->llvm_ref = c->ptr_type;
    default:
        SIC_UNREACHABLE();
    }
}

static LLVMTargetRef get_llvm_target(const char* triple)
{
    char* error;
    LLVMTargetRef ref;
    if(LLVMGetTargetFromTriple(triple, &ref, &error))
        sic_error_fatal("Failed to get target from triple \'%s\': %s", triple, error);
    return ref;
}

static void llvm_diag_handler(LLVMDiagnosticInfoRef ref, UNUSED void *context)
{
	char *desc = LLVMGetDiagInfoDescription(ref);
	const char *severity;
	switch (LLVMGetDiagInfoSeverity(ref))
	{
		case LLVMDSError:
			sic_error_fatal("LLVM Error: %s", desc);
		case LLVMDSWarning:
			severity = "\033[33mLLVM Warning";
			break;
		case LLVMDSRemark:
			severity = "\033[35mLLVM Remark";
			break;
		case LLVMDSNote:
			severity = "\033[36mLLVM Note";
			break;
		default:
			severity = "message";
			break;
	}

    // TODO: Make this only log for debug.
	printf("[DEBUG] %s\033[0m: %s\n", severity, desc);
	LLVMDisposeMessage(desc);
}
