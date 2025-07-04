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

    LLVMContextRef         global_context;
    LLVMTargetMachineRef   target_machine;
    LLVMModuleRef          module_ref;
};

static void gen_module(CodegenContext* c, Module* module);
static void gen_unit(CodegenContext* c, CompilationUnit* unit);
static void emit_function(CodegenContext* c, Object* function);
static void emit_llvm_ir(CodegenContext* c);
static void emit_file(CodegenContext* c, const char* out_path, LLVMCodeGenFileType llvm_file_type);
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

    for(size_t i = 0; i < modules->size; ++i)
        gen_module(&c, modules->data[i]);

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
        emit_function(c, unit->funcs.data[i]);
}

static void emit_function(CodegenContext* c, Object* function)
{
    LLVMTypeRef func_type = LLVMFunctionType(LLVMInt32Type(), NULL, 0, false);
    scratch_appendn(function->symbol.start, function->symbol.len);
    LLVMValueRef func = LLVMAddFunction(c->module_ref, scratch_string(), func_type);
    LLVMBasicBlockRef bb = LLVMAppendBasicBlock(func, "entry");
    LLVMBuilderRef builder = LLVMCreateBuilder();
    LLVMPositionBuilderAtEnd(builder, bb);
    LLVMBuildRet(builder, LLVMConstInt(LLVMInt32Type(), 69, false));
    if(LLVMVerifyFunction(func, LLVMPrintMessageAction))
        sic_error_fatal("Failed.");
    LLVMDisposeBuilder(builder);
    (void)func;
    (void)function;
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
