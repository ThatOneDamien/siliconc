#include "codegen-internal.h"
#include "core/cmdline.h"

#include <llvm-c/Analysis.h>
#include <llvm-c/Core.h>
#include <llvm-c/Target.h>
#include <llvm-c/TargetMachine.h>

typedef struct CodegenContext CodegenContext;
struct CodegenContext
{
    const CompilationUnit* unit;
    LLVMTargetMachineRef   target_machine;
    LLVMModuleRef          cur_module;
    const char*            llvm_filename;
    const char*            asm_filename;
    const char*            obj_filename;
};

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

void llvm_codegen(const CompilationUnit* unit)
{
    SIC_ASSERT(s_initialized);
    SIC_ASSERT(unit != NULL);
    CodegenContext c;
    c.unit = unit;

    // TODO: Change this from being hardcoded.
    const char* target_triple = "x86_64-unknown-linux-gnu";
    LLVMContextRef global_context = LLVMGetGlobalContext();
    LLVMTargetRef target = get_llvm_target(target_triple);
    // No optimization for now, this will be changed later along with properly setting up
    // target detection.
    c.target_machine = LLVMCreateTargetMachine(target, target_triple, "generic", "", LLVMCodeGenLevelNone, LLVMRelocPIC, LLVMCodeModelDefault);
    if(c.target_machine == NULL)
        sic_error_fatal("LLVM failed to create target machine, maybe check target triple?");

    c.cur_module = LLVMModuleCreateWithNameInContext(unit->file.full_path, global_context);
    LLVMSetModuleDataLayout(c.cur_module, LLVMCreateTargetDataLayout(c.target_machine));
    LLVMSetSourceFileName(c.cur_module, unit->file.full_path, (uintptr_t)unit->file.path_end - (uintptr_t)unit->file.full_path);

    emit_function(&c, unit->funcs.data[0]);

    if(g_args.emit_ir)
    {
        c.llvm_filename = convert_file_to(&unit->file, FT_LLVM_IR).file_name;
        emit_llvm_ir(&c);
    }

    if(g_args.mode == MODE_COMPILE)
    {
        c.asm_filename = g_args.output_file == NULL ? 
                            convert_file_to(&unit->file, FT_ASM).file_name : 
                            g_args.output_file;
        emit_file(&c, c.asm_filename, LLVMAssemblyFile);
        return;
    }

    if(g_args.emit_asm)
    {
        c.asm_filename = convert_file_to(&unit->file, FT_ASM).file_name;
        emit_file(&c, c.asm_filename, LLVMAssemblyFile);
    }

    if(g_args.mode > MODE_ASSEMBLE)
        c.obj_filename = create_tempfile(FT_OBJ).full_path;
    else if(g_args.mode == MODE_ASSEMBLE)
        c.obj_filename = g_args.output_file == NULL ? 
                            convert_file_to(&unit->file, FT_OBJ).file_name : 
                            g_args.output_file;

    emit_file(&c, c.obj_filename, LLVMObjectFile);
    LLVMDisposeTargetMachine(c.target_machine);
    LLVMDisposeModule(c.cur_module);
}

static void emit_function(CodegenContext* c, Object* function)
{
    LLVMContextRef module_context = LLVMGetModuleContext(c->cur_module);
    LLVMTypeRef func_type = LLVMFunctionType(LLVMVoidTypeInContext(module_context), NULL, 0, false);
    LLVMValueRef func = LLVMAddFunction(c->cur_module, "Test", func_type);
    LLVMBasicBlockRef bb = LLVMCreateBasicBlockInContext(module_context, "entry");
    LLVMBuilderRef builder = LLVMCreateBuilderInContext(module_context);
    LLVMPositionBuilderAtEnd(builder, bb);
    LLVMBuildRetVoid(builder);
    LLVMAppendExistingBasicBlock(func, bb);
    if(LLVMVerifyFunction(func, LLVMPrintMessageAction))
        sic_error_fatal("Failed.");
    LLVMDisposeBuilder(builder);
    (void)func;
    (void)function;
}

static void emit_llvm_ir(CodegenContext* c)
{
    char* error;
    if(LLVMPrintModuleToFile(c->cur_module, c->llvm_filename, &error))
        sic_error_fatal("Failed to emit LLVM IR: \'%s\'", error);
    if(LLVMVerifyModule(c->cur_module, LLVMPrintMessageAction, &error))
    {
        if(error != NULL && *error != '\0')
            sic_error_fatal("Failed to verify LLVM-IR module: %s", error);
        else
            sic_error_fatal("Failed to verify LLVM-IR module. No error supplied.");
    }
}

static void emit_file(CodegenContext* c, const char* out_path, LLVMCodeGenFileType llvm_file_type)
{
    char* error;
    if(LLVMTargetMachineEmitToFile(c->target_machine, c->cur_module, out_path, llvm_file_type, &error))
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
	LLVMDiagnosticSeverity severity = LLVMGetDiagInfoSeverity(ref);
	const char *severity_str;
	switch (severity)
	{
		case LLVMDSError:
			sic_error_fatal("LLVM Error: %s", desc);
		case LLVMDSWarning:
			severity_str = "\033[33mLLVM Warning";
			break;
		case LLVMDSRemark:
			severity_str = "\033[35mremark";
			break;
		case LLVMDSNote:
			severity_str = "\033[36mnote";
			break;
		default:
			severity_str = "message";
			break;
	}

    // TODO: Make this only log for debug.
	printf("[DEBUG] %s\033[0m: %s\n", severity_str, desc);
	LLVMDisposeMessage(desc);
}
