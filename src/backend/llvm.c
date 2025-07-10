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
static void emit_stmt(CodegenContext* c, ASTStmt* stmt);
static void emit_if(CodegenContext* c, ASTStmt* stmt);
static void emit_while(CodegenContext* c, ASTStmt* stmt);
static LLVMValueRef emit_expr(CodegenContext* c, ASTExpr* expr, bool is_lval);
static LLVMValueRef emit_binary(CodegenContext* c, ASTExpr* expr);
static LLVMValueRef emit_cast(CodegenContext* c, ASTExpr* expr);
static LLVMValueRef emit_constant(CodegenContext* c, ASTExpr* expr);
static LLVMValueRef emit_function_call(CodegenContext* c, ASTExpr* expr);
static void emit_var_alloca(CodegenContext* c, Object* obj);
static LLVMValueRef emit_alloca(CodegenContext* c, const char* name, LLVMTypeRef type, uint32_t align);
static LLVMBasicBlockRef emit_basic_block(CodegenContext* c, const char* label);

static LLVMTypeRef   get_llvm_type(CodegenContext* c, Type* type);
static LLVMTargetRef get_llvm_target(const char* triple);
static void llvm_diag_handler(LLVMDiagnosticInfoRef ref, void *context);

static bool stmt_not_empty(ASTStmt* stmt)
{
    return stmt != NULL &&
           (stmt->kind != STMT_EXPR_STMT || stmt->stmt.expr->kind != EXPR_NOP) &&
           (stmt->kind != STMT_BLOCK || (stmt->stmt.block.body != NULL && stmt_not_empty(stmt->stmt.block.body)));
}

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

    for(size_t i = 0; i < function->func.params.size; ++i)
    {
        Object* param = function->func.params.data[i];
        scratch_clear();
        scratch_appendn(param->symbol.start, param->symbol.len);
        LLVMBuildStore(c->builder, LLVMGetParam(function->llvm_ref, i), param->llvm_ref);
    }

    ASTStmt* body = function->func.body;
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
        LLVMDumpModule(c->module_ref);
        fprintf(stderr, "\n");
        sic_error_fatal("Failed.");
    }
}

static void emit_stmt(CodegenContext* c, ASTStmt* stmt)
{
    switch(stmt->kind)
    {
    case STMT_BLOCK: {
        ASTStmt* body = stmt->stmt.block.body;
        while(body != NULL)
        {
            emit_stmt(c, body);
            body = body->next;
        }
        return;
    }
    case STMT_IF: 
        emit_if(c, stmt);
        break;
    case STMT_SINGLE_DECL: {
        ASTDeclaration* decl = &stmt->stmt.single_decl;
        emit_var_alloca(c, decl->obj);
        if(decl->init_expr != NULL)
            LLVMBuildStore(c->builder, emit_expr(c, decl->init_expr, false), decl->obj->llvm_ref);
        return;
    }
    case STMT_MULTI_DECL:
        for(size_t i = 0; i < stmt->stmt.multi_decl.size; ++i)
        {
            ASTDeclaration* decl = stmt->stmt.multi_decl.data + i;
            emit_var_alloca(c, decl->obj);
            if(decl->init_expr != NULL)
                LLVMBuildStore(c->builder, emit_expr(c, decl->init_expr, false), decl->obj->llvm_ref);
        }
        return;
    case STMT_EXPR_STMT:
        emit_expr(c, stmt->stmt.expr, false);
        return;
    case STMT_RETURN:
        if(c->cur_func->func.ret_type->kind == TYPE_VOID)
            LLVMBuildRetVoid(c->builder);
        else
            LLVMBuildRet(c->builder, emit_expr(c, stmt->stmt.return_.ret_expr, false));
        c->cur_bb = NULL;
        return;
    case STMT_WHILE:
        emit_while(c, stmt);
        return;
    default:
        SIC_UNREACHABLE();
    }
}

static void emit_if(CodegenContext* c, ASTStmt* stmt)
{
    ASTIf* if_stmt = &stmt->stmt.if_;

    LLVMBasicBlockRef exit_block = LLVMCreateBasicBlockInContext(c->global_context, ".if_exit");
    LLVMBasicBlockRef then_block = exit_block;
    LLVMBasicBlockRef else_block = exit_block;

    if(stmt_not_empty(if_stmt->then_stmt))
        then_block = LLVMAppendBasicBlock(c->cur_func->llvm_ref, ".if_then");
    
    if(stmt_not_empty(if_stmt->else_stmt))
        else_block = LLVMAppendBasicBlock(c->cur_func->llvm_ref, ".if_else");

    LLVMValueRef cond = emit_expr(c, if_stmt->cond, false);
    cond = LLVMBuildTrunc(c->builder, cond, LLVMInt1Type(), "");
    if(then_block == exit_block && else_block == exit_block)
        return;

    LLVMAppendExistingBasicBlock(c->cur_func->llvm_ref, exit_block);
    LLVMBuildCondBr(c->builder, cond, then_block, else_block);
    if(then_block != exit_block)
    {
        LLVMPositionBuilderAtEnd(c->builder, then_block);
        emit_stmt(c, if_stmt->then_stmt);
        LLVMBuildBr(c->builder, exit_block);
    }

    if(else_block != exit_block)
    {
        LLVMPositionBuilderAtEnd(c->builder, else_block);
        emit_stmt(c, if_stmt->else_stmt);
        LLVMBuildBr(c->builder, exit_block);
    }
    c->cur_bb = exit_block;
    LLVMPositionBuilderAtEnd(c->builder, exit_block);
}

static void emit_while(CodegenContext* c, ASTStmt* stmt)
{
    ASTWhile* while_stmt = &stmt->stmt.while_;

    LLVMBasicBlockRef cond_block = LLVMAppendBasicBlock(c->cur_func->llvm_ref, ".while_cond");
    LLVMBasicBlockRef body_block = cond_block;
    LLVMBasicBlockRef exit_block;

    if(c->cur_bb)
        LLVMBuildBr(c->builder, cond_block);

    if(stmt_not_empty(while_stmt->body))
        body_block = LLVMAppendBasicBlock(c->cur_func->llvm_ref, ".while_body");

    exit_block = LLVMAppendBasicBlock(c->cur_func->llvm_ref, ".while_exit");

    LLVMPositionBuilderAtEnd(c->builder, cond_block);
    LLVMBuildCondBr(c->builder, emit_expr(c, while_stmt->cond, false), body_block, exit_block);
    
    if(body_block != cond_block)
    {
        LLVMPositionBuilderAtEnd(c->builder, body_block);
        emit_stmt(c, while_stmt->body);
        LLVMBuildBr(c->builder, cond_block);
    }
    
    c->cur_bb = exit_block;
    LLVMPositionBuilderAtEnd(c->builder, exit_block);
}

static LLVMValueRef emit_expr(CodegenContext* c, ASTExpr* expr, bool is_lval)
{
    switch(expr->kind)
    {
    case EXPR_BINARY:
        return emit_binary(c, expr);
    case EXPR_CAST:
        return emit_cast(c, expr);
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
        return LLVMBuildSub(c->builder, lhs, rhs, "");
    case BINARY_MUL:
        return LLVMBuildMul(c->builder, lhs, rhs, "");
    case BINARY_DIV:
        if(type_is_signed(expr->type))
            return LLVMBuildSDiv(c->builder, lhs, rhs, "");
        else if(type_is_unsigned(expr->type))
            return LLVMBuildUDiv(c->builder, lhs, rhs, "");
        else
            return LLVMBuildFDiv(c->builder, lhs, rhs, "");
    case BINARY_MOD:
        if(type_is_signed(expr->type))
            return LLVMBuildSRem(c->builder, lhs, rhs, "");
        else if(type_is_unsigned(expr->type))
            return LLVMBuildURem(c->builder, lhs, rhs, "");
        else
            return LLVMBuildFRem(c->builder, lhs, rhs, "");
    case BINARY_LOG_OR:
    case BINARY_LOG_AND:
        SIC_TODO();
    case BINARY_EQ:
        return type_is_float(binary->lhs->type) ?
                    LLVMBuildFCmp(c->builder, LLVMRealOEQ, lhs, rhs, "") :
                    LLVMBuildICmp(c->builder, LLVMIntEQ, lhs, rhs, "");                    
    case BINARY_NE:
        return type_is_float(binary->lhs->type) ?
                    LLVMBuildFCmp(c->builder, LLVMRealONE, lhs, rhs, "") :
                    LLVMBuildICmp(c->builder, LLVMIntNE, lhs, rhs, "");                    
    case BINARY_LT:
        return type_is_float(binary->lhs->type) ?
                    LLVMBuildFCmp(c->builder, LLVMRealOLT, lhs, rhs, "") :
                    LLVMBuildICmp(c->builder, type_is_signed(binary->lhs->type) ? LLVMIntSLT : LLVMIntULT, lhs, rhs, "");                    
    case BINARY_LE:
        return type_is_float(binary->lhs->type) ?
                    LLVMBuildFCmp(c->builder, LLVMRealOLT, lhs, rhs, "") :
                    LLVMBuildICmp(c->builder, type_is_signed(binary->lhs->type) ? LLVMIntSLE : LLVMIntULE, lhs, rhs, "");                    
    case BINARY_GT:
        return type_is_float(binary->lhs->type) ?
                    LLVMBuildFCmp(c->builder, LLVMRealOGT, lhs, rhs, "") :
                    LLVMBuildICmp(c->builder, type_is_signed(binary->lhs->type) ? LLVMIntSGT : LLVMIntUGT, lhs, rhs, "");                    
    case BINARY_GE:
        return type_is_float(binary->lhs->type) ?
                    LLVMBuildFCmp(c->builder, LLVMRealOGE, lhs, rhs, "") :
                    LLVMBuildICmp(c->builder, type_is_signed(binary->lhs->type) ? LLVMIntSGE : LLVMIntUGE, lhs, rhs, "");                    
    case BINARY_SHL:
        return LLVMBuildShl(c->builder, lhs, rhs, "");
    case BINARY_LSHR:
        return LLVMBuildLShr(c->builder, lhs, rhs, "");
    case BINARY_ASHR:
        return LLVMBuildAShr(c->builder, lhs, rhs, "");
    case BINARY_BIT_OR:
        return LLVMBuildOr(c->builder, lhs, rhs, "");
    case BINARY_BIT_XOR:
        return LLVMBuildXor(c->builder, lhs, rhs, "");
    case BINARY_BIT_AND:
        return LLVMBuildAnd(c->builder, lhs, rhs, "");
    case BINARY_ASSIGN:
        // TODO: Temporary, replace this with a proper method of finding the l-value ref for
        //       the lhs.
        SIC_ASSERT(binary->lhs->kind == EXPR_IDENT);
        return LLVMBuildStore(c->builder, rhs, lhs);
    default:
        SIC_UNREACHABLE();
    }
}

static LLVMValueRef emit_cast(CodegenContext* c, ASTExpr* expr)
{
    ASTExprCast* cast = &expr->expr.cast;
    LLVMValueRef inner = emit_expr(c, cast->expr_to_cast, false);
    LLVMTypeRef to_llvm = get_llvm_type(c, expr->type);
    switch(cast->kind)
    {
    case CAST_FLOAT_TO_SINT:
        return LLVMBuildFPToSI(c->builder, inner, to_llvm, "");
    case CAST_FLOAT_TO_UINT:
        return LLVMBuildFPToUI(c->builder, inner, to_llvm, "");
    case CAST_SINT_TO_FLOAT:
        return LLVMBuildSIToFP(c->builder, inner, to_llvm, "");
    case CAST_UINT_TO_FLOAT:
        return LLVMBuildUIToFP(c->builder, inner, to_llvm, "");
    case CAST_INT_TO_BOOL:
        return LLVMBuildIsNotNull(c->builder, inner, "");
    case CAST_PTR_TO_INT:
        SIC_TODO();
    case CAST_INT_TO_PTR:
        SIC_TODO();
    case CAST_FLOAT_EXT_TRUNC: {
        bool is_widen = type_size(expr->type) > type_size(cast->expr_to_cast->type);
        return is_widen ? LLVMBuildFPExt(c->builder, inner, to_llvm, "") :
                          LLVMBuildFPTrunc(c->builder, inner, to_llvm, "");
    }
    case CAST_SINT_EXT_TRUNC: {
        bool is_widen = type_size(expr->type) > type_size(cast->expr_to_cast->type);
        return is_widen ? LLVMBuildSExt(c->builder, inner, to_llvm, "") :
                          LLVMBuildTrunc(c->builder, inner, to_llvm, "");
    }
    case CAST_UINT_EXT_TRUNC: {
        bool is_widen = type_size(expr->type) > type_size(cast->expr_to_cast->type);
        return is_widen ? LLVMBuildZExt(c->builder, inner, to_llvm, "") :
                          LLVMBuildTrunc(c->builder, inner, to_llvm, "");
    }
    case CAST_REINTERPRET:
        return inner;
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
    case CONSTANT_BOOL:
        SIC_ASSERT(constant->val.i <= 1);
        return LLVMConstInt(LLVMInt1Type(), constant->val.i, false);
    case CONSTANT_FLOAT:
        return LLVMConstReal(get_llvm_type(c, expr->type), constant->val.f);
    case CONSTANT_STRING: {
        LLVMValueRef constant = LLVMConstString(expr->expr.constant.val.s, strlen(expr->expr.constant.val.s), false);
        LLVMValueRef global_string = LLVMAddGlobal(c->module_ref, LLVMTypeOf(constant), ".str");
        LLVMSetInitializer(global_string, constant);
        return global_string;
    }
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
    case TYPE_BOOL:
    case TYPE_U8:
    case TYPE_U16:
    case TYPE_U32:
    case TYPE_U64:
    case TYPE_S8:
    case TYPE_S16:
    case TYPE_S32:
    case TYPE_S64:
        return type->llvm_ref = LLVMIntType(type->builtin.size * 8);
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
