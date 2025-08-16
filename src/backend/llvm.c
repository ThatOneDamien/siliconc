#include "codegen-internal.h"

#include <llvm-c/Analysis.h>
#include <llvm-c/Core.h>
#include <llvm-c/Target.h>
#include <llvm-c/TargetMachine.h>

typedef struct CodegenContext CodegenContext;
typedef struct GenValue       GenValue;
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
    LLVMValueRef           swap_ref;
    LLVMBasicBlockRef      cur_bb;

    LLVMTypeRef            ptr_type;
};

typedef enum
{
    GEN_VAL_RVALUE = 0,
    GEN_VAL_ADDRESS,
} GenValueKind;

struct GenValue
{
    LLVMValueRef value;
    Type*        type;
    GenValueKind kind;
};

static void     gen_module(CodegenContext* c, Module* module);
static void     gen_unit_decls(CodegenContext* c, CompilationUnit* unit);
static void     gen_unit_bodies(CodegenContext* c, CompilationUnit* unit);
static void     emit_llvm_ir(CodegenContext* c);
static void     emit_file(CodegenContext* c, const char* out_path, LLVMCodeGenFileType llvm_file_type);
static void     decl_function(CodegenContext* c, Object* function);
static void     decl_global_var(CodegenContext* c, Object* var);
static void     emit_function_body(CodegenContext* c, Object* function);
static void     emit_stmt(CodegenContext* c, ASTStmt* stmt);
static void     emit_block_stmt(CodegenContext* c, ASTStmt* stmt);
static void     emit_for(CodegenContext* c, ASTStmt* stmt);
static void     emit_if(CodegenContext* c, ASTStmt* stmt);
static void     emit_swap(CodegenContext* c, ASTStmt* stmt);
static void     emit_switch(CodegenContext* c, ASTStmt* stmt);
static void     emit_while(CodegenContext* c, ASTStmt* stmt);
static GenValue emit_expr(CodegenContext* c, ASTExpr* expr);
static void     emit_array_access(CodegenContext* c, ASTExpr* expr, GenValue* result);
static void     emit_binary(CodegenContext* c, ASTExpr* expr, GenValue* result);
static void     emit_cast(CodegenContext* c, ASTExpr* expr, GenValue* inner, GenValue* result);
static void     emit_constant(CodegenContext* c, ASTExpr* expr, GenValue* result);
static void     emit_function_call(CodegenContext* c, ASTExpr* expr, GenValue* result);
static void     emit_ident(ASTExpr* expr, GenValue* result);
static void     emit_incdec(CodegenContext* c, ASTExpr* expr, GenValue* inner, GenValue* result, bool is_post);
static void     emit_member_access(CodegenContext* c, ASTExpr* expr, GenValue* result);
static void     emit_logical_andor(CodegenContext* c, GenValue* lhs, ASTExpr* rhs, GenValue* result, bool is_or);
static void     emit_ternary(CodegenContext* c, ASTExpr* expr, GenValue* result);
static void     emit_unary(CodegenContext* c, ASTExpr* expr, GenValue* result);

static void     emit_add(CodegenContext* c, GenValue* left, GenValue* right, GenValue* result);
static void     emit_assign(CodegenContext* c, GenValue* ptr, GenValue* value, GenValue* result);
static void     emit_br(CodegenContext* c, LLVMBasicBlockRef block);

static void              emit_var_alloca(CodegenContext* c, Object* obj);
static LLVMValueRef      emit_alloca(CodegenContext* c, const char* name, LLVMTypeRef type, uint32_t align);
static LLVMBasicBlockRef append_new_basic_block(CodegenContext* c, const char* label);
static void              append_old_basic_block(CodegenContext* c, LLVMBasicBlockRef block);
static LLVMBasicBlockRef create_basic_block(CodegenContext* c, const char* label);
static void              use_basic_block(CodegenContext* c, LLVMBasicBlockRef block);
static LLVMTypeRef       get_llvm_type(CodegenContext* c, Type* type);
static void              load_rvalue(CodegenContext* c, GenValue* lvalue);
static LLVMTargetRef     get_llvm_target(const char* triple);
static void              llvm_diag_handler(LLVMDiagnosticInfoRef ref, void *context);

static bool stmt_not_empty(ASTStmt* stmt)
{
    return stmt != NULL &&
           (stmt->kind != STMT_EXPR_STMT || stmt->stmt.expr->kind != EXPR_NOP) &&
           (stmt->kind != STMT_BLOCK || stmt_not_empty(stmt->stmt.block.body));
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
        sic_fatal_error("LLVM failed to create target machine, maybe check target triple?");

    c->module_ref = LLVMModuleCreateWithNameInContext(module->name, c->global_context);
    LLVMSetModuleDataLayout(c->module_ref, LLVMCreateTargetDataLayout(c->target_machine));
    LLVMSetTarget(c->module_ref, target_triple);

    for(size_t i = 0; i < module->units.size; ++i)
        gen_unit_decls(c, module->units.data[i]);

    for(size_t i = 0; i < module->units.size; ++i)
        gen_unit_bodies(c, module->units.data[i]);

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
        c->obj_filename = create_tempfile(FT_OBJ);
    else if(g_args.mode == MODE_ASSEMBLE)
        c->obj_filename = g_args.output_file == NULL ? 
                            convert_ext_to(module->name, FT_OBJ) : 
                            g_args.output_file;

    emit_file(c, c->obj_filename, LLVMObjectFile);
    da_append(&g_compiler.linker_inputs, c->obj_filename);
    LLVMDisposeTargetMachine(c->target_machine);
    LLVMDisposeModule(c->module_ref);

}

static void gen_unit_decls(CodegenContext* c, CompilationUnit* unit)
{
    c->unit = unit;
    for(size_t i = 0; i < unit->funcs.size; ++i)
        decl_function(c, unit->funcs.data[i]);

    for(size_t i = 0; i < unit->vars.size; ++i)
        decl_global_var(c, unit->vars.data[i]);
}

static void gen_unit_bodies(CodegenContext* c, CompilationUnit* unit)
{
    c->unit = unit;
    for(size_t i = 0; i < unit->funcs.size; ++i)
    {
        c->cur_bb = NULL;
        emit_function_body(c, unit->funcs.data[i]);
    }
}

static void decl_function(CodegenContext* c, Object* function)
{
    // Emit parameters
    LLVMTypeRef* param_types = NULL;
    FuncSignature* sig = function->func.signature;
    if(sig->params.size != 0)
        param_types = MALLOC_STRUCTS(LLVMTypeRef, sig->params.size);

    for(size_t i = 0; i < sig->params.size; ++i)
        param_types[i] = get_llvm_type(c, sig->params.data[i]->var.type);
    sig->llvm_func_type = LLVMFunctionType(get_llvm_type(c, sig->ret_type), param_types, sig->params.size, sig->is_var_arg);

    scratch_clear();
    scratch_append(function->symbol);
    function->llvm_ref = LLVMAddFunction(c->module_ref, scratch_string(), sig->llvm_func_type);
}

static void decl_global_var(CodegenContext* c, Object* var)
{
    scratch_clear();
    scratch_append(var->symbol);
    var->llvm_ref = LLVMAddGlobal(c->module_ref, get_llvm_type(c, var->var.type), scratch_string());
    // TODO: Fix this!!!
    LLVMSetInitializer(var->llvm_ref, LLVMConstInt(get_llvm_type(c, var->var.type), 0, false));
}

static void emit_function_body(CodegenContext* c, Object* function)
{
    SIC_ASSERT(function->llvm_ref);
    if(function->func.body == NULL)
        return;
    c->cur_func = function;
    append_old_basic_block(c, create_basic_block(c, ""));
    c->alloca_ref = LLVMBuildAlloca(c->builder, LLVMInt32Type(), ".alloca_ptr");
    if(function->func.swap_stmt_size > 0)
    {
        c->swap_ref = emit_alloca(c, ".swap_space", 
                                  LLVMArrayType2(LLVMInt8Type(), function->func.swap_stmt_size), 
                                  function->func.swap_stmt_align);
    }

    FuncSignature* sig = function->func.signature;

    for(size_t i = 0; i < sig->params.size; ++i)
    {
        Object* param = sig->params.data[i];
        scratch_clear();
        scratch_append(param->symbol);
        param->llvm_ref = emit_alloca(c, scratch_string(), get_llvm_type(c, param->var.type), type_alignment(param->var.type));
    }

    for(size_t i = 0; i < sig->params.size; ++i)
    {
        Object* param = sig->params.data[i];
        scratch_clear();
        scratch_append(param->symbol);
        LLVMBuildStore(c->builder, LLVMGetParam(function->llvm_ref, i), param->llvm_ref);
    }

    emit_block_stmt(c, function->func.body);

    if(c->cur_bb != NULL)
    {
        SIC_ASSERT(sig->ret_type->kind == TYPE_VOID);
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
        sic_fatal_error("Failed.");
    }
}

static void emit_stmt(CodegenContext* c, ASTStmt* stmt)
{
    if(stmt == NULL || c->cur_bb == NULL) // Check if this is a label, for now we dont have that.
        return;
    switch(stmt->kind)
    {
    case STMT_BLOCK:
        emit_block_stmt(c, stmt->stmt.block.body);
        return;
    case STMT_BREAK:
    case STMT_CONTINUE:
        SIC_TODO();
    case STMT_EXPR_STMT:
        emit_expr(c, stmt->stmt.expr);
        return;
    case STMT_FOR:
        emit_for(c, stmt);
        return;
    case STMT_GOTO:
        SIC_TODO();
    case STMT_IF: 
        emit_if(c, stmt);
        return;
    case STMT_LABEL:
        SIC_TODO();
    case STMT_MULTI_DECL:
        for(size_t i = 0; i < stmt->stmt.multi_decl.size; ++i)
        {
            ASTDeclaration* decl = stmt->stmt.multi_decl.data + i;
            emit_var_alloca(c, decl->obj);
            if(decl->init_expr != NULL)
            {
                GenValue init = emit_expr(c, decl->init_expr);
                load_rvalue(c, &init);
                LLVMBuildStore(c->builder, init.value, decl->obj->llvm_ref);
            }
        }
        return;
    case STMT_NOP:
        return;
    case STMT_RETURN:
        if(c->cur_func->func.signature->ret_type->kind == TYPE_VOID)
            LLVMBuildRetVoid(c->builder);
        else
        {
            GenValue ret_expr = emit_expr(c, stmt->stmt.return_.ret_expr);
            load_rvalue(c, &ret_expr);
            LLVMBuildRet(c->builder, ret_expr.value);
        }
        c->cur_bb = NULL;
        return;
    case STMT_SINGLE_DECL: {
        ASTDeclaration* decl = &stmt->stmt.single_decl;
        emit_var_alloca(c, decl->obj);
        GenValue var;
        var.type = decl->obj->var.type;
        var.value = decl->obj->llvm_ref;
        var.kind = GEN_VAL_ADDRESS;
        if(decl->init_expr != NULL)
        {
            GenValue init = emit_expr(c, decl->init_expr);
            load_rvalue(c, &init);
            GenValue temp;
            emit_assign(c, &var, &init, &temp);
        }
        return;
    }
    case STMT_SWAP:
        emit_swap(c, stmt);
        return;
    case STMT_SWITCH:
        emit_switch(c, stmt);
        return;
    case STMT_WHILE:
        emit_while(c, stmt);
        return;
    case STMT_INVALID:
    case STMT_TYPE_DECL:
        break;
    }
    SIC_UNREACHABLE();
}

static void emit_block_stmt(CodegenContext* c, ASTStmt* stmt)
{
    while(stmt != NULL)
    {
        emit_stmt(c, stmt);
        stmt = stmt->next;
    }
}

static void emit_for(CodegenContext* c, ASTStmt* stmt)
{
    ASTFor* for_stmt = &stmt->stmt.for_;

    LLVMBasicBlockRef exit_block = create_basic_block(c, ".for_exit");
    LLVMBasicBlockRef body_block = create_basic_block(c, ".for_body");
    LLVMBasicBlockRef cond_block = body_block;
    if(stmt_not_empty(for_stmt->init_stmt))
        emit_stmt(c, for_stmt->init_stmt);

    if(for_stmt->cond_expr != NULL)
    {
        cond_block = append_new_basic_block(c, ".for_cond");
        GenValue cond = emit_expr(c, for_stmt->cond_expr);
        load_rvalue(c, &cond);
        LLVMBuildCondBr(c->builder, cond.value, body_block, exit_block);
    }
    else
        emit_br(c, body_block);
    
    append_old_basic_block(c, body_block);
    emit_stmt(c, for_stmt->body);
    
    if(for_stmt->loop_expr != NULL)
    {
        append_new_basic_block(c, ".for_loop");
        emit_expr(c, for_stmt->loop_expr);
    }

    emit_br(c, cond_block);
    append_old_basic_block(c, exit_block);
}

static void emit_if(CodegenContext* c, ASTStmt* stmt)
{
    ASTIf* if_stmt = &stmt->stmt.if_;

    LLVMBasicBlockRef exit_block = create_basic_block(c, ".if_exit");
    LLVMBasicBlockRef then_block = exit_block;
    LLVMBasicBlockRef else_block = exit_block;

    GenValue cond = emit_expr(c, if_stmt->cond);
    load_rvalue(c, &cond);

    if(stmt_not_empty(if_stmt->then_stmt))
        then_block = create_basic_block(c, ".if_then");
    
    if(stmt_not_empty(if_stmt->else_stmt))
        else_block = create_basic_block(c, ".if_else");

    if(then_block == exit_block && else_block == exit_block)
        return;

    LLVMBuildCondBr(c->builder, cond.value, then_block, else_block);
    if(then_block != exit_block)
    {
        append_old_basic_block(c, then_block);
        emit_stmt(c, if_stmt->then_stmt);
        emit_br(c, exit_block);
    }

    if(else_block != exit_block)
    {
        append_old_basic_block(c, else_block);
        emit_stmt(c, if_stmt->else_stmt);
        emit_br(c, exit_block);
    }
    append_old_basic_block(c, exit_block);
}

static void emit_swap(CodegenContext* c, ASTStmt* stmt)
{
    Type* ty = stmt->stmt.swap.left->type;
    GenValue left = emit_expr(c, stmt->stmt.swap.left);
    GenValue right = emit_expr(c, stmt->stmt.swap.right);
    if(type_is_trivially_copyable(ty))
    {
        LLVMValueRef lrval = LLVMBuildLoad2(c->builder, get_llvm_type(c, ty), left.value, "");
        LLVMValueRef rrval = LLVMBuildLoad2(c->builder, get_llvm_type(c, ty), right.value, "");
        LLVMBuildStore(c->builder, rrval, left.value);
        LLVMBuildStore(c->builder, lrval, right.value);
        return;
    }

    SIC_ASSERT(c->swap_ref != NULL);
    uint32_t align = type_alignment(ty);
    LLVMValueRef size = LLVMConstInt(LLVMInt64Type(), type_size(ty), false);
    LLVMBuildMemCpy(c->builder, c->swap_ref, align, left.value,  align, size);
    LLVMBuildMemCpy(c->builder, left.value,  align, right.value, align, size);
    LLVMBuildMemCpy(c->builder, right.value, align, c->swap_ref, align, size);
}

static void emit_switch(CodegenContext* c, ASTStmt* stmt)
{
    ASTSwitch* swi = &stmt->stmt.switch_;
    GenValue expr = emit_expr(c, swi->expr);
    load_rvalue(c, &expr);
    LLVMBasicBlockRef exit_block = create_basic_block(c, ".switch_exit");
    LLVMBasicBlockRef default_block = exit_block;
    LLVMBasicBlockRef orig_block = c->cur_bb;
    LLVMBasicBlockRef last_block = exit_block;
    for(uint32_t i = swi->cases.size - 1; i < swi->cases.size; --i)
    {
        ASTCase* cas = swi->cases.data + i;
        if(cas->body == NULL)
            cas->llvm_block_ref = last_block;
        else
        {
            cas->llvm_block_ref = create_basic_block(c, ".switch_case");
            last_block = cas->llvm_block_ref;
        }
        if(cas->expr == NULL)
            default_block = cas->llvm_block_ref;
    }
    LLVMPositionBuilderAtEnd(c->builder, orig_block);
    LLVMValueRef switch_val = LLVMBuildSwitch(c->builder, expr.value, default_block, swi->cases.size - (default_block != exit_block)); 
    for(uint32_t i = 0; i < swi->cases.size; ++i)
    {
        ASTCase* cas = swi->cases.data + i;
        if(cas->body != NULL)
        {
            append_old_basic_block(c, cas->llvm_block_ref);
            emit_block_stmt(c, cas->body);
            emit_br(c, exit_block);
        }
        if(cas->expr == NULL)
            continue;
        SIC_ASSERT(cas->expr->kind == EXPR_CONSTANT && type_is_integer(cas->expr->type));
        LLVMAddCase(switch_val, LLVMConstInt(get_llvm_type(c, cas->expr->type), cas->expr->expr.constant.val.i, false), cas->llvm_block_ref);
    }
    append_old_basic_block(c, exit_block);
}

static void emit_while(CodegenContext* c, ASTStmt* stmt)
{
    ASTWhile* while_stmt = &stmt->stmt.while_;

    LLVMBasicBlockRef cond_block = append_new_basic_block(c, ".while_cond");
    LLVMBasicBlockRef exit_block = create_basic_block(c, ".while_exit");
    LLVMBasicBlockRef body_block = cond_block;

    if(stmt_not_empty(while_stmt->body))
        body_block = create_basic_block(c, ".while_body");

    GenValue cond = emit_expr(c, while_stmt->cond);
    load_rvalue(c, &cond);
    LLVMBuildCondBr(c->builder, cond.value, body_block, exit_block);
    
    if(body_block != cond_block)
    {
        append_old_basic_block(c, body_block);
        emit_stmt(c, while_stmt->body);
        emit_br(c, cond_block);
    }
    
    append_old_basic_block(c, exit_block);
}

static GenValue emit_expr(CodegenContext* c, ASTExpr* expr)
{
    GenValue result;
    result.type = expr->type;
    switch(expr->kind)
    {
    case EXPR_ARRAY_ACCESS:
        emit_array_access(c, expr, &result);
        break;
    case EXPR_BINARY:
        emit_binary(c, expr, &result);
        break;
    case EXPR_CAST: {
        GenValue inner = emit_expr(c, expr->expr.cast.inner);
        emit_cast(c, expr, &inner, &result);
        break;
    }
    case EXPR_CONSTANT:
        emit_constant(c, expr, &result);
        break;
    case EXPR_DEFAULT:
        SIC_TODO();
    case EXPR_FUNC_CALL:
        emit_function_call(c, expr, &result);
        break;
    case EXPR_IDENT:
        emit_ident(expr, &result);
        break;
    case EXPR_MEMBER_ACCESS:
        emit_member_access(c, expr, &result);
        break;
    case EXPR_NOP:
        result.value = NULL;
        break;
    case EXPR_POSTFIX: {
        GenValue inner = emit_expr(c, expr->expr.unary.inner);
        emit_incdec(c, expr, &inner, &result, true);
        break;
    }
    case EXPR_TERNARY:
        emit_ternary(c, expr, &result);
        break;
    case EXPR_UNARY:
        emit_unary(c, expr, &result);
        break;
    case EXPR_INVALID:
    case EXPR_PRE_SEMANTIC_IDENT:
    case EXPR_UNRESOLVED_ARR:
    case EXPR_UNRESOLVED_DOT:
        SIC_UNREACHABLE();
    }
    return result;
}

static void emit_array_access(CodegenContext* c, ASTExpr* expr, GenValue* result)
{
    ASTExprAAccess* aa = &expr->expr.array_access;
    GenValue array = emit_expr(c, aa->array_expr);
    GenValue index = emit_expr(c, aa->index_expr);
    load_rvalue(c, &array);
    load_rvalue(c, &index);
    LLVMValueRef indices[2] = { NULL, index.value };
    Type* arr_type = aa->array_expr->type;
    // TODO: Fix this with proper struct that holds SSA values and their
    //       corresponding type. As of right now this is just a hack to get this to
    //       work.
    if(arr_type->kind == TYPE_SS_ARRAY)
    {
        indices[0] = LLVMConstInt(LLVMInt64Type(), 0, false);
        result->value = LLVMBuildGEP2(c->builder, get_llvm_type(c, arr_type), array.value, indices, 2, "");
    }
    else
    {
        result->value = LLVMBuildGEP2(c->builder, get_llvm_type(c, type_pointer_base(arr_type)), array.value, indices + 1, 1, "");
    }

    result->kind = GEN_VAL_ADDRESS;
}

static void emit_binary(CodegenContext* c, ASTExpr* expr, GenValue* result)
{
    ASTExprBinary* binary = &expr->expr.binary;
    GenValue lhs = emit_expr(c, binary->lhs);
    if(binary->kind == BINARY_LOG_AND || binary->kind == BINARY_LOG_OR)
    {
        load_rvalue(c, &lhs);
        emit_logical_andor(c, &lhs, binary->rhs, result, binary->kind == BINARY_LOG_OR);
        return;
    }
    GenValue rhs = emit_expr(c, binary->rhs);
    load_rvalue(c, &rhs);
    result->kind = GEN_VAL_RVALUE;

    switch(binary->kind)
    {
    case BINARY_ADD:
        load_rvalue(c, &lhs);
        result->value = LLVMBuildAdd(c->builder, lhs.value, rhs.value, "");
        return;
    case BINARY_SUB:
        load_rvalue(c, &lhs);
        result->value = LLVMBuildSub(c->builder, lhs.value, rhs.value, "");
        return;
    case BINARY_MUL:
        load_rvalue(c, &lhs);
        result->value = LLVMBuildMul(c->builder, lhs.value, rhs.value, "");
        return;
    case BINARY_DIV:
        load_rvalue(c, &lhs);
        if(type_is_signed(expr->type))
            result->value = LLVMBuildSDiv(c->builder, lhs.value, rhs.value, "");
        else if(type_is_unsigned(expr->type))
            result->value = LLVMBuildUDiv(c->builder, lhs.value, rhs.value, "");
        else
            result->value = LLVMBuildFDiv(c->builder, lhs.value, rhs.value, "");
        return;
    case BINARY_MOD:
        load_rvalue(c, &lhs);
        if(type_is_signed(expr->type))
            result->value = LLVMBuildSRem(c->builder, lhs.value, rhs.value, "");
        else if(type_is_unsigned(expr->type))
            result->value = LLVMBuildURem(c->builder, lhs.value, rhs.value, "");
        else
            result->value = LLVMBuildFRem(c->builder, lhs.value, rhs.value, "");
        return;
    case BINARY_EQ:
        load_rvalue(c, &lhs);
        result->value = type_is_float(binary->lhs->type) ?
                    LLVMBuildFCmp(c->builder, LLVMRealOEQ, lhs.value, rhs.value, "") :
                    LLVMBuildICmp(c->builder, LLVMIntEQ, lhs.value, rhs.value, "");                    
        return;
    case BINARY_NE:
        load_rvalue(c, &lhs);
        result->value = type_is_float(binary->lhs->type) ?
                    LLVMBuildFCmp(c->builder, LLVMRealONE, lhs.value, rhs.value, "") :
                    LLVMBuildICmp(c->builder, LLVMIntNE, lhs.value, rhs.value, "");                    
        return;
    case BINARY_LT:
        load_rvalue(c, &lhs);
        result->value = type_is_float(binary->lhs->type) ?
                    LLVMBuildFCmp(c->builder, LLVMRealOLT, lhs.value, rhs.value, "") :
                    LLVMBuildICmp(c->builder, type_is_signed(binary->lhs->type) ? LLVMIntSLT : LLVMIntULT, lhs.value, rhs.value, "");                    
        return;
    case BINARY_LE:
        load_rvalue(c, &lhs);
        result->value = type_is_float(binary->lhs->type) ?
                    LLVMBuildFCmp(c->builder, LLVMRealOLT, lhs.value, rhs.value, "") :
                    LLVMBuildICmp(c->builder, type_is_signed(binary->lhs->type) ? LLVMIntSLE : LLVMIntULE, lhs.value, rhs.value, "");                    
        return;
    case BINARY_GT:
        load_rvalue(c, &lhs);
        result->value = type_is_float(binary->lhs->type) ?
                    LLVMBuildFCmp(c->builder, LLVMRealOGT, lhs.value, rhs.value, "") :
                    LLVMBuildICmp(c->builder, type_is_signed(binary->lhs->type) ? LLVMIntSGT : LLVMIntUGT, lhs.value, rhs.value, "");                    
        return;
    case BINARY_GE:
        load_rvalue(c, &lhs);
        result->value = type_is_float(binary->lhs->type) ?
                    LLVMBuildFCmp(c->builder, LLVMRealOGE, lhs.value, rhs.value, "") :
                    LLVMBuildICmp(c->builder, type_is_signed(binary->lhs->type) ? LLVMIntSGE : LLVMIntUGE, lhs.value, rhs.value, "");                    
        return;
    case BINARY_SHL:
        load_rvalue(c, &lhs);
        result->value = LLVMBuildShl(c->builder, lhs.value, rhs.value, "");
        return;
    case BINARY_LSHR:
        load_rvalue(c, &lhs);
        result->value = LLVMBuildLShr(c->builder, lhs.value, rhs.value, "");
        return;
    case BINARY_ASHR:
        load_rvalue(c, &lhs);
        result->value = LLVMBuildAShr(c->builder, lhs.value, rhs.value, "");
        return;
    case BINARY_BIT_OR:
        load_rvalue(c, &lhs);
        result->value = LLVMBuildOr(c->builder, lhs.value, rhs.value, "");
        return;
    case BINARY_BIT_XOR:
        load_rvalue(c, &lhs);
        result->value = LLVMBuildXor(c->builder, lhs.value, rhs.value, "");
        return;
    case BINARY_BIT_AND:
        load_rvalue(c, &lhs);
        result->value = LLVMBuildAnd(c->builder, lhs.value, rhs.value, "");
        return;
    case BINARY_ASSIGN:
        emit_assign(c, &lhs, &rhs, result);
        return;
    case BINARY_INVALID:
    case BINARY_ADD_ASSIGN:
    case BINARY_SUB_ASSIGN:
    case BINARY_MUL_ASSIGN:
    case BINARY_DIV_ASSIGN:
    case BINARY_MOD_ASSIGN:
    case BINARY_BIT_OR_ASSIGN:
    case BINARY_BIT_XOR_ASSIGN:
    case BINARY_BIT_AND_ASSIGN:
    case BINARY_SHL_ASSIGN:
    case BINARY_LSHR_ASSIGN:
    case BINARY_ASHR_ASSIGN:
    case BINARY_LOG_OR:
    case BINARY_LOG_AND:
        break;
    }
    SIC_UNREACHABLE();
}

static void emit_cast(CodegenContext* c, ASTExpr* expr, GenValue* inner, GenValue* result)
{
    ASTExprCast* cast = &expr->expr.cast;
    load_rvalue(c, inner);
    result->kind = GEN_VAL_RVALUE;
    LLVMTypeRef to_llvm = get_llvm_type(c, expr->type);
    switch(cast->kind)
    {
    case CAST_FLOAT_TO_SINT:
        result->value = LLVMBuildFPToSI(c->builder, inner->value, to_llvm, "");
        return;
    case CAST_FLOAT_TO_UINT:
        result->value = LLVMBuildFPToUI(c->builder, inner->value, to_llvm, "");
        return;
    case CAST_SINT_TO_FLOAT:
        result->value = LLVMBuildSIToFP(c->builder, inner->value, to_llvm, "");
        return;
    case CAST_UINT_TO_FLOAT:
        result->value = LLVMBuildUIToFP(c->builder, inner->value, to_llvm, "");
        return;
    case CAST_INT_TO_BOOL:
        result->value = LLVMBuildIsNotNull(c->builder, inner->value, "");
        return;
    case CAST_PTR_TO_INT:
        result->value = LLVMBuildPtrToInt(c->builder, inner->value, LLVMInt64Type(), "");
        return;
    case CAST_INT_TO_PTR:
        SIC_TODO();
    case CAST_FLOAT_EXT_TRUNC: {
        bool is_widen = type_size(expr->type) > type_size(cast->inner->type);
        result->value = is_widen ? LLVMBuildFPExt(c->builder, inner->value, to_llvm, "") :
                                   LLVMBuildFPTrunc(c->builder, inner->value, to_llvm, "");
        return;
    }
    case CAST_SINT_EXT_TRUNC: {
        bool is_widen = type_size(expr->type) > type_size(cast->inner->type);
        result->value = is_widen ? LLVMBuildSExt(c->builder, inner->value, to_llvm, "") :
                                   LLVMBuildTrunc(c->builder, inner->value, to_llvm, "");
        return;
    }
    case CAST_UINT_EXT_TRUNC: {
        bool is_widen = type_size(expr->type) > type_size(cast->inner->type);
        result->value = is_widen ? LLVMBuildZExt(c->builder, inner->value, to_llvm, "") :
                                   LLVMBuildTrunc(c->builder, inner->value, to_llvm, "");
        return;
    }
    case CAST_REINTERPRET:
        result->value = inner->value;
        return;
    case CAST_INVALID:
        break;
    }
    SIC_UNREACHABLE();
}

static void emit_constant(CodegenContext* c, ASTExpr* expr, GenValue* result)
{
    ASTExprConstant* constant = &expr->expr.constant;
    result->kind = GEN_VAL_RVALUE;
    switch(constant->kind)
    {
    case CONSTANT_INTEGER:
        result->value = LLVMConstInt(get_llvm_type(c, expr->type), constant->val.i, false);
        return;
    case CONSTANT_BOOL:
        SIC_ASSERT(constant->val.i <= 1);
        result->value = LLVMConstInt(LLVMInt1Type(), constant->val.i, false);
        return;
    case CONSTANT_FLOAT:
        result->value = LLVMConstReal(get_llvm_type(c, expr->type), constant->val.f);
        return;
    case CONSTANT_STRING: {
        LLVMValueRef str = LLVMConstString(expr->expr.constant.val.s, strlen(expr->expr.constant.val.s), false);
        LLVMValueRef global_string = LLVMAddGlobal(c->module_ref, LLVMTypeOf(str), ".str");
        LLVMSetGlobalConstant(global_string, true);
        LLVMSetLinkage(global_string, LLVMPrivateLinkage);
        LLVMSetUnnamedAddress(global_string, LLVMGlobalUnnamedAddr);
        LLVMSetInitializer(global_string, str);
        result->value = global_string;
        return;
    }
    case CONSTANT_POINTER:
        result->value = expr->expr.constant.val.i == 0 ? LLVMConstPointerNull(c->ptr_type) :
                                                         LLVMConstPointerCast(LLVMConstInt(LLVMInt64Type(), constant->val.i, false), c->ptr_type);
        return;
    case CONSTANT_INVALID:
        break;
    }
    SIC_UNREACHABLE();
}

static void emit_function_call(CodegenContext* c, ASTExpr* expr, GenValue* result)
{
    // TODO: Remove temporary
    ASTExprCall* call = &expr->expr.call;
    Object* function = call->func_expr->expr.ident;
    SIC_ASSERT(call->func_expr->kind == EXPR_IDENT);
    SIC_ASSERT(function->llvm_ref != NULL);

    LLVMValueRef* args = NULL;
    if(call->args.size > 0)
        args = MALLOC_STRUCTS(LLVMValueRef, call->args.size);
    for(size_t i = 0; i < call->args.size; ++i)
    {
        GenValue temp = emit_expr(c, call->args.data[i]);
        load_rvalue(c, &temp);
        args[i] = temp.value;
    }

    result->value = LLVMBuildCall2(c->builder, function->func.signature->llvm_func_type, function->llvm_ref, args, call->args.size, "");
    result->kind = GEN_VAL_RVALUE;
}

static void emit_ident(ASTExpr* expr, GenValue* result)
{
    Object* obj = expr->expr.ident;
    SIC_ASSERT(obj->llvm_ref != NULL);
    result->value = obj->llvm_ref;
    result->kind = obj->kind == OBJ_FUNC ? GEN_VAL_RVALUE : GEN_VAL_ADDRESS;
}

static void emit_incdec(CodegenContext* c, ASTExpr* expr, GenValue* inner, GenValue* result, bool is_post)
{
    (void)expr;
    GenValue inner_rval = *inner;
    load_rvalue(c, &inner_rval);
    // TODO: Change this 
    GenValue temp;
    temp.value = LLVMConstInt(get_llvm_type(c, inner->type), 1, false);
    emit_add(c, &inner_rval, &temp, result);
    LLVMBuildStore(c->builder, result->value, inner->value);
    result->kind = GEN_VAL_RVALUE;
    if(is_post)
        result->value = inner_rval.value;
}

static void emit_member_access(CodegenContext* c, ASTExpr* expr, GenValue* result)
{
    ASTExprMAccess* maccess = &expr->expr.member_access;
    GenValue parent = emit_expr(c, maccess->parent_expr);
    uint32_t index = maccess->parent_expr->type->kind == TYPE_UNION ? 0 : maccess->member->var.member_idx;
    result->value = LLVMBuildStructGEP2(c->builder, get_llvm_type(c, maccess->parent_expr->type), parent.value, index, "");
    result->kind = GEN_VAL_ADDRESS;
}

static void emit_logical_andor(CodegenContext* c, GenValue* lhs, ASTExpr* rhs, GenValue* result, bool is_or)
{
    LLVMBasicBlockRef rhs_bb = LLVMAppendBasicBlock(c->cur_func->llvm_ref, ".log_rhs");
    LLVMBasicBlockRef blocks[2] = { c->cur_bb, rhs_bb };
    LLVMBasicBlockRef exit_bb = LLVMAppendBasicBlock(c->cur_func->llvm_ref, ".log_exit");
    LLVMBasicBlockRef true_bb;
    LLVMBasicBlockRef false_bb;
    int exit_early_val;

    if(is_or)
    {
        true_bb = exit_bb;
        false_bb = rhs_bb;
        exit_early_val = 1;
    }
    else
    {
        true_bb = rhs_bb;
        false_bb = exit_bb;
        exit_early_val = 0;
    }

    lhs->value = LLVMBuildTrunc(c->builder, lhs->value, LLVMInt1Type(), "");
    LLVMBuildCondBr(c->builder, lhs->value, true_bb, false_bb);
    LLVMPositionBuilderAtEnd(c->builder, rhs_bb);
    c->cur_bb = rhs_bb;
    GenValue rhs_val = emit_expr(c, rhs);
    load_rvalue(c, &rhs_val);
    rhs_val.value = LLVMBuildTrunc(c->builder, rhs_val.value, LLVMInt1Type(), "");
    emit_br(c, exit_bb);
    LLVMPositionBuilderAtEnd(c->builder, exit_bb);
    c->cur_bb = exit_bb;
    result->value = LLVMBuildPhi(c->builder, LLVMInt1Type(), "");

    LLVMValueRef values[2] = { LLVMConstInt(LLVMInt1Type(), exit_early_val, false), rhs_val.value };
    result->type = g_type_bool;
    LLVMAddIncoming(result->value, values, blocks, 2);
}

static void emit_ternary(CodegenContext* c, ASTExpr* expr, GenValue* result)
{
    ASTExprTernary* ternary = &expr->expr.ternary;
    GenValue cond;
    GenValue then;
    GenValue elss;
    LLVMBasicBlockRef then_bb = c->cur_bb;
    LLVMBasicBlockRef else_bb = create_basic_block(c, ".cond_else");
    LLVMBasicBlockRef exit_bb = create_basic_block(c, ".cond_exit");
    if(ternary->then_expr == NULL)
    {
        if(expr->type->kind == TYPE_BOOL)
        {
            then = emit_expr(c, ternary->cond_expr);
            load_rvalue(c, &then);
            cond = then;
        }
        else if(ternary->cond_expr->kind == EXPR_CAST)
        {
            then = emit_expr(c, ternary->cond_expr->expr.cast.inner);
            load_rvalue(c, &then);
            emit_cast(c, ternary->cond_expr, &then, &cond);
        }
        else
            SIC_UNREACHABLE();
        load_rvalue(c, &cond);
        LLVMBuildCondBr(c->builder, cond.value, exit_bb, else_bb);
    }
    else
    {
        cond = emit_expr(c, ternary->cond_expr);
        load_rvalue(c, &cond);
        then_bb = create_basic_block(c, ".cond_then");
        LLVMBuildCondBr(c->builder, cond.value, then_bb, else_bb);
        append_old_basic_block(c, then_bb);
        then = emit_expr(c, ternary->then_expr);
        load_rvalue(c, &then);
    }
    append_old_basic_block(c, else_bb);
    elss = emit_expr(c, ternary->else_expr);
    load_rvalue(c, &elss);

    emit_br(c, exit_bb);
    append_old_basic_block(c, exit_bb);
    result->value = LLVMBuildPhi(c->builder, get_llvm_type(c, result->type), "");
    result->kind = GEN_VAL_RVALUE;

    LLVMValueRef values[2] = { then.value, elss.value };
    LLVMBasicBlockRef blocks[2] = { then_bb, else_bb };
    LLVMAddIncoming(result->value, values, blocks, 2);

}

static void emit_unary(CodegenContext* c, ASTExpr* expr, GenValue* result)
{
    ASTExprUnary* unary = &expr->expr.unary;
    // TODO: Make emit_expr always emit l-value, then make a function that
    //       gets the r-value of an l-value for certain emissions.
    GenValue inner = emit_expr(c, unary->inner);
    switch(unary->kind)
    {
    case UNARY_ADDR_OF:
        result->kind = GEN_VAL_RVALUE;
        result->value = inner.value;
        return;
    case UNARY_DEC:
        SIC_TODO();
    case UNARY_DEREF:
        load_rvalue(c, &inner);
        result->kind = GEN_VAL_ADDRESS;
        result->value = inner.value;
        return;
    case UNARY_INC:
        emit_incdec(c, expr, &inner, result, false);
        result->kind = GEN_VAL_RVALUE;
        return;
    case UNARY_BIT_NOT:
    case UNARY_LOG_NOT:
        load_rvalue(c, &inner);
        result->kind = GEN_VAL_RVALUE;
        result->value = LLVMBuildNot(c->builder, inner.value, "");
        return;
    case UNARY_NEG:
        load_rvalue(c, &inner);
        result->kind = GEN_VAL_RVALUE;
        result->value = type_is_float(inner.type) ? LLVMBuildFNeg(c->builder, inner.value, "") :
                                                    LLVMBuildNeg(c->builder, inner.value, "");
        return;
    case UNARY_INVALID:
        break;
    }
    SIC_UNREACHABLE();
}

static void emit_add(CodegenContext* c, GenValue* left, GenValue* right, GenValue* result)
{
    if(type_is_pointer(left->type))
        result->value = LLVMBuildGEP2(c->builder, get_llvm_type(c, left->type->pointer_base), left->value, &right->value, 1, "");
    else if(type_is_float(left->type))
        result->value = LLVMBuildFAdd(c->builder, left->value, right->value, "");
    else
        result->value = LLVMBuildAdd(c->builder, left->value, right->value, "");
}

static void emit_assign(CodegenContext* c, GenValue* ptr, GenValue* value, GenValue* result)
{
    result->value = value->value;
    if(type_is_array(value->type) || type_is_user_def(value->type))
    {
        LLVMBuildMemCpy(c->builder, 
                        ptr->value, type_alignment(ptr->type), 
                        value->value, type_alignment(value->type), 
                        LLVMConstInt(LLVMInt64Type(), type_size(value->type), false));
    }
    else
        LLVMBuildStore(c->builder, value->value, ptr->value);
}

static void emit_br(CodegenContext* c, LLVMBasicBlockRef block)
{
    if(c->cur_bb)
        LLVMBuildBr(c->builder, block);
}

static void emit_var_alloca(CodegenContext* c, Object* obj)
{
    SIC_ASSERT(obj->kind == OBJ_VAR);
    get_llvm_type(c, obj->var.type);
    scratch_clear();
    scratch_append(obj->symbol);
    if(obj->var.type->kind == TYPE_DS_ARRAY)
    {
        GenValue size_val = emit_expr(c, obj->var.type->array.size_expr);
        load_rvalue(c, &size_val);
        obj->llvm_ref = LLVMBuildArrayAlloca(c->builder, 
                                             get_llvm_type(c, obj->var.type), 
                                             size_val.value,
                                             scratch_string());
        return;
    }
    obj->llvm_ref = emit_alloca(c, scratch_string(), get_llvm_type(c, obj->var.type), type_alignment(obj->var.type));
}

static LLVMValueRef emit_alloca(CodegenContext* c, const char* name, LLVMTypeRef type, uint32_t align)
{
    LLVMBasicBlockRef bb = c->cur_bb;
    LLVMPositionBuilderBefore(c->builder, c->alloca_ref);
    LLVMValueRef new_alloca = LLVMBuildAlloca(c->builder, type, name);
    LLVMSetAlignment(new_alloca, align);
    LLVMPositionBuilderAtEnd(c->builder, bb);
    return new_alloca;
}

static LLVMBasicBlockRef append_new_basic_block(CodegenContext* c, const char* label)
{
    c->cur_bb = LLVMAppendBasicBlock(c->cur_func->llvm_ref, label);
    emit_br(c, c->cur_bb);
    LLVMPositionBuilderAtEnd(c->builder, c->cur_bb);
    return c->cur_bb;
}

static void append_old_basic_block(CodegenContext* c, LLVMBasicBlockRef block)
{
    LLVMAppendExistingBasicBlock(c->cur_func->llvm_ref, block);
    use_basic_block(c, block);
}

static LLVMBasicBlockRef create_basic_block(CodegenContext* c, const char* label)
{
    return LLVMCreateBasicBlockInContext(c->global_context, label);
}

static void use_basic_block(CodegenContext* c, LLVMBasicBlockRef block)
{
    c->cur_bb = block;
    LLVMPositionBuilderAtEnd(c->builder, c->cur_bb);
}

static void emit_llvm_ir(CodegenContext* c)
{
    char* error;
    if(LLVMVerifyModule(c->module_ref, LLVMPrintMessageAction, &error))
    {
        if(error != NULL && *error != '\0')
            error = "No error supplied.";
        sic_fatal_error("Failed to verify LLVM-IR module: %s", error);
    }
    if(LLVMPrintModuleToFile(c->module_ref, c->llvm_filename, &error))
        sic_fatal_error("Failed to emit LLVM IR: \'%s\'", error);
}

static void emit_file(CodegenContext* c, const char* out_path, LLVMCodeGenFileType llvm_file_type)
{
    char* error;
    if(LLVMTargetMachineEmitToFile(c->target_machine, c->module_ref, out_path, llvm_file_type, &error))
        sic_fatal_error("LLVM failed to emit file \'%s\': %s", out_path, error);
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
    case TYPE_UBYTE:
    case TYPE_USHORT:
    case TYPE_UINT:
    case TYPE_ULONG:
    case TYPE_BYTE:
    case TYPE_SHORT:
    case TYPE_INT:
    case TYPE_LONG:
        return type->llvm_ref = LLVMIntType(type->builtin.size * 8);
    case TYPE_FLOAT:
        return type->llvm_ref = LLVMFloatType();
    case TYPE_DOUBLE:
        return type->llvm_ref = LLVMDoubleType();
    case TYPE_POINTER:
        return type->llvm_ref = c->ptr_type;
    case TYPE_SS_ARRAY:
        return type->llvm_ref = LLVMArrayType(get_llvm_type(c, type->array.elem_type), type->array.ss_size);
    case TYPE_DS_ARRAY:
        return type->llvm_ref = get_llvm_type(c, type->array.elem_type);
    case TYPE_ENUM:
    case TYPE_STRUCT: {
        Object* user = type->user_def;
        if(user->llvm_ref)
            return type->llvm_ref = user->llvm_ref;
        LLVMTypeRef* element_types = MALLOC_STRUCTS(LLVMTypeRef, user->struct_.members.size);
        for(size_t i = 0; i < user->struct_.members.size; ++i)
            element_types[i] = get_llvm_type(c, user->struct_.members.data[i]->var.type);
        scratch_clear();
        scratch_append("struct.");
        scratch_append(user->symbol);
        user->llvm_ref = LLVMStructCreateNamed(c->global_context, scratch_string());
        LLVMStructSetBody(user->llvm_ref, element_types, user->struct_.members.size, false);
        return type->llvm_ref = user->llvm_ref;
    }
    case TYPE_TYPEDEF:
        SIC_TODO();
    case TYPE_UNION: {
        Object* user = type->user_def;
        if(user->llvm_ref)
            return type->llvm_ref = user->llvm_ref;
        LLVMTypeRef largest_ref = get_llvm_type(c, user->struct_.largest_type);
        scratch_clear();
        scratch_append("union.");
        scratch_append(user->symbol);
        user->llvm_ref = LLVMStructCreateNamed(c->global_context, scratch_string());
        LLVMStructSetBody(user->llvm_ref, &largest_ref, 1, false);
        return type->llvm_ref = user->llvm_ref;
    }
    case TYPE_INVALID:
    case TYPE_NULLPTR:
    case TYPE_PRE_SEMA_ARRAY:
    case __TYPE_COUNT:
        break;
    }
    SIC_UNREACHABLE();
}

static void load_rvalue(CodegenContext* c, GenValue* lvalue)
{
    if(lvalue->kind == GEN_VAL_RVALUE)
        return;
    lvalue->kind = GEN_VAL_RVALUE;
    switch(lvalue->type->kind)
    {
    case TYPE_BOOL:
    case TYPE_UBYTE:
    case TYPE_USHORT:
    case TYPE_UINT:
    case TYPE_ULONG:
    case TYPE_BYTE:
    case TYPE_SHORT:
    case TYPE_INT:
    case TYPE_LONG:
    case TYPE_FLOAT:
    case TYPE_DOUBLE:
    case TYPE_POINTER:
    case TYPE_STRUCT:
    case TYPE_UNION:
        lvalue->value = LLVMBuildLoad2(c->builder, get_llvm_type(c, lvalue->type), lvalue->value, "");
        return;
    case TYPE_SS_ARRAY:
    case TYPE_DS_ARRAY:
        return;
    case TYPE_INVALID:
    case TYPE_VOID:
    case TYPE_NULLPTR:
    case TYPE_ENUM:
    case TYPE_TYPEDEF:
    case TYPE_PRE_SEMA_ARRAY:
    case __TYPE_COUNT:
        break;
    }
    SIC_UNREACHABLE();
}

static LLVMTargetRef get_llvm_target(const char* triple)
{
    char* error;
    LLVMTargetRef ref;
    if(LLVMGetTargetFromTriple(triple, &ref, &error))
        sic_fatal_error("Failed to get target from triple \'%s\': %s", triple, error);
    return ref;
}

static void llvm_diag_handler(LLVMDiagnosticInfoRef ref, UNUSED void *context)
{
	char *desc = LLVMGetDiagInfoDescription(ref);
	const char *severity;
	switch (LLVMGetDiagInfoSeverity(ref))
	{
		case LLVMDSError:
			sic_fatal_error("LLVM Error: %s", desc);
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
            LLVMDisposeMessage(desc);
			return;
	}

#ifdef SI_DEBUG
    if(g_args.emit_debug_output)
        printf("[DEBUG] %s\033[0m: %s\n", severity, desc);
#endif
	LLVMDisposeMessage(desc);
}
