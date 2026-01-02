#include "codegen-internal.h"

#include <llvm-c/Analysis.h>
#include <llvm-c/Core.h>
#include <llvm-c/Target.h>
#include <llvm-c/TargetMachine.h>

typedef struct CodegenContext CodegenContext;
typedef struct GenValue       GenValue;
struct CodegenContext
{
    const char*          llvm_filename;
    const char*          asm_filename;
    const char*          obj_filename;
    ObjFunc*             cur_func;


    LLVMTargetMachineRef target_machine;
    LLVMModuleRef        llvm_module;
    LLVMBuilderRef       builder;
    LLVMValueRef         alloca_ref;
    LLVMValueRef         swap_ref;
    LLVMBasicBlockRef    cur_bb;
    LLVMBasicBlockRef    break_bb;
    LLVMBasicBlockRef    continue_bb;

    LLVMTypeRef          ptr_type;
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


static void     gen_source_file(CodegenContext* c, SourceFile* file);
static void     gen_module(CodegenContext* c, ObjModule* module);
static void     emit_llvm_ir(CodegenContext* c);
static void     emit_file(CodegenContext* c, const char* out_path, LLVMCodeGenFileType llvm_file_type);
static void     emit_global_var(CodegenContext* c, ObjVar* global);
static void     emit_function_body(CodegenContext* c, ObjFunc* func);
static void     emit_stmt(CodegenContext* c, ASTStmt* stmt);
static void     emit_block_stmt(CodegenContext* c, ASTStmt* stmt);
static void     emit_declaration(CodegenContext* c, ObjVar* decl);
static void     emit_for(CodegenContext* c, ASTStmt* stmt);
static void     emit_if(CodegenContext* c, ASTStmt* stmt);
static void     emit_swap(CodegenContext* c, ASTStmt* stmt);
static void     emit_switch(CodegenContext* c, ASTStmt* stmt);
static void     emit_while(CodegenContext* c, ASTStmt* stmt);
static GenValue emit_expr(CodegenContext* c, ASTExpr* expr);
static void     emit_array_access(CodegenContext* c, ASTExpr* expr, GenValue* result);
static void     emit_array_initialization(CodegenContext* c, GenValue* lhs, ASTExpr* expr, GenValue* result);
static void     emit_binary(CodegenContext* c, ASTExpr* expr, GenValue* result);
static void     emit_call(CodegenContext* c, ASTExpr* expr, GenValue* result);
static void     emit_cast(CodegenContext* c, ASTExpr* expr, GenValue* inner, GenValue* result);
static void     emit_constant(CodegenContext* c, ASTExpr* expr, GenValue* result);
static LLVMValueRef emit_const_initializer(CodegenContext* c, ASTExpr* expr);
static LLVMValueRef emit_const_array_init_list(CodegenContext* c, ASTExpr* expr);
static void     emit_ident(CodegenContext* c, ASTExpr* expr, GenValue* result);
static void     emit_incdec(CodegenContext* c, ASTExpr* expr, GenValue* inner, GenValue* result, bool is_post);
static void     emit_member_access(CodegenContext* c, ASTExpr* expr, GenValue* result);
static void     emit_logical_andor(CodegenContext* c, GenValue* lhs, ASTExpr* rhs, GenValue* result, bool is_or);
static void     emit_ternary(CodegenContext* c, ASTExpr* expr, GenValue* result);
static void     emit_unary(CodegenContext* c, ASTExpr* expr, GenValue* result);

static void     emit_add(CodegenContext* c, GenValue* left, GenValue* right, GenValue* result);
static void     emit_assign(CodegenContext* c, GenValue* lhs, ASTExpr* rhs, GenValue* result);
static void     emit_br(CodegenContext* c, LLVMBasicBlockRef block);
static void     emit_sub(CodegenContext* c, GenValue* left, GenValue* right, GenValue* result);

static LLVMValueRef      emit_alloca(CodegenContext* c, const char* name, LLVMTypeRef type, uint32_t align);
static void              emit_var_alloca(CodegenContext* c, ObjVar* obj);
static LLVMBasicBlockRef create_basic_block(const char* label);
static LLVMBasicBlockRef append_new_basic_block(CodegenContext* c, const char* label);
static void              append_old_basic_block(CodegenContext* c, LLVMBasicBlockRef block);
static void              use_basic_block(CodegenContext* c, LLVMBasicBlockRef block);
static LLVMTypeRef       get_llvm_type(CodegenContext* c, Type* type);
static LLVMTypeRef       get_llvm_func_type(CodegenContext* c, Type* type);
static LLVMValueRef      get_llvm_ref(CodegenContext* c, Object* obj);
static LLVMTargetRef     get_llvm_target(const char* triple);
static void              load_rvalue(CodegenContext* c, GenValue* lvalue);
static void              llvm_diag_handler(LLVMDiagnosticInfoRef ref, void *context);

static bool stmt_empty(ASTStmt* stmt)
{
    return (stmt == NULL) ||
           (stmt->kind == STMT_NOP && stmt->next == NULL) ||
           (stmt->kind == STMT_BLOCK && stmt_empty(stmt->stmt.block.body));
}

static bool s_initialized = false;
static LLVMContextRef s_context;

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
    s_context = LLVMGetGlobalContext();
}

void llvm_codegen()
{
    SIC_ASSERT(s_initialized);
    SIC_ASSERT(g_compiler.sources.size > 0);

    CodegenContext c = {0};
    c.ptr_type = LLVMPointerType(LLVMInt8Type(), 0);
    c.builder = LLVMCreateBuilder();

    for(uint32_t i = 0; i < g_compiler.sources.size; ++i)
        gen_source_file(&c, g_compiler.sources.data + i);

    LLVMDisposeBuilder(c.builder);
}

static void gen_source_file(CodegenContext* c, SourceFile* file)
{
    // TODO: Change this from being hardcoded.
    const char* target_triple = "x86_64-pc-linux-gnu";
    LLVMTargetRef target = get_llvm_target(target_triple);
    // No optimization for now, this will be changed later along with properly setting up
    // target detection.
    c->target_machine = LLVMCreateTargetMachine(target, target_triple, "generic", "", LLVMCodeGenLevelNone, LLVMRelocPIC, LLVMCodeModelDefault);
    if(c->target_machine == NULL)
        sic_fatal_error("LLVM failed to create target machine, maybe check target triple?");

    c->llvm_module = LLVMModuleCreateWithNameInContext(file->rel_path, s_context);
    LLVMSetSourceFileName(c->llvm_module, file->rel_path, strlen(file->rel_path));
    LLVMSetModuleDataLayout(c->llvm_module, LLVMCreateTargetDataLayout(c->target_machine));
    LLVMSetTarget(c->llvm_module, target_triple);
    gen_module(c, file->module);

    if(g_compiler.emit_ir)
    {
        c->llvm_filename = convert_ext_to(file->rel_path, FT_LLVM_IR);
        emit_llvm_ir(c);
    }

    if(g_compiler.emit_asm)
    {
        c->asm_filename = convert_ext_to(file->rel_path, FT_ASM);
        emit_file(c, c->asm_filename, LLVMAssemblyFile);
    }

    // c->obj_filename = g_compiler.emit_obj ? 
    //                     convert_ext_to(file->rel_path, FT_OBJ) :
    //                     create_tempfile(FT_OBJ);

    // TODO: Uncomment the above code and add proper incremental build support.
    c->obj_filename = create_tempfile(FT_OBJ);

    emit_file(c, c->obj_filename, LLVMObjectFile);
    da_append(&g_compiler.linker_inputs, c->obj_filename);
    LLVMDisposeTargetMachine(c->target_machine);
    LLVMDisposeModule(c->llvm_module);

}

static void gen_module(CodegenContext* c, ObjModule* module)
{
    for(uint32_t i = 0; i < module->funcs.size; ++i)
        get_llvm_ref(c, &module->funcs.data[i]->header);

    for(uint32_t i = 0; i < module->vars.size; ++i)
        emit_global_var(c, module->vars.data[i]);

    for(uint32_t i = 0; i < module->funcs.size; ++i)
    {
        c->cur_bb = NULL;
        emit_function_body(c, module->funcs.data[i]);
    }
    
    for(uint32_t i = 0; i < module->submodules.size; ++i)
    {
        ObjModule* mod = module->submodules.data[i];
        if(mod->is_inline)
            gen_module(c, mod);
    }
}

static void emit_global_var(CodegenContext* c, ObjVar* global)
{
    if(global->kind == VAR_CONST) return;
    get_llvm_ref(c, &global->header);

    if(global->initial_val == NULL)
    {
        LLVMSetInitializer(global->header.llvm_ref, LLVMConstNull(get_llvm_type(c, global->type_loc.type)));
        return;
    }

    LLVMSetInitializer(global->header.llvm_ref, emit_const_initializer(c, global->initial_val));
}

static void emit_function_body(CodegenContext* c, ObjFunc* func)
{
    SIC_ASSERT(func->header.llvm_ref != NULL);
    if(func->body == NULL)
        return;
    c->cur_func = func;
    append_old_basic_block(c, create_basic_block(""));
    c->alloca_ref = LLVMBuildAlloca(c->builder, LLVMInt32Type(), ".alloca_ptr");
    if(func->swap_stmt_size > 0)
    {
        c->swap_ref = emit_alloca(c, ".swap_space", 
                                  LLVMArrayType2(LLVMInt8Type(), func->swap_stmt_size), 
                                  func->swap_stmt_align);
    }

    const ObjVarDA params = func->signature.params;

    for(uint32_t i = 0; i < params.size; ++i)
    {
        ObjVar* param = params.data[i];
        if(param->header.symbol)
        {
            emit_var_alloca(c, param);
            LLVMBuildStore(c->builder, LLVMGetParam(func->header.llvm_ref, i), param->header.llvm_ref);
        }
    }

    emit_block_stmt(c, func->body);

    if(c->cur_bb != NULL)
    {
        if(func->signature.ret_type.type->kind == TYPE_VOID)
            LLVMBuildRetVoid(c->builder);
        else
            LLVMRemoveBasicBlockFromParent(c->cur_bb);
    }

    if(LLVMGetInstructionParent(c->alloca_ref))
    {
        LLVMInstructionEraseFromParent(c->alloca_ref);
        c->alloca_ref = NULL;
    }

    if(LLVMVerifyFunction(func->header.llvm_ref, LLVMPrintMessageAction))
    {
        LLVMDumpModule(c->llvm_module);
        putc('\n', stderr);
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
        SIC_ASSERT(c->break_bb != NULL);
        emit_br(c, c->break_bb);
        return;
    case STMT_CONTINUE:
        SIC_ASSERT(c->continue_bb != NULL);
        emit_br(c, c->continue_bb);
        return;
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
    case STMT_MULTI_DECL: {
        const ObjVarDA decls = stmt->stmt.multi_decl;
        for(uint32_t i = 0; i < decls.size; ++i)
            emit_declaration(c, decls.data[i]); 
        return;
    }
    case STMT_NOP:
        return;
    case STMT_RETURN:
        if(c->cur_func->signature.ret_type.type->kind == TYPE_VOID)
            LLVMBuildRetVoid(c->builder);
        else
        {
            GenValue ret_expr = emit_expr(c, stmt->stmt.return_.ret_expr);
            load_rvalue(c, &ret_expr);
            LLVMBuildRet(c->builder, ret_expr.value);
        }
        c->cur_bb = NULL;
        return;
    case STMT_SINGLE_DECL:
        emit_declaration(c, stmt->stmt.single_decl);
        return;
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

static void emit_declaration(CodegenContext* c, ObjVar* decl)
{
    SIC_ASSERT(decl->kind == VAR_LOCAL);
    emit_var_alloca(c, decl);
    GenValue var;
    var.type = decl->type_loc.type;
    var.value = decl->header.llvm_ref;
    var.kind = GEN_VAL_ADDRESS;
    if(decl->initial_val != NULL)
    {
        GenValue temp;
        emit_assign(c, &var, decl->initial_val, &temp);
    }
}

static void emit_for(CodegenContext* c, ASTStmt* stmt)
{
    ASTFor* for_stmt = &stmt->stmt.for_;

    LLVMBasicBlockRef exit_block = create_basic_block(".for_exit");
    LLVMBasicBlockRef body_block = create_basic_block(".for_body");
    LLVMBasicBlockRef cond_block = body_block;
    LLVMBasicBlockRef loop_block;
    if(!stmt_empty(for_stmt->init_stmt))
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

    loop_block = for_stmt->loop_expr == NULL ? cond_block : create_basic_block(".for_loop");
    
    LLVMBasicBlockRef prev_break = c->break_bb;
    LLVMBasicBlockRef prev_cont = c->continue_bb;
    c->break_bb = exit_block;
    c->continue_bb = loop_block;
    append_old_basic_block(c, body_block);
    emit_stmt(c, for_stmt->body);
    emit_br(c, loop_block);
    c->break_bb = prev_break;
    c->continue_bb = prev_cont;
    
    if(for_stmt->loop_expr != NULL)
    {
        append_old_basic_block(c, loop_block); 
        emit_expr(c, for_stmt->loop_expr);
    }

    emit_br(c, cond_block);
    append_old_basic_block(c, exit_block);
}

static void emit_if(CodegenContext* c, ASTStmt* stmt)
{
    ASTIf* if_stmt = &stmt->stmt.if_;

    LLVMBasicBlockRef exit_block = create_basic_block(".if_exit");
    LLVMBasicBlockRef then_block = exit_block;
    LLVMBasicBlockRef else_block = exit_block;

    GenValue cond = emit_expr(c, if_stmt->cond);

    load_rvalue(c, &cond);
    if(!stmt_empty(if_stmt->then_stmt))
        then_block = create_basic_block(".if_then");
    
    if(!stmt_empty(if_stmt->else_stmt))
        else_block = create_basic_block(".if_else");

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
    LLVMBasicBlockRef exit_block = create_basic_block(".switch_exit");
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
            cas->llvm_block_ref = create_basic_block(".switch_case");
            last_block = cas->llvm_block_ref;
        }
        if(cas->expr == NULL)
            default_block = cas->llvm_block_ref;
    }

    LLVMPositionBuilderAtEnd(c->builder, orig_block);
    LLVMValueRef switch_val = LLVMBuildSwitch(c->builder, expr.value, default_block, swi->cases.size - (default_block != exit_block)); 
    LLVMBasicBlockRef prev_break = c->break_bb;
    c->break_bb = exit_block;
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
    c->break_bb = prev_break;
    append_old_basic_block(c, exit_block);
}

static void emit_while(CodegenContext* c, ASTStmt* stmt)
{
    ASTWhile* while_stmt = &stmt->stmt.while_;

    LLVMBasicBlockRef cond_block = append_new_basic_block(c, ".while_cond");
    LLVMBasicBlockRef exit_block = create_basic_block(".while_exit");
    LLVMBasicBlockRef body_block = cond_block;

    if(!stmt_empty(while_stmt->body))
        body_block = create_basic_block(".while_body");

    GenValue cond = emit_expr(c, while_stmt->cond);
    load_rvalue(c, &cond);
    LLVMBuildCondBr(c->builder, cond.value, body_block, exit_block);
    
    if(body_block != cond_block)
    {
        LLVMBasicBlockRef prev_break = c->break_bb;
        LLVMBasicBlockRef prev_cont = c->continue_bb;
        c->break_bb = exit_block;
        c->continue_bb = cond_block;
        append_old_basic_block(c, body_block);
        emit_stmt(c, while_stmt->body);
        c->break_bb = prev_break;
        c->continue_bb = prev_cont;
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
        return result;
    case EXPR_ARRAY_INIT_LIST:
        SIC_TODO();
    case EXPR_BINARY:
        emit_binary(c, expr, &result);
        return result;
    case EXPR_CAST: {
        GenValue inner = emit_expr(c, expr->expr.cast.inner);
        emit_cast(c, expr, &inner, &result);
        return result;
    }
    case EXPR_CONSTANT:
        emit_constant(c, expr, &result);
        return result;
    case EXPR_DEFAULT:
        SIC_TODO();
    case EXPR_FUNC_CALL:
        emit_call(c, expr, &result);
        return result;
    case EXPR_IDENT:
        emit_ident(c, expr, &result);
        return result;
    case EXPR_MEMBER_ACCESS:
        emit_member_access(c, expr, &result);
        return result;
    case EXPR_POSTFIX: {
        GenValue inner = emit_expr(c, expr->expr.unary.inner);
        emit_incdec(c, expr, &inner, &result, true);
        return result;
    }
    case EXPR_STRUCT_INIT_LIST:
        SIC_TODO();
    case EXPR_TERNARY:
        emit_ternary(c, expr, &result);
        return result;
    case EXPR_UNARY:
        emit_unary(c, expr, &result);
        return result;
    case EXPR_ZEROED_OUT:
        SIC_TODO();
    case SEMA_ONLY_EXPRS:
        break;
    }
    SIC_UNREACHABLE();
}

static void emit_array_access(CodegenContext* c, ASTExpr* expr, GenValue* result)
{
    ASTExprAAccess* aa = &expr->expr.array_access;
    GenValue array = emit_expr(c, aa->array_expr);
    GenValue index = emit_expr(c, aa->index_expr);
    load_rvalue(c, &array);
    load_rvalue(c, &index);
    Type* arr_type = aa->array_expr->type;
    result->value = LLVMBuildGEP2(c->builder, get_llvm_type(c, type_pointer_base(arr_type)), array.value, &index.value, 1, "");
    result->kind = GEN_VAL_ADDRESS;
}

static void emit_array_initialization(CodegenContext* c, GenValue* lhs, ASTExpr* expr, GenValue* result)
{
    SIC_ASSERT(expr->kind == EXPR_ARRAY_INIT_LIST);
    result->kind = GEN_VAL_ADDRESS;
    if(expr->const_eval)
    {
        LLVMValueRef global = LLVMAddGlobal(c->llvm_module, get_llvm_type(c, expr->type), "__anon.const");
        LLVMSetGlobalConstant(global, true);
        LLVMSetLinkage(global, LLVMPrivateLinkage);
        LLVMSetUnnamedAddress(global, LLVMGlobalUnnamedAddr);
        LLVMSetInitializer(global, emit_const_array_init_list(c, expr));
        LLVMBuildMemCpy(c->builder, 
                        lhs->value, type_alignment(lhs->type), 
                        global, type_alignment(expr->type), 
                        LLVMConstInt(LLVMInt64Type(), type_size(expr->type), false));
        result->value = global;
        return;
    }

    SIC_ASSERT(lhs->type->kind == TYPE_STATIC_ARRAY);
    InitList* list = &expr->expr.init_list;
    for(uint64_t i = 0; i < lhs->type->array.static_len; ++i)
    {
        LLVMValueRef indval = LLVMConstInt(get_llvm_type(c, g_type_usize), i, false);
        GenValue index;
        GenValue temp;
        index.value = LLVMBuildGEP2(c->builder, get_llvm_type(c, lhs->type->array.elem_type), lhs->value, &indval, 1, "");
        index.kind = GEN_VAL_ADDRESS;
        index.type = lhs->type->array.elem_type;
        for(uint32_t j = 0; j < list->size; ++j)
        {
            if(list->data[j].const_index == i)
            {
                ASTExpr cast;
                cast.kind = EXPR_CAST;
                cast.type = expr->type->array.elem_type;
                cast.expr.cast.inner = list->data[j].init_value;
                perform_cast(&cast);
                emit_assign(c, &index, &cast, &temp);
                goto NEXT_ENTRY;
            }
        }
        
        LLVMBuildStore(c->builder, LLVMConstNull(get_llvm_type(c, index.type)), index.value);

    NEXT_ENTRY:;
    }

    result->value = lhs->value;
}

static void emit_binary(CodegenContext* c, ASTExpr* expr, GenValue* result)
{
    ASTExprBinary* binary = &expr->expr.binary;
    GenValue lhs = emit_expr(c, binary->lhs);
    GenValue rhs;
    result->kind = GEN_VAL_RVALUE;
    switch(binary->kind)
    {
    case BINARY_ADD:
        load_rvalue(c, &lhs);
        rhs = emit_expr(c, binary->rhs);
        load_rvalue(c, &rhs);
        emit_add(c, &lhs, &rhs, result);
        return;
    case BINARY_SUB:
        load_rvalue(c, &lhs);
        rhs = emit_expr(c, binary->rhs);
        load_rvalue(c, &rhs);
        emit_sub(c, &lhs, &rhs, result);
        return;
    case BINARY_MUL:
        load_rvalue(c, &lhs);
        rhs = emit_expr(c, binary->rhs);
        load_rvalue(c, &rhs);
        result->value = LLVMBuildMul(c->builder, lhs.value, rhs.value, "");
        return;
    case BINARY_DIV:
        load_rvalue(c, &lhs);
        rhs = emit_expr(c, binary->rhs);
        load_rvalue(c, &rhs);
        if(type_is_signed(expr->type))
            result->value = LLVMBuildSDiv(c->builder, lhs.value, rhs.value, "");
        else if(type_is_unsigned(expr->type))
            result->value = LLVMBuildUDiv(c->builder, lhs.value, rhs.value, "");
        else
            result->value = LLVMBuildFDiv(c->builder, lhs.value, rhs.value, "");
        return;
    case BINARY_MOD:
        load_rvalue(c, &lhs);
        rhs = emit_expr(c, binary->rhs);
        load_rvalue(c, &rhs);
        if(type_is_signed(expr->type))
            result->value = LLVMBuildSRem(c->builder, lhs.value, rhs.value, "");
        else if(type_is_unsigned(expr->type))
            result->value = LLVMBuildURem(c->builder, lhs.value, rhs.value, "");
        else
            result->value = LLVMBuildFRem(c->builder, lhs.value, rhs.value, "");
        return;
    case BINARY_LOG_OR:
        load_rvalue(c, &lhs);
        emit_logical_andor(c, &lhs, binary->rhs, result, true);
        return;
    case BINARY_LOG_AND:
        load_rvalue(c, &lhs);
        emit_logical_andor(c, &lhs, binary->rhs, result, false);
        return;
    case BINARY_EQ:
        load_rvalue(c, &lhs);
        rhs = emit_expr(c, binary->rhs);
        load_rvalue(c, &rhs);
        result->value = type_is_float(binary->lhs->type) ?
                    LLVMBuildFCmp(c->builder, LLVMRealOEQ, lhs.value, rhs.value, "") :
                    LLVMBuildICmp(c->builder, LLVMIntEQ, lhs.value, rhs.value, "");                    
        return;
    case BINARY_NE:
        load_rvalue(c, &lhs);
        rhs = emit_expr(c, binary->rhs);
        load_rvalue(c, &rhs);
        result->value = type_is_float(binary->lhs->type) ?
                    LLVMBuildFCmp(c->builder, LLVMRealONE, lhs.value, rhs.value, "") :
                    LLVMBuildICmp(c->builder, LLVMIntNE, lhs.value, rhs.value, "");                    
        return;
    case BINARY_LT:
        load_rvalue(c, &lhs);
        rhs = emit_expr(c, binary->rhs);
        load_rvalue(c, &rhs);
        result->value = type_is_float(binary->lhs->type) ?
                    LLVMBuildFCmp(c->builder, LLVMRealOLT, lhs.value, rhs.value, "") :
                    LLVMBuildICmp(c->builder, type_is_signed(binary->lhs->type) ? LLVMIntSLT : LLVMIntULT, lhs.value, rhs.value, "");                    
        return;
    case BINARY_LE:
        load_rvalue(c, &lhs);
        rhs = emit_expr(c, binary->rhs);
        load_rvalue(c, &rhs);
        result->value = type_is_float(binary->lhs->type) ?
                    LLVMBuildFCmp(c->builder, LLVMRealOLT, lhs.value, rhs.value, "") :
                    LLVMBuildICmp(c->builder, type_is_signed(binary->lhs->type) ? LLVMIntSLE : LLVMIntULE, lhs.value, rhs.value, "");                    
        return;
    case BINARY_GT:
        load_rvalue(c, &lhs);
        rhs = emit_expr(c, binary->rhs);
        load_rvalue(c, &rhs);
        result->value = type_is_float(binary->lhs->type) ?
                    LLVMBuildFCmp(c->builder, LLVMRealOGT, lhs.value, rhs.value, "") :
                    LLVMBuildICmp(c->builder, type_is_signed(binary->lhs->type) ? LLVMIntSGT : LLVMIntUGT, lhs.value, rhs.value, "");                    
        return;
    case BINARY_GE:
        load_rvalue(c, &lhs);
        rhs = emit_expr(c, binary->rhs);
        load_rvalue(c, &rhs);
        result->value = type_is_float(binary->lhs->type) ?
                    LLVMBuildFCmp(c->builder, LLVMRealOGE, lhs.value, rhs.value, "") :
                    LLVMBuildICmp(c->builder, type_is_signed(binary->lhs->type) ? LLVMIntSGE : LLVMIntUGE, lhs.value, rhs.value, "");                    
        return;
    case BINARY_SHL:
        load_rvalue(c, &lhs);
        rhs = emit_expr(c, binary->rhs);
        load_rvalue(c, &rhs);
        result->value = LLVMBuildShl(c->builder, lhs.value, rhs.value, "");
        return;
    case BINARY_LSHR:
        load_rvalue(c, &lhs);
        rhs = emit_expr(c, binary->rhs);
        load_rvalue(c, &rhs);
        result->value = LLVMBuildLShr(c->builder, lhs.value, rhs.value, "");
        return;
    case BINARY_ASHR:
        load_rvalue(c, &lhs);
        rhs = emit_expr(c, binary->rhs);
        load_rvalue(c, &rhs);
        result->value = LLVMBuildAShr(c->builder, lhs.value, rhs.value, "");
        return;
    case BINARY_BIT_OR:
        load_rvalue(c, &lhs);
        rhs = emit_expr(c, binary->rhs);
        load_rvalue(c, &rhs);
        result->value = LLVMBuildOr(c->builder, lhs.value, rhs.value, "");
        return;
    case BINARY_BIT_XOR:
        load_rvalue(c, &lhs);
        rhs = emit_expr(c, binary->rhs);
        load_rvalue(c, &rhs);
        result->value = LLVMBuildXor(c->builder, lhs.value, rhs.value, "");
        return;
    case BINARY_BIT_AND:
        load_rvalue(c, &lhs);
        rhs = emit_expr(c, binary->rhs);
        load_rvalue(c, &rhs);
        result->value = LLVMBuildAnd(c->builder, lhs.value, rhs.value, "");
        return;
    case BINARY_ASSIGN:
        emit_assign(c, &lhs, binary->rhs, result);
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
        break;
    }
    SIC_UNREACHABLE();
}

static void emit_call(CodegenContext* c, ASTExpr* expr, GenValue* result)
{
    ASTExprCall* call = &expr->expr.call;
    GenValue func_val = emit_expr(c, call->func_expr);
    load_rvalue(c, &func_val);
    Type* func_type = call->func_expr->type;

    // TODO: Make a permanent solution. I want to limit the number of parameters
    //       a function can have, but for now I just want to avoid MALLOCing
    //       unnecessarily.
    LLVMValueRef args[256];
    SIC_ASSERT(call->args.size <= 256);
    for(uint32_t i = 0; i < call->args.size; ++i)
    {
        GenValue temp = emit_expr(c, call->args.data[i]);
        load_rvalue(c, &temp);
        args[i] = temp.value;
    }
    result->value = LLVMBuildCall2(c->builder, 
                                   get_llvm_func_type(c, func_type),
                                   func_val.value,
                                   args, call->args.size, 
                                   "");
    result->kind = GEN_VAL_RVALUE;
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
    case CAST_PTR_TO_BOOL:
        result->value = LLVMBuildPtrToInt(c->builder, inner->value, get_llvm_type(c, g_type_uptr), "");
        result->value = LLVMBuildIsNotNull(c->builder, result->value, "");
        return;
    case CAST_PTR_TO_INT:
        result->value = LLVMBuildPtrToInt(c->builder, inner->value, get_llvm_type(c, g_type_uptr), "");
        return;
    case CAST_INT_TO_PTR:
        result->value = LLVMBuildIntToPtr(c->builder, inner->value, c->ptr_type, "");
        return;
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
    case CONSTANT_FLOAT:
        result->value = LLVMConstReal(get_llvm_type(c, expr->type), constant->val.f);
        return;
    case CONSTANT_STRING: {
        LLVMValueRef str = LLVMConstString(constant->val.str, constant->val.str_len, false);
        LLVMValueRef global_string = LLVMAddGlobal(c->llvm_module, LLVMTypeOf(str), ".str");
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

static LLVMValueRef emit_const_initializer(CodegenContext* c, ASTExpr* expr)
{
    switch(expr->kind)
    {
    case EXPR_ARRAY_INIT_LIST:
        return emit_const_array_init_list(c, expr);
    case EXPR_CONSTANT: {
        ASTExprConstant* constant = &expr->expr.constant;
        uint64_t size = expr->type->array.static_len;
        if(constant->kind == CONSTANT_STRING && type_is_array(expr->type))
        {
            SIC_ASSERT(size > constant->val.str_len);
            char* str;
            if((size - constant->val.str_len) > 1)
            {
                str = MALLOC(size, sizeof(char));
                memcpy(str, constant->val.str, constant->val.str_len);
                memset(str + constant->val.str_len, 0, size - constant->val.str_len);
            }
            else
                str = constant->val.str;
            LLVMValueRef strref = LLVMConstString(str, size, true);
            FREE(str, size); // Attempt to free if we can.
            return strref;
        }

        GenValue v;
        emit_constant(c, expr, &v);
        return v.value;
    }
    default:
        return emit_expr(c, expr).value;
    }
}

static LLVMValueRef emit_const_array_init_list(CodegenContext* c, ASTExpr* expr)
{
    SIC_ASSERT(expr->type != g_type_anon_arr);
    SIC_ASSERT(expr->const_eval);
    InitList* list = &expr->expr.init_list;
    if(list->size == 0)
        return LLVMConstNull(get_llvm_type(c, expr->type));

    LLVMValueRef* values = CALLOC_STRUCTS(LLVMValueRef, expr->type->array.static_len);
    for(uint32_t i = 0; i < list->size; ++i)
    {
        ASTExpr cast;
        cast.kind = EXPR_CAST;
        cast.type = expr->type->array.elem_type;
        cast.expr.cast.inner = list->data[i].init_value;
        perform_cast(&cast);
        values[list->data[i].const_index] = emit_const_initializer(c, &cast);
    }
    for(uint32_t i = 0; i < expr->type->array.static_len; ++i)
    {
        if(values[i] == NULL)
            values[i] = LLVMConstNull(get_llvm_type(c, expr->type->array.elem_type));
    }
    return LLVMConstArray2(get_llvm_type(c, expr->type->array.elem_type), values, expr->type->array.static_len);
}


static void emit_ident(CodegenContext* c, ASTExpr* expr, GenValue* result)
{
    Object* obj = expr->expr.ident;
    result->value = get_llvm_ref(c, obj);
    switch(obj->kind)
    {
    case OBJ_FUNC:
        result->kind = GEN_VAL_RVALUE;
        return;
    case OBJ_VAR:
        result->kind = GEN_VAL_ADDRESS;
        return;
    default:
        SIC_UNREACHABLE();
    }
}

static void emit_incdec(CodegenContext* c, ASTExpr* expr, GenValue* inner, GenValue* result, bool is_post)
{
    GenValue inner_rval = *inner;
    load_rvalue(c, &inner_rval);

    GenValue temp;
    temp.value = LLVMConstInt(get_llvm_type(c, inner->type), 1, false);
    if(expr->expr.unary.kind == UNARY_INC)
        emit_add(c, &inner_rval, &temp, result);
    else
        emit_sub(c, &inner_rval, &temp, result);
    LLVMBuildStore(c->builder, result->value, inner->value);
    result->kind = GEN_VAL_RVALUE;
    if(is_post)
        result->value = inner_rval.value;
}

static void emit_member_access(CodegenContext* c, ASTExpr* expr, GenValue* result)
{
    ASTExprMAccess* maccess = &expr->expr.member_access;
    ASTExpr* parent = expr->expr.member_access.parent_expr;
    GenValue parent_val = emit_expr(c, parent);

    result->kind = GEN_VAL_ADDRESS;
    if(parent->type->kind == TYPE_UNION || maccess->member_idx == 0)
    {
        result->value = parent_val.value;
        return;
    }

    result->value = LLVMBuildStructGEP2(c->builder, 
                                        get_llvm_type(c, maccess->parent_expr->type), 
                                        parent_val.value, 
                                        maccess->member_idx, 
                                        "");
}

static void emit_logical_andor(CodegenContext* c, GenValue* lhs, ASTExpr* rhs, GenValue* result, bool is_or)
{
    LLVMBasicBlockRef rhs_bb = LLVMAppendBasicBlock(c->cur_func->header.llvm_ref, ".log_rhs");
    LLVMBasicBlockRef blocks[2] = { c->cur_bb, rhs_bb };
    LLVMBasicBlockRef exit_bb = LLVMAppendBasicBlock(c->cur_func->header.llvm_ref, ".log_exit");
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
    LLVMBasicBlockRef else_bb = create_basic_block(".cond_else");
    LLVMBasicBlockRef exit_bb = create_basic_block(".cond_exit");
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
        then_bb = create_basic_block(".cond_then");
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
    GenValue inner = emit_expr(c, unary->inner);
    switch(unary->kind)
    {
    case UNARY_ADDR_OF:
        result->kind = GEN_VAL_RVALUE;
        result->value = inner.value;
        return;
    case UNARY_DEREF:
        load_rvalue(c, &inner);
        result->kind = GEN_VAL_ADDRESS;
        result->value = inner.value;
        return;
    case UNARY_DEC:
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

static void emit_assign(CodegenContext* c, GenValue* lhs, ASTExpr* rhs, GenValue* result)
{
    if(rhs->kind == EXPR_ARRAY_INIT_LIST)
    {
        emit_array_initialization(c, lhs, rhs, result);
        return;
    }

    GenValue rhsval = emit_expr(c, rhs);
    if(rhsval.kind == GEN_VAL_ADDRESS)
    {
        if(type_is_trivially_copyable(rhs->type))
            load_rvalue(c, &rhsval);
        else
        {
            LLVMBuildMemCpy(c->builder, 
                            lhs->value, type_alignment(lhs->type), 
                            rhsval.value, type_alignment(rhs->type), 
                            LLVMConstInt(LLVMInt64Type(), type_size(rhs->type), false));
            result->kind = GEN_VAL_ADDRESS;
            result->value = rhsval.value;
            return;
        }
    }

    LLVMBuildStore(c->builder, rhsval.value, lhs->value);
    result->kind = GEN_VAL_RVALUE;
    result->value = rhsval.value;
}

static void emit_br(CodegenContext* c, LLVMBasicBlockRef block)
{
    if(c->cur_bb != NULL)
        LLVMBuildBr(c->builder, block);
    c->cur_bb = NULL;
}

static void emit_sub(CodegenContext* c, GenValue* left, GenValue* right, GenValue* result)
{
    if(type_is_pointer(left->type))
    {
        right->value = LLVMBuildNeg(c->builder, right->value, "");
        result->value = LLVMBuildGEP2(c->builder, get_llvm_type(c, left->type->pointer_base), left->value, &right->value, 1, "");
    }
    else if(type_is_float(left->type))
        result->value = LLVMBuildFSub(c->builder, left->value, right->value, "");
    else
        result->value = LLVMBuildSub(c->builder, left->value, right->value, "");
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

static void emit_var_alloca(CodegenContext* c, ObjVar* var)
{
    if(var->type_loc.type->kind == TYPE_RUNTIME_ARRAY)
    {
        GenValue size_val = emit_expr(c, var->type_loc.type->array.size_expr);
        load_rvalue(c, &size_val);
        var->header.llvm_ref = LLVMBuildArrayAlloca(c->builder, 
                                                    get_llvm_type(c, var->type_loc.type), 
                                                    size_val.value,
                                                    var->header.symbol); 
        return;
    }
    var->header.llvm_ref = emit_alloca(c, var->header.symbol, get_llvm_type(c, var->type_loc.type), type_alignment(var->type_loc.type));
}

static LLVMBasicBlockRef create_basic_block(const char* label)
{
    return LLVMCreateBasicBlockInContext(s_context, label);
}

static LLVMBasicBlockRef append_new_basic_block(CodegenContext* c, const char* label)
{
    LLVMBasicBlockRef res = LLVMAppendBasicBlock(c->cur_func->header.llvm_ref, label);
    emit_br(c, res);
    LLVMPositionBuilderAtEnd(c->builder, res);
    return c->cur_bb = res;
}

static void append_old_basic_block(CodegenContext* c, LLVMBasicBlockRef block)
{
    LLVMAppendExistingBasicBlock(c->cur_func->header.llvm_ref, block);
    use_basic_block(c, block);
}

static void use_basic_block(CodegenContext* c, LLVMBasicBlockRef block)
{
    c->cur_bb = block;
    LLVMPositionBuilderAtEnd(c->builder, block);
}

static void emit_llvm_ir(CodegenContext* c)
{
    char* error;
    if(LLVMVerifyModule(c->llvm_module, LLVMPrintMessageAction, &error))
    {
        if(error != NULL && *error != '\0')
            error = "No error supplied.";
        sic_fatal_error("Failed to verify LLVM-IR module: %s", error);
    }
    if(LLVMPrintModuleToFile(c->llvm_module, c->llvm_filename, &error))
        sic_fatal_error("Failed to emit LLVM IR: \'%s\'", error);
}

static void emit_file(CodegenContext* c, const char* out_path, LLVMCodeGenFileType llvm_file_type)
{
    char* error;
    if(LLVMTargetMachineEmitToFile(c->target_machine, c->llvm_module, out_path, llvm_file_type, &error))
        sic_fatal_error("LLVM failed to emit file \'%s\': %s", out_path, error);
}

static LLVMTypeRef get_llvm_type(CodegenContext* c, Type* type)
{
    type = type->canonical;
    switch(type->kind)
    {
    case TYPE_VOID:
        return type->llvm_ref = LLVMVoidType();
    case TYPE_BOOL:
    case TYPE_CHAR:
    case TYPE_BYTE:
    case TYPE_UBYTE:
        return type->llvm_ref = LLVMInt8Type();
    case TYPE_SHORT:
    case TYPE_USHORT:
        return type->llvm_ref = LLVMInt16Type();
    case TYPE_INT:
    case TYPE_UINT:
        return type->llvm_ref = LLVMInt32Type();
    case TYPE_LONG:
    case TYPE_ULONG:
        return type->llvm_ref = LLVMInt64Type();
    case TYPE_FLOAT:
        return type->llvm_ref = LLVMFloatType();
    case TYPE_DOUBLE:
        return type->llvm_ref = LLVMDoubleType();
    case TYPE_POINTER:
        return type->llvm_ref = c->ptr_type;
    case TYPE_FUNC_PTR:
        return c->ptr_type;
    case TYPE_STATIC_ARRAY:
        if(type->llvm_ref != NULL) return type->llvm_ref;
        return type->llvm_ref = LLVMArrayType(get_llvm_type(c, type->array.elem_type), type->array.static_len);
    case TYPE_RUNTIME_ARRAY:
        if(type->llvm_ref != NULL) return type->llvm_ref;
        return type->llvm_ref = get_llvm_type(c, type->array.elem_type);
    case TYPE_ALIAS_DISTINCT:
        if(type->llvm_ref != NULL) return type->llvm_ref;
        return type->llvm_ref = get_llvm_type(c, type->typedef_->alias.type);
    case TYPE_ENUM_DISTINCT:
        if(type->llvm_ref != NULL) return type->llvm_ref;
        return type->llvm_ref = get_llvm_type(c, type->enum_->underlying.type);
    case TYPE_STRUCT: {
        if(type->llvm_ref != NULL) return type->llvm_ref;
        ObjStruct* struct_ = type->struct_;
        if(struct_->header.llvm_ref)
            return type->llvm_ref = struct_->header.llvm_ref;
        LLVMTypeRef* element_types = MALLOC_STRUCTS(LLVMTypeRef, struct_->members.size);
        for(uint32_t i = 0; i < struct_->members.size; ++i)
            element_types[i] = get_llvm_type(c, struct_->members.data[i]->type_loc.type);
        scratch_clear();
        scratch_append("struct.");
        scratch_append(struct_->header.symbol);
        struct_->header.llvm_ref = LLVMStructCreateNamed(s_context, scratch_string());
        LLVMStructSetBody(struct_->header.llvm_ref, element_types, struct_->members.size, false);
        return type->llvm_ref = struct_->header.llvm_ref;
    }
    case TYPE_UNION: {
        if(type->llvm_ref != NULL) return type->llvm_ref;
        ObjStruct* union_ = type->struct_;
        if(union_->header.llvm_ref)
            return type->llvm_ref = union_->header.llvm_ref;
        union_->header.llvm_ref = get_llvm_type(c, union_->largest_type);
        return type->llvm_ref = union_->header.llvm_ref;
    }
    case TYPE_INVALID:
    case TYPE_ALIAS:
    case TYPE_ENUM:
    case TYPE_PS_ARRAY:
    case TYPE_PS_USER:
    case TYPE_ANON_ARRAY:
    case TYPE_AUTO:
    case TYPE_TYPEOF:
    case __TYPE_COUNT:
        break;
    }
    SIC_UNREACHABLE();
}

static LLVMTypeRef get_llvm_func_type(CodegenContext* c, Type* type)
{
    SIC_ASSERT(type->kind == TYPE_FUNC_PTR);
    if(type->llvm_ref != NULL)
        return type->llvm_ref;

    LLVMTypeRef* param_types = NULL;
    FuncSignature* sig = type->func_ptr;
    if(sig->params.size > 0)
        param_types = MALLOC_STRUCTS(LLVMTypeRef, sig->params.size);

    for(uint32_t i = 0; i < sig->params.size; ++i)
        param_types[i] = get_llvm_type(c, sig->params.data[i]->type_loc.type);

    return type->llvm_ref = LLVMFunctionType(get_llvm_type(c, sig->ret_type.type), param_types, sig->params.size, sig->is_var_arg);
}

static LLVMValueRef get_llvm_ref(CodegenContext* c, Object* obj)
{
    if(obj->llvm_ref != NULL && LLVMGetGlobalParent(obj->llvm_ref) == c->llvm_module)
        return obj->llvm_ref;
    switch(obj->kind)
    {
    case OBJ_FUNC: {
        ObjFunc* func = obj_as_func(obj);
        return obj->llvm_ref = LLVMAddFunction(c->llvm_module, obj->symbol, get_llvm_func_type(c, func->func_type));
    }
    case OBJ_VAR: {
        ObjVar* var = obj_as_var(obj);
        if(var->kind != VAR_GLOBAL)
            return obj->llvm_ref;
        return obj->llvm_ref = LLVMAddGlobal(c->llvm_module, get_llvm_type(c, var->type_loc.type), obj->symbol);
    }
    case OBJ_ENUM_VALUE:
    case OBJ_BITFIELD:
    case OBJ_ENUM:
    case OBJ_IMPORT:
    case OBJ_MODULE:
    case OBJ_STRUCT:
    case OBJ_TYPEDEF:
    case OBJ_UNION:
    case OBJ_INVALID:
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

static void load_rvalue(CodegenContext* c, GenValue* lvalue)
{
    if(lvalue->kind == GEN_VAL_RVALUE)
        return;
    lvalue->kind = GEN_VAL_RVALUE;
    switch(lvalue->type->canonical->kind)
    {
    case TYPE_BOOL:
        lvalue->value = LLVMBuildLoad2(c->builder, get_llvm_type(c, lvalue->type), lvalue->value, "");
        lvalue->value = LLVMBuildTrunc(c->builder, lvalue->value, LLVMInt1Type(), "");
        return;
    case TYPE_CHAR:
    case TYPE_BYTE:
    case TYPE_UBYTE:
    case TYPE_SHORT:
    case TYPE_USHORT:
    case TYPE_INT:
    case TYPE_UINT:
    case TYPE_LONG:
    case TYPE_ULONG:
    case TYPE_FLOAT:
    case TYPE_DOUBLE:
    case TYPE_POINTER:
    case TYPE_FUNC_PTR:
    case TYPE_ENUM:
    case TYPE_ENUM_DISTINCT:
    case TYPE_STRUCT:
    case TYPE_UNION:
        lvalue->value = LLVMBuildLoad2(c->builder, get_llvm_type(c, lvalue->type), lvalue->value, "");
        return;
    case TYPE_STATIC_ARRAY:
    case TYPE_RUNTIME_ARRAY:
        return;
    case TYPE_INVALID:
    case TYPE_VOID:
    case TYPE_ALIAS:
    case TYPE_ALIAS_DISTINCT:
    case TYPE_PS_ARRAY:
    case TYPE_PS_USER:
    case TYPE_ANON_ARRAY:
    case TYPE_AUTO:
    case TYPE_TYPEOF:
    case __TYPE_COUNT:
        break;
    }
    SIC_UNREACHABLE();
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
    (void)severity;
#ifdef SI_DEBUG
    if(g_compiler.debug_output & DEBUG_CODEGEN)
        printf("[DEBUG] %s\033[0m: %s\n", severity, desc);
#endif
	LLVMDisposeMessage(desc);
}
