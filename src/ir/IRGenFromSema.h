#pragma once
#include <memory>
#include <unordered_map>
#include "../sema/SemaIR.h"
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Module.h>

class IRGenFromSema {
public:
    IRGenFromSema(llvm::LLVMContext& ctx, std::string moduleName);

    llvm::Module& module();
    void emitModule(const sema::Module& m);
    // Transfer ownership of the underlying Module (for ORC JIT)
    std::unique_ptr<llvm::Module> takeModule();

private:
    llvm::Function* emitFunction(const sema::Function& f);
    void emitBlock(const sema::Block& b);
    llvm::Value* emitExpr(const sema::Expr& e);
    void emitStmt(const sema::Stmt& s);

    llvm::Type* toLlvmType(const TypeRef& t);
    llvm::Constant* toLlvmConst(const Value& v);
    llvm::Value* emitBinOp(const sema::BinOp& b);
    llvm::Value* emitUnary(const sema::Unary& u);

private:
    void emitEntryShim(const sema::Module& m);
    llvm::LLVMContext& ctx_;
    std::unique_ptr<llvm::Module> mod_;
    std::unique_ptr<llvm::IRBuilder<>> builder_;
    std::vector<std::unordered_map<std::string, llvm::Value*>> vars_;
};