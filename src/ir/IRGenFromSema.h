#pragma once
#include "../sema/SemaIR.h"

#include "llvm/IR/Instructions.h"
#include "llvm/IR/Value.h"

#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Module.h>
#include <memory>
#include <unordered_map>
#include <variant>

class IrGenFromSema {
  public:
    IrGenFromSema(llvm::LLVMContext& ctx, const std::string& module_name);
    [[nodiscard]] llvm::Module& module() const;
    void emitModule(const sema::Module& module);
    // Transfer ownership of the underlying Module (for ORC JIT)
    std::unique_ptr<llvm::Module> takeModule();

  private:
    struct RValue {
        llvm::Value* val;
    };
    struct LValue {
        llvm::AllocaInst* addr;
    };
    using Binding = std::variant<RValue, LValue>;

    llvm::AllocaInst* createAllocaInEntry(llvm::Function* func, llvm::Type* type,
                                          llvm::StringRef name);

    llvm::Function* emitFunction(const sema::Function& func);
    void emitBlock(const sema::Block& blk);
    llvm::Value* emitExpr(const sema::Expr& expr);
    void emitStmt(const sema::Stmt& stmt);

    [[nodiscard]] llvm::Type* toLlvmType(const TypeRef& t_ref) const;
    [[nodiscard]] llvm::Constant* toLlvmConst(const Value& val) const;
    llvm::Value* emitBinOp(const sema::BinOp& bin_op);
    llvm::Value* emitUnary(const sema::Unary& unary);

    void emitEntryShim(const sema::Module& module) const;
    llvm::LLVMContext& ctx_;
    std::unique_ptr<llvm::Module> mod_;
    std::unique_ptr<llvm::IRBuilder<>> builder_;
    std::vector<std::unordered_map<std::string, Binding>> vars_;
};
