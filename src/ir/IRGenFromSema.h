#pragma once
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Module.h>

#include <memory>
#include <unordered_map>

#include "../sema/SemaIR.h"

class ir_gen_from_sema {
 public:
  ir_gen_from_sema(llvm::LLVMContext& ctx, const std::string& module_name);
  [[nodiscard]] llvm::Module& module() const;
  void emit_module(const sema::module& m);
  // Transfer ownership of the underlying Module (for ORC JIT)
  std::unique_ptr<llvm::Module> take_module();

 private:
  llvm::Function* emit_function(const sema::function& f);
  void emit_block(const sema::block& b);
  llvm::Value* emit_expr(const sema::expr& e);
  void emit_stmt(const sema::stmt& s);

  [[nodiscard]] llvm::Type* to_llvm_type(const type_ref& t) const;
  [[nodiscard]] llvm::Constant* to_llvm_const(const value& v) const;
  llvm::Value* emit_bin_op(const sema::bin_op& b);
  llvm::Value* emit_unary(const sema::unary& u);

 private:
  void emit_entry_shim(const sema::module& m) const;
  llvm::LLVMContext& ctx_;
  std::unique_ptr<llvm::Module> mod_;
  std::unique_ptr<llvm::IRBuilder<>> builder_;
  std::vector<std::unordered_map<std::string, llvm::Value*>> vars_;
};