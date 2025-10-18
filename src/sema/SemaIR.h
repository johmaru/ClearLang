#pragma once
#include <memory>
#include <string>
#include <vector>

#include "../core/CLType.h"

namespace sema {
struct node {
  virtual ~node() = default;
};

struct expr : node {
  type_ref type;
  [[nodiscard]] virtual bool is_const() const { return false; }
};

struct literal : expr {
  value value;
  [[nodiscard]] bool is_const() const override { return true; }
};

struct var_ref : expr {
  std::string name;
};

struct cast : expr {
  type_ref target_type;
  std::shared_ptr<expr> inner;
  [[nodiscard]] bool is_const() const override { return inner->is_const(); }
};

struct bin_op : expr {
  std::string op;
  std::shared_ptr<expr> lhs;
  std::shared_ptr<expr> rhs;
};

struct unary : expr {
  std::string op;
  std::shared_ptr<expr> inner;
};

struct call : expr {
  std::string callee;
  std::vector<std::shared_ptr<expr>> args;
};

struct stmt : node {};
struct stmt_var_decl : stmt {
  std::string name;
  type_ref decl_type;
  std::shared_ptr<expr> init_expr;  // may be null
};

struct stmt_return : stmt {
  std::shared_ptr<expr> value;
};

struct stmt_expr : stmt {
  std::shared_ptr<expr> expr;
};

struct block : stmt {
  std::vector<std::shared_ptr<stmt>> statements;
};

struct stmt_if : stmt {
  std::shared_ptr<expr> cond;
  std::shared_ptr<block> then_blk;
  std::shared_ptr<block> else_blk;
};

struct param {
  std::string name;
  type_ref type;
};
struct function {
  std::string name;
  std::vector<param> params;
  type_ref return_type;
  std::shared_ptr<block> body;
};

struct module : node {
  std::vector<std::shared_ptr<function>> functions;
  std::string entry_name;  // optional; set when [EntryPoint] is present
};

}  // namespace sema