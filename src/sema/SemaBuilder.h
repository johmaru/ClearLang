#pragma once
#include <memory>

#include "ClearLanguageBaseVisitor.h"
#include "SemaIR.h"


[[maybe_unused]] inline constexpr auto DOUBLE_COLON = "::";

class sema_builder : public ClearLanguageBaseVisitor {
 public:
  sema_builder();

  std::shared_ptr<sema::module> take_module();

  std::any visitIntLiteral(
      ClearLanguageParser::IntLiteralContext* ctx) override;
  std::any visitFloatLiteral(
      ClearLanguageParser::FloatLiteralContext* ctx) override;
  std::any visitUnaryMinus(
      ClearLanguageParser::UnaryMinusContext* ctx) override;
  std::any visitUnaryPrimary(
      ClearLanguageParser::UnaryPrimaryContext* ctx) override;
  std::any visitPostfixExpr(
      ClearLanguageParser::PostfixExprContext* ctx) override;
  std::any visitParenExpr(ClearLanguageParser::ParenExprContext* ctx) override;
  std::any visitUnitLiteral(
      ClearLanguageParser::UnitLiteralContext* ctx) override;
  std::any visitOrExpr(ClearLanguageParser::OrExprContext* ctx) override;
  std::any visitAndExpr(ClearLanguageParser::AndExprContext* ctx) override;
  std::any visitEqualExpr(ClearLanguageParser::EqualExprContext* ctx) override;
  std::any visitAddExpr(ClearLanguageParser::AddExprContext* ctx) override;
  std::any visitMulExpr(ClearLanguageParser::MulExprContext* ctx) override;
  std::any visitVarRef(ClearLanguageParser::VarRefContext* ctx) override;
  std::any visitBlock(ClearLanguageParser::BlockContext* ctx) override;
  std::any visitIfBlock(ClearLanguageParser::IfBlockContext* ctx) override;
  std::any visitIfSingle(ClearLanguageParser::IfSingleContext* ctx) override;
  std::any visitStmtVarDecl(
      ClearLanguageParser::StmtVarDeclContext* ctx) override;
  std::any visitConstantDecl(
      ClearLanguageParser::ConstantDeclContext* context) override;
  std::any visitStmtReturn(
      ClearLanguageParser::StmtReturnContext* ctx) override;
  std::any visitStringLiteral(
      ClearLanguageParser::StringLiteralContext* ctx) override;
  std::any visitBoolLiteral(
      ClearLanguageParser::BoolLiteralContext* ctx) override;
  std::any visitStmtExpr(ClearLanguageParser::StmtExprContext* ctx) override;

  void collect_signatures(ClearLanguageParser::StartContext* ctx);
  void construct_target(ClearLanguageParser::StartContext* ctx);

 private:
  std::string current_package_;

  std::unordered_map<std::string, std::string> imports_;

 public:
  enum class symbol_kind : std::uint8_t {
    variable,
    constant,
    function,
    type_name,
  };

  enum class scope_kind : std::uint8_t { global, function, block };

  using value_variant = std::variant<std::monostate, int64_t, uint64_t, double,
                                     bool, std::string>;

  struct symbol_entry {
    symbol_kind kind;
    type_ref type;
    bool is_mutable = true;

    value_variant value;
    std::shared_ptr<sema::expr> const_expr;

    std::shared_ptr<function_sig> function_sig;

    [[nodiscard]] bool has_folded() const noexcept {
      return !std::holds_alternative<std::monostate>(value);
    }
  };

  struct scope_frame {
    scope_kind kind;
    std::unordered_map<std::string, symbol_entry> symbols;
  };

 private:
  std::vector<scope_frame> symbol_scopes_;
  std::shared_ptr<sema::module> mod_;
  type_ref current_return_type_ =
      type_ref::builtin_type(type{type::kind_enum::unit});

  void push_scope(const scope_kind k) {
    symbol_scopes_.push_back(scope_frame{k, {}});
  }
  void pop_scope() {
    if (symbol_scopes_.empty())
      throw std::runtime_error("symbol scope underflow");
    symbol_scopes_.pop_back();
  }

  bool insert_symbol(const std::string& name, symbol_entry& entry);

  [[nodiscard]] const symbol_entry* lookup_symbol(
      const std::string& name) const {
    for (auto it = symbol_scopes_.rbegin(); it != symbol_scopes_.rend(); ++it) {
      auto f = it->symbols.find(name);
      if (f != it->symbols.end()) return &f->second;
    }
    return nullptr;
  }

  void register_builtin_types();
  [[nodiscard]] type_ref resolve_type(const std::string& name) const;
  type_ref make_type_ref_from(ClearLanguageParser::TypeContext* ctx);

  [[nodiscard]] std::string resolve_function_name(
      const std::string& name) const;
  [[nodiscard]] const symbol_entry* lookup_function_symbol(
      const std::string& name) const;
  [[nodiscard]] type_ref resolve_type_symbol(const std::string& name) const;

  bool try_fold_expr(const std::shared_ptr<sema::expr>& e,
                     value_variant& out) const;

  [[nodiscard]] std::string qualify(const std::string& name) const {
    return current_package_.empty() ? name : current_package_ + "::" + name;
  }

  struct scope_guard {
    sema_builder& sb;
    bool active{true};
    scope_kind kind;
    explicit scope_guard(sema_builder& b, scope_kind k) : sb(b), kind(k) {
      sb.push_scope(k);
    }
    ~scope_guard() {
      if (active) sb.pop_scope();
    }
    void dismiss() { active = false; }
    scope_guard(const scope_guard&) = delete;
    scope_guard& operator=(const scope_guard&) = delete;
  };

  struct file_ctx_guard {
    sema_builder& sb;
    std::string prev_package;
    std::unordered_map<std::string, std::string> prev_imports;

    file_ctx_guard(sema_builder& b, ClearLanguageParser::StartContext* ctx)
        : sb(b), prev_package(b.current_package_), prev_imports(b.imports_) {
      if (auto* pkg = ctx->packageDecl())
        sb.current_package_ = pkg->qualifiedIdent()->getText();
      else
        sb.current_package_.clear();

      sb.imports_.clear();
      for (auto* imp : ctx->importDecl()) {
        const std::string full = imp->qualifiedIdent()->getText();
        std::string alias = imp->AS() ? imp->IDENT()->getText() : full;
        sb.imports_[alias] = full;
      }
    }

    ~file_ctx_guard() {
      sb.current_package_ = std::move(prev_package);
      sb.imports_ = std::move(prev_imports);
    }

    file_ctx_guard(const file_ctx_guard&) = delete;
    file_ctx_guard& operator=(const file_ctx_guard&) = delete;
  };

  [[nodiscard]] const symbol_entry* lookup_symbol_qualified(
      const std::string& name) const {
    auto q = name;
    auto fqn = qualify(q);

    if (!symbol_scopes_.empty()) {
      auto& g = symbol_scopes_.front();
      auto it = g.symbols.find(fqn);
      if (it != g.symbols.end()) return &it->second;
    }
    return lookup_symbol(name);
  }
};