#pragma once
#include "ClearLanguageBaseVisitor.h"
#include "ClearLanguageParser.h"
#include "ParserRuleContext.h"
#include "SemaIR.h"

#include <memory>

[[maybe_unused]] inline constexpr auto DOUBLE_COLON = "::";

class SemaBuilder : public ClearLanguageBaseVisitor {
  public:
    SemaBuilder();

    std::shared_ptr<sema::Module> takeModule();

    std::any visitIntLiteral(ClearLanguageParser::IntLiteralContext* ctx) override;
    std::any visitFloatLiteral(ClearLanguageParser::FloatLiteralContext* ctx) override;
    std::any visitUnaryMinus(ClearLanguageParser::UnaryMinusContext* ctx) override;
    std::any visitUnaryPrimary(ClearLanguageParser::UnaryPrimaryContext* ctx) override;
    std::any visitPostfixExpr(ClearLanguageParser::PostfixExprContext* ctx) override;
    std::any visitUnaryRef(ClearLanguageParser::UnaryRefContext* ctx) override;
    std::any visitUnaryDeref(ClearLanguageParser::UnaryDerefContext* ctx) override;
    std::any visitParenExpr(ClearLanguageParser::ParenExprContext* ctx) override;
    std::any visitUnitLiteral(ClearLanguageParser::UnitLiteralContext* ctx) override;
    std::any visitOrExpr(ClearLanguageParser::OrExprContext* ctx) override;
    std::any visitAndExpr(ClearLanguageParser::AndExprContext* ctx) override;
    std::any visitEqualExpr(ClearLanguageParser::EqualExprContext* ctx) override;
    std::any visitAddExpr(ClearLanguageParser::AddExprContext* ctx) override;
    std::any visitMulExpr(ClearLanguageParser::MulExprContext* ctx) override;
    std::any visitVarRef(ClearLanguageParser::VarRefContext* ctx) override;
    std::any visitBlock(ClearLanguageParser::BlockContext* ctx) override;
    std::any visitIfBlock(ClearLanguageParser::IfBlockContext* ctx) override;
    std::any visitIfSingle(ClearLanguageParser::IfSingleContext* ctx) override;
    std::any visitStmtVarDecl(ClearLanguageParser::StmtVarDeclContext* ctx) override;
    std::any visitConstantDecl(ClearLanguageParser::ConstantDeclContext* ctx) override;
    std::any visitVarStmtDecl(ClearLanguageParser::VarStmtDeclContext* ctx) override;
    std::any visitConstExpr(ClearLanguageParser::ConstExprContext* ctx) override;
    std::any visitConstAddExpr(ClearLanguageParser::ConstAddExprContext* ctx) override;
    std::any visitConstMulExpr(ClearLanguageParser::ConstMulExprContext* ctx) override;
    std::any visitUnaryConstMinus(ClearLanguageParser::UnaryConstMinusContext* ctx) override;
    std::any visitIntConstLiteral(ClearLanguageParser::IntConstLiteralContext* ctx) override;
    std::any visitFloatConstLiteral(ClearLanguageParser::FloatConstLiteralContext* ctx) override;
    std::any visitStringConstLiteral(ClearLanguageParser::StringConstLiteralContext* ctx) override;
    std::any visitBoolConstLiteral(ClearLanguageParser::BoolConstLiteralContext* ctx) override;
    std::any visitParenConstExpr(ClearLanguageParser::ParenConstExprContext* ctx) override;
    std::any visitStmtReturn(ClearLanguageParser::StmtReturnContext* ctx) override;
    std::any visitStringLiteral(ClearLanguageParser::StringLiteralContext* ctx) override;
    std::any visitBoolLiteral(ClearLanguageParser::BoolLiteralContext* ctx) override;
    std::any visitStmtExpr(ClearLanguageParser::StmtExprContext* ctx) override;

    // Build phases
    void collectSignatures(ClearLanguageParser::StartContext* ctx);
    void constructTarget(ClearLanguageParser::StartContext* ctx);

    // Error handling function
    void setCurrentFile(const std::string& file_path) {
        current_file_ = file_path;
    }

    [[nodiscard]] const std::string& currentFile() const noexcept {
        return current_file_;
    }

    [[noreturn]] void throwErrorAt(antlr4::ParserRuleContext* ctx, const std::string& msg) const;

  private:
    std::string current_package_;

    std::unordered_map<std::string, std::string> imports_;

    // Error context
    std::string current_file_;

  public:
    enum class symbol_kind : std::uint8_t {
        VARIABLE,
        CONSTANT,
        FUNCTION,
        TYPE_NAME,
    };

    enum class scope_kind : std::uint8_t { GLOBAL, FUNCTION, BLOCK };

    using value_variant =
        std::variant<std::monostate, int64_t, uint64_t, double, bool, std::string>;

    struct SymbolEntry {
        symbol_kind kind;
        TypeRef type;
        sema::mutability mut = sema::mutability::VAR;

        value_variant value;
        std::shared_ptr<sema::Expr> const_expr;

        std::shared_ptr<FunctionSig> function_sig;

        [[nodiscard]] bool hasFolded() const noexcept {
            return !std::holds_alternative<std::monostate>(value);
        }

        [[nodiscard]] bool isConstant() const noexcept {
            return mut == sema::mutability::CONST;
        }
        [[nodiscard]] bool isLet() const noexcept {
            return mut == sema::mutability::LET;
        }
        [[nodiscard]] bool isVariable() const noexcept {
            return mut == sema::mutability::VAR;
        }
    };

    struct ScopeFrame {
        scope_kind kind;
        std::unordered_map<std::string, SymbolEntry> symbols;
    };

  private:
    std::vector<ScopeFrame> symbol_scopes_;
    std::shared_ptr<sema::Module> mod_;
    TypeRef current_return_type_ = TypeRef::builtinType(Type{Type::kind_enum::UNIT});

    void pushScope(const scope_kind KIND) {
        symbol_scopes_.push_back(ScopeFrame{KIND, {}});
    }
    void popScope() {
        if (symbol_scopes_.empty()) {
            throw std::runtime_error("symbol scope underflow");
        }
        symbol_scopes_.pop_back();
    }

    bool insertSymbol(const std::string& name, SymbolEntry& entry);

    [[nodiscard]] const SymbolEntry* lookupSymbol(const std::string& name) const {
        for (auto it = symbol_scopes_.rbegin(); it != symbol_scopes_.rend(); ++it) {
            auto find = it->symbols.find(name);
            if (find != it->symbols.end()) {
                return &find->second;
            }
        }
        return nullptr;
    }

    void registerBuiltinTypes();
    [[nodiscard]] TypeRef resolveType(const std::string& name) const;
    TypeRef makeTypeRefFrom(ClearLanguageParser::TypeContext* ctx);

    [[nodiscard]] std::string resolveFunctionName(const std::string& name) const;
    [[nodiscard]] const SymbolEntry* lookupFunctionSymbol(const std::string& name) const;
    [[nodiscard]] TypeRef resolveTypeSymbol(const std::string& name) const;
    [[nodiscard]] bool isAddressable(const std::shared_ptr<sema::Expr>& expr) noexcept;
    [[nodiscard]] bool inferMutability(const std::shared_ptr<sema::Expr>& expr);

    bool tryFoldExpr(const std::shared_ptr<sema::Expr>& expr, value_variant& out) const;

    [[nodiscard]] std::string qualify(const std::string& name) const {
        return current_package_.empty() ? name : current_package_ + "::" + name;
    }

    struct ScopeGuard {
        SemaBuilder& s_builder;
        bool active{true};
        scope_kind kind;
        ScopeGuard(ScopeGuard&&) = delete;
        ScopeGuard& operator=(ScopeGuard&&) = delete;
        explicit ScopeGuard(SemaBuilder& builder, scope_kind kind)
            : s_builder(builder), kind(kind) {
            s_builder.pushScope(kind);
        }
        ~ScopeGuard();
        void dismiss() {
            active = false;
        }
        ScopeGuard(const ScopeGuard&) = delete;
        ScopeGuard& operator=(const ScopeGuard&) = delete;
    };

    struct FileCtxGuard {
        SemaBuilder& s_builder;
        std::string prev_package;
        std::unordered_map<std::string, std::string> prev_imports;

        FileCtxGuard(FileCtxGuard&&) = delete;
        FileCtxGuard& operator=(FileCtxGuard&&) = delete;
        FileCtxGuard(SemaBuilder& builder, ClearLanguageParser::StartContext* ctx)
            : s_builder(builder), prev_package(builder.current_package_),
              prev_imports(builder.imports_) {
            if (auto* pkg = ctx->packageDecl()) {
                s_builder.current_package_ = pkg->qualifiedIdent()->getText();
            } else {
                s_builder.current_package_.clear();
            }

            s_builder.imports_.clear();
            for (auto* imp : ctx->importDecl()) {
                const std::string FULL = imp->qualifiedIdent()->getText();
                std::string alias = (imp->AS() != nullptr) ? imp->IDENT()->getText() : FULL;
                s_builder.imports_[alias] = FULL;
            }
        }

        ~FileCtxGuard() {
            s_builder.current_package_ = std::move(prev_package);
            s_builder.imports_ = std::move(prev_imports);
        }

        FileCtxGuard(const FileCtxGuard&) = delete;
        FileCtxGuard& operator=(const FileCtxGuard&) = delete;
    };

    [[nodiscard]] const SymbolEntry* lookupSymbolQualified(const std::string& name) const {
        const auto& qualified_name = name;
        auto fqn = qualify(qualified_name);

        if (!symbol_scopes_.empty()) {
            const auto& global = symbol_scopes_.front();
            auto iter = global.symbols.find(fqn);
            if (iter != global.symbols.end()) {
                return &iter->second;
            }
        }
        return lookupSymbol(name);
    }
};