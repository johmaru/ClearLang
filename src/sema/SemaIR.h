#pragma once
#include "../core/CLType.h"

#include <memory>
#include <string>
#include <vector>

namespace sema {

enum class mutability : std::uint8_t {
    CONST,
    LET,
    VAR,
};
struct Node {
    Node(const Node&) = default;
    Node(Node&&) = delete;
    Node() = default;
    Node& operator=(const Node&) = default;
    Node& operator=(Node&&) = delete;
    virtual ~Node() = default;
};

// For a error reporting
struct SourceLocation {
    std::string file_path;
    size_t line{0U};
    size_t column{0U};
};
struct Expr : Node {
    Expr(const Expr&) = default;
    Expr(Expr&&) = delete;
    Expr() = default;
    Expr& operator=(const Expr&) = default;
    Expr& operator=(Expr&&) = delete;
    TypeRef type;
    SourceLocation loc;
    [[nodiscard]] virtual bool isConst() const {
        return false;
    }
};

struct Literal : Expr {
    Value value;
    [[nodiscard]] bool isConst() const override {
        return true;
    }
};

struct VarRef : Expr {
    std::string name;
    sema::mutability mut{sema::mutability::CONST};
    [[nodiscard]] bool isMutable() const {
        return mut == sema::mutability::VAR;
    }
};

struct Cast : Expr {
    TypeRef target_type;
    std::shared_ptr<Expr> inner;
    [[nodiscard]] bool isConst() const override {
        return inner->isConst();
    }
};

struct BinOp : Expr {
    std::string op;
    std::shared_ptr<Expr> lhs;
    std::shared_ptr<Expr> rhs;
    [[nodiscard]] bool isConst() const override {
        return lhs && rhs && lhs->isConst() && rhs->isConst();
    }
};

enum class UnaryOpKind : std::uint8_t {
    REF,
    DEREF,
    NEGATE,
};
struct Unary : Expr {
    UnaryOpKind op;
    std::shared_ptr<Expr> inner;
    [[nodiscard]] bool isConst() const override {
        return inner && inner->isConst();
    }
};

struct Call : Expr {
    std::string callee;
    std::vector<std::shared_ptr<Expr>> args;
};

struct Stmt : Node {
    SourceLocation loc;
};
struct StmtVarDecl : Stmt {
    std::string name;
    TypeRef decl_type;
    std::shared_ptr<Expr> init_expr; // may be null
    mutability mut;
};

struct StmtReturn : Stmt {
    std::shared_ptr<Expr> value;
};

struct StmtExpr : Stmt {
    std::shared_ptr<Expr> expr;
};

struct Block : Stmt {
    std::vector<std::shared_ptr<Stmt>> statements;
};

struct StmtIf : Stmt {
    std::shared_ptr<Expr> cond;
    std::shared_ptr<Block> then_blk;
    std::shared_ptr<Block> else_blk;
};

struct Param {
    std::string name;
    TypeRef type;
};
struct Function {
    std::string name;
    std::vector<Param> params;
    TypeRef return_type;
    std::shared_ptr<Block> body;
};

struct Module : Node {
    std::vector<std::shared_ptr<Function>> functions;
    std::string entry_name; // optional; set when [EntryPoint] is present
};

} // namespace sema
