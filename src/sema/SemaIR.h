#pragma once
#include <memory>
#include <string>
#include <vector>
#include "../core/CLType.h"

namespace sema {
    struct Node {virtual ~Node() = default;};

    struct Expr : Node {TypeRef type; virtual bool isConst() const {return false;}};

    struct Literal : Expr {
        Value value;
        bool isConst() const override {return true;}
    };

    struct VarRef : Expr { std::string name;};

    struct Cast : Expr {
        TypeRef targetType;
        std::shared_ptr<Expr> inner;
        bool isConst() const override { return inner->isConst(); }
	};

    struct BinOp : Expr {
        std::string op;
        std::shared_ptr<Expr> lhs;
        std::shared_ptr<Expr> rhs;
    };

    struct Unary : Expr {
        std::string op;
        std::shared_ptr<Expr> inner;
    };

    struct Call : Expr {
        std::string callee;
        std::vector<std::shared_ptr<Expr>> args;
    };

    struct Stmt  : Node {};
    struct StmtVarDecl : Stmt {
        std::string name;
        TypeRef declType;
        std::shared_ptr<Expr> initExpr; // may be null
    };

    struct StmtReturn : Stmt { std::shared_ptr<Expr> value; };

    struct StmtExpr : Stmt {
        std::shared_ptr<Expr> expr;
	};

    struct Block : Stmt {
        std::vector<std::shared_ptr<Stmt>> statements;
    };

    struct Param { std::string name; TypeRef type; };
    struct Function {
        std::string name;
        std::vector<Param> params;
        TypeRef returnType;
        std::shared_ptr<Block> body;
    };

    struct Module : Node {
        std::vector<std::shared_ptr<Function>> functions;
        std::string entryName; // optional; set when [EntryPoint] is present
    };

}  // namespace sema