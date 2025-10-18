#include "SemaBuilder.h"

#include <llvm/ADT/APFloat.h>

#include <any>
#include <memory>
#include <stdexcept>
#include <string>

#include "../core/CLType.h"
#include "../core/SemaUtils.h"
#include "ClearLanguageParser.h"
#include "SemaIR.h"

using sema::bin_op;
using sema::block;
using sema::expr;
using sema::function;
using sema::literal;
using sema::stmt_return;
using sema::stmt_var_decl;
using sema::unary;
using sema::var_ref;

namespace {
bool is_int_kind(const type::kind_enum k) {
  switch (k) {
    case type::kind_enum::i8:
    case type::kind_enum::u8:
    case type::kind_enum::i16:
    case type::kind_enum::u16:
    case type::kind_enum::i32:
    case type::kind_enum::u32:
    case type::kind_enum::i64:
    case type::kind_enum::u64:
      return true;
    default:
      return false;
  }
}

bool is_bool_kind(const type::kind_enum k) {
  switch (k) {
    case type::kind_enum::boolean:
      return true;

    default:
      return false;
  }
}

bool is_num_kind(const type::kind_enum k) {
  switch (k) {
    case type::kind_enum::i8:
    case type::kind_enum::u8:
    case type::kind_enum::i16:
    case type::kind_enum::u16:
    case type::kind_enum::i32:
    case type::kind_enum::u32:
    case type::kind_enum::i64:
    case type::kind_enum::u64:
    case type::kind_enum::f16:
    case type::kind_enum::f32:
      return true;
    default:
      return false;
  }
}

type_ref boolean_type() {
  return type_ref::builtin_type(type{type::kind_enum::boolean});
}
}  // namespace

sema_builder::sema_builder() : mod_(std::make_shared<sema::module>()) {
  push_scope(scope_kind::global);
  register_builtin_types();

  auto add_builtin_function = [&](const char* name,
                                  std::initializer_list<type::kind_enum> params,
                                  type::kind_enum ret) {
    auto sig = std::make_shared<function_sig>();
    for (auto k : params)
      sig->param_types.push_back(type_ref::builtin_type(type{k}));
    sig->return_type =
        std::make_shared<type_ref>(type_ref::builtin_type(type{ret}));

    symbol_entry e;
    e.kind = symbol_kind::function;
    e.function_sig = sig;
    e.type = type_ref::function_type(sig);
    insert_symbol(name, e);
  };

  // Signatures of built-in functions

  add_builtin_function("_cl_printf", {type::kind_enum::string},
                       type::kind_enum::unit);

  add_builtin_function("__cl_i8_printf", {type::kind_enum::i8},
                       type::kind_enum::unit);

  add_builtin_function("__cl_i8_printfn", {type::kind_enum::i8},
                       type::kind_enum::unit);

  add_builtin_function("__cl_u8_printfn", {type::kind_enum::u8},
                       type::kind_enum::unit);

  add_builtin_function("__cl_i16_printfn", {type::kind_enum::i16},
                       type::kind_enum::unit);

  add_builtin_function("__cl_f16_printfn", {type::kind_enum::f16},
                       type::kind_enum::unit);

  add_builtin_function("__cl_f32_printfn", {type::kind_enum::f32},
                       type::kind_enum::unit);

  {
    auto add_parse = [&](const char* name, type::kind_enum k) {
      const auto sig = std::make_shared<function_sig>();
      sig->param_types.push_back(
          type_ref::builtin_type(type{type::kind_enum::string}));
      sig->return_type =
          std::make_shared<type_ref>(type_ref::builtin_type(type{k}));
      symbol_entry e;
      e.kind = symbol_kind::function;
      e.function_sig = sig;
      e.type = type_ref::function_type(sig);
      insert_symbol(name, e);
    };
    add_parse("__cl_parse_i8", type::kind_enum::i8);
    add_parse("__cl_parse_u8", type::kind_enum::u8);
    add_parse("__cl_parse_i16", type::kind_enum::i16);
    add_parse("__cl_parse_u16", type::kind_enum::u16);
    add_parse("__cl_parse_i32", type::kind_enum::i32);
    add_parse("__cl_parse_u32", type::kind_enum::u32);
    add_parse("__cl_parse_i64", type::kind_enum::i64);
    add_parse("__cl_parse_u64", type::kind_enum::u64);
  }

  add_builtin_function("__set_entry", {type::kind_enum::string},
                       type::kind_enum::unit);
  add_builtin_function("__add_source", {type::kind_enum::string},
                       type::kind_enum::unit);
  add_builtin_function("__set_output", {type::kind_enum::string},
                       type::kind_enum::unit);
  add_builtin_function("__set_target", {type::kind_enum::string},
                       type::kind_enum::unit);
  add_builtin_function("__set_app_name", {type::kind_enum::string},
                       type::kind_enum::unit);
}

bool sema_builder::try_fold_expr(const std::shared_ptr<sema::expr>& e,
                                 value_variant& out) const {
  using kind = type::kind_enum;
  if (auto lit = std::dynamic_pointer_cast<sema::literal>(e)) {
    if (lit->type.is_builtin()) {
      switch (lit->type.builtin.kind) {
        case kind::i8:
        case kind::i16:
        case kind::i32:
        case kind::i64: {
          if (std::holds_alternative<int64_t>(lit->value.v)) {
            out = static_cast<int64_t>(std::get<int64_t>(lit->value.v));
            return true;
          }
          if (std::holds_alternative<uint64_t>(lit->value.v)) {
            out = static_cast<uint64_t>(std::get<uint64_t>(lit->value.v));
            return true;
          }
          break;
        }
        case kind::u8:
        case kind::u16:
        case kind::u32:
        case kind::u64: {
          if (std::holds_alternative<uint64_t>(lit->value.v)) {
            out = std::get<uint64_t>(lit->value.v);
            return true;
          }
          if (std::holds_alternative<int64_t>(lit->value.v)) {
            out = static_cast<uint64_t>(std::get<int64_t>(lit->value.v));
            return true;
          }
          break;
        }
        case kind::f16:
        case kind::f32: {
          if (std::holds_alternative<cl_f32>(lit->value.v)) {
            cl_f32 h = std::get<cl_f32>(lit->value.v);
            float f;
            static_assert(sizeof(float) == 4, "expected IEEE single");
            uint32_t bits = h.bits;
            std::memcpy(&f, &bits, sizeof(float));
            out = static_cast<double>(f);
            return true;
          }
          break;
        }
        case kind::boolean: {
          if (std::holds_alternative<int64_t>(lit->value.v)) {
            out = (std::get<int64_t>(lit->value.v) != 0);
            return true;
          }
          break;
        }
        case kind::string:
        case kind::unit:
        case kind::noreturn:
          break;
      }
    }
    return false;
  }
  if (auto un = std::dynamic_pointer_cast<sema::unary>(e)) {
    value_variant inner;
    if (!try_fold_expr(un->inner, inner)) return false;
    if (un->op == "-") {
      if (auto* pi = std::get_if<int64_t>(&inner)) {
        out = -(*pi);
        return true;
      }
      if (auto* pu = std::get_if<uint64_t>(&inner)) {
        out = static_cast<int64_t>(-static_cast<int64_t>(*pu));
        return true;
      }
      if (auto* pd = std::get_if<double>(&inner)) {
        out = -(*pd);
        return true;
      }
    }
    return false;
  }
  if (auto bin = std::dynamic_pointer_cast<sema::bin_op>(e)) {
    value_variant lv, rv;
    if (!try_fold_expr(bin->lhs, lv) || !try_fold_expr(bin->rhs, rv))
      return false;
    auto op = bin->op;
    auto getd = [](const value_variant& v) -> double {
      if (const auto* const pd = std::get_if<double>(&v)) return *pd;
      if (const auto* const pi = std::get_if<int64_t>(&v))
        return static_cast<double>(*pi);
      if (const auto* const pu = std::get_if<uint64_t>(&v))
        return static_cast<double>(*pu);
      return 0.0;
    };
    auto geti = [](const value_variant& v) -> int64_t {
      if (const auto* const pi = std::get_if<int64_t>(&v)) return *pi;
      if (const auto* const pu = std::get_if<uint64_t>(&v))
        return static_cast<int64_t>(*pu);
      return 0;
    };

    bool use_double = std::holds_alternative<double>(lv) ||
                      std::holds_alternative<double>(rv);
    if (use_double) {
      double l = getd(lv), r = getd(rv);
      if (op == "+")
        out = l + r;
      else if (op == "-")
        out = l - r;
      else if (op == "*")
        out = l * r;
      else if (op == "/")
        out = l / r;
      else
        return false;
      return true;
    }
    int64_t l = geti(lv), r = geti(rv);
    if (op == "+")
      out = static_cast<int64_t>(l + r);
    else if (op == "-")
      out = static_cast<int64_t>(l - r);
    else if (op == "*")
      out = static_cast<int64_t>(l * r);
    else if (op == "/") {
      if (r == 0) return false;
      out = static_cast<int64_t>(l / r);
    } else if (op == "%") {
      if (r == 0) return false;
      out = static_cast<int64_t>(l % r);
    } else
      return false;
    return true;
  }
  return false;
}

void sema_builder::register_builtin_types() {
  static const std::pair<const char*, type::kind_enum> builtins[] = {
      {"i8", type::kind_enum::i8},
      {"u8", type::kind_enum::u8},
      {"i16", type::kind_enum::i16},
      {"u16", type::kind_enum::u16},
      {"i32", type::kind_enum::i32},
      {"int", type::kind_enum::i32},
      {"u32", type::kind_enum::u32},
      {"i64", type::kind_enum::i64},
      {"u64", type::kind_enum::u64},
      {"f16", type::kind_enum::f16},
      {"f32", type::kind_enum::f32},
      {"noreturn", type::kind_enum::noreturn},
      {"unit", type::kind_enum::unit},
      {"()", type::kind_enum::unit},
      {"string", type::kind_enum::string},
      {"bool", type::kind_enum::boolean}};
  for (auto& b : builtins) {
    symbol_entry e;
    e.kind = symbol_kind::type_name;
    e.type = type_ref::builtin_type(type{b.second});
    insert_symbol(b.first, e);
  }
}

std::shared_ptr<sema::module> sema_builder::take_module() {
  return std::move(mod_);
}

std::string sema_builder::resolve_function_name(const std::string& name) const {
  if (name.find(DOUBLE_COLON) != std::string::npos) {
    if (auto* se = lookup_symbol(name); se && se->kind == symbol_kind::function)
      return name;

    throw std::runtime_error("undefined function (qualified): " + name);
  }

  if (auto* se = lookup_symbol(name); se && se->kind == symbol_kind::function)
    return name;

  if (!current_package_.empty()) {
    std::string q = qualify(name);
    if (auto* se = lookup_symbol(q); se && se->kind == symbol_kind::function)
      return q;
  }

  throw std::runtime_error("undefined function: " + name);
}

const sema_builder::symbol_entry* sema_builder::lookup_function_symbol(
    const std::string& name) const {
  if (auto* se = lookup_symbol(name); se && se->kind == symbol_kind::function)
    return se;

  if (!current_package_.empty()) {
    std::string q = current_package_ + DOUBLE_COLON + name;
    if (auto* se = lookup_symbol(q); se && se->kind == symbol_kind::function)
      return se;
  }
  for (auto& kv : imports_) {
    std::string cand = kv.second + DOUBLE_COLON + name;
    if (auto* se = lookup_symbol(cand); se && se->kind == symbol_kind::function)
      return se;

    std::string alias_cand = kv.first + DOUBLE_COLON + name;
    if (auto* se2 = lookup_symbol(alias_cand);
        se2 && se2->kind == symbol_kind::function)
      return se2;
  }
  return nullptr;
}

bool sema_builder::insert_symbol(const std::string& name, symbol_entry& entry) {
  if (symbol_scopes_.empty()) push_scope(scope_kind::global);
  auto& cur = symbol_scopes_.back().symbols;
  if (cur.count(name)) return false;
  cur.emplace(name, entry);
  return true;
}

type_ref sema_builder::resolve_type_symbol(const std::string& name) const {
  if (auto* se = lookup_symbol(name)) {
    if (se->kind == symbol_kind::type_name) return se->type;
  }
  throw std::runtime_error("unknown type: " + name);
}

type_ref sema_builder::resolve_type(const std::string& name) const {
  if (auto* se = lookup_symbol(name);
      se && se->kind == symbol_kind::type_name) {
    return se->type;
  }

  if (!current_package_.empty()) {
    std::string q = current_package_ + DOUBLE_COLON + name;
    if (auto* se = lookup_symbol(q); se && se->kind == symbol_kind::type_name) {
      return se->type;
    }
  }

  for (const auto& kv : imports_) {
    std::string cand = kv.second + DOUBLE_COLON + name;
    if (auto* se = lookup_symbol(cand);
        se && se->kind == symbol_kind::type_name) {
      return se->type;
    }
    std::string alias_cand = kv.first + DOUBLE_COLON + name;
    if (auto* se2 = lookup_symbol(alias_cand);
        se2 && se2->kind == symbol_kind::type_name) {
      return se2->type;
    }
  }

  try {
    return type_ref::builtin_type(type::from_string(name));
  } catch (...) {
  }

  throw std::runtime_error("unknown type: " + name);
}

type_ref sema_builder::make_type_ref_from(
    ClearLanguageParser::TypeContext* ctx) {
  if (auto* const nt =
          dynamic_cast<ClearLanguageParser::NamedTypeContext*>(ctx)) {
    return resolve_type(nt->IDENT()->getText());
  }
  if (dynamic_cast<ClearLanguageParser::UnitTypeContext*>(ctx)) {
    return type_ref::builtin_type(type{type::kind_enum::unit});
  }
  if (auto* const ft =
          dynamic_cast<ClearLanguageParser::FunctionTypeContext*>(ctx)) {
    auto sig = std::make_shared<function_sig>();
    if (auto* const tl = ft->typeList()) {
      for (auto* tctx : tl->type())
        sig->param_types.push_back(make_type_ref_from(tctx));
    }
    sig->return_type =
        std::make_shared<type_ref>(make_type_ref_from(ft->type()));
    return type_ref::function_type(std::move(sig));
  }
  throw std::runtime_error("unknown type alt");
}

void sema_builder::collect_signatures(ClearLanguageParser::StartContext* ctx) {
  file_ctx_guard g(*this, ctx);

  for (auto* cd : ctx->constantDecl()) {
    auto name = cd->IDENT()->getText();
    std::string qualified_name = qualify(name);
  }

  for (auto* cd : ctx->constantDecl()) {
    // Currently not implement
  }

  for (auto* fd : ctx->funcDecl()) {
    std::string qualified_name = qualify(fd->name->getText());

    const auto sig = std::make_shared<function_sig>();
    if (auto* const pl = fd->paramList()) {
      for (auto* p : pl->param()) {
        sig->param_types.push_back(make_type_ref_from(p->type()));
      }
    }
    sig->return_type =
        std::make_shared<type_ref>(make_type_ref_from(fd->type()));

    symbol_entry e;
    e.kind = symbol_kind::function;
    e.function_sig = sig;
    e.type = type_ref::function_type(sig);
    if (!insert_symbol(qualified_name, e))
      throw std::runtime_error("function redeclaration: " + qualified_name);

    // pick up entry point name if any
    for (auto* at : fd->attributes()) {
      for (auto* id : at->IDENT()) {
        if (id->getText() == "EntryPoint") {
          mod_->entry_name = qualified_name;
        }
      }
    }
  }
}

void sema_builder::construct_target(ClearLanguageParser::StartContext* ctx) {
  file_ctx_guard g(*this, ctx);

  for (auto* fd : ctx->funcDecl()) {
    auto func = std::make_shared<function>();
    func->name = qualify(fd->name->getText());

    if (auto* const pl = fd->paramList()) {
      for (auto* p : pl->param()) {
        sema::param prm;
        prm.name = p->IDENT()->getText();
        prm.type = make_type_ref_from(p->type());
        func->params.push_back(std::move(prm));
      }
    }
    func->return_type = make_type_ref_from(fd->type());
    func->body = std::make_shared<block>();

    current_return_type_ = func->return_type;

    scope_guard g(*this, scope_kind::function);

    for (auto& prm : func->params) {
      symbol_entry v;
      v.kind = symbol_kind::variable;
      v.type = prm.type;
      v.is_mutable = true;
      if (!insert_symbol(prm.name, v))
        throw std::runtime_error("parameter redeclaration: " + prm.name);
    }

    auto any_blk = visit(fd->block());
    *func->body = *std::any_cast<std::shared_ptr<block>>(any_blk);

    mod_->functions.push_back(std::move(func));
  }
}

std::any sema_builder::visitIntLiteral(
    ClearLanguageParser::IntLiteralContext* ctx) {
  const auto node = std::make_shared<sema::literal>();
  node->type = type_ref::builtin_type(type{type::kind_enum::i32});
  int64_t v = std::stoll(ctx->INT()->getText());
  node->value = value{node->type, v, true};
  return std::static_pointer_cast<expr>(node);
}

std::any sema_builder::visitFloatLiteral(
    ClearLanguageParser::FloatLiteralContext* ctx) {
  const auto node = std::make_shared<literal>();
  node->type = type_ref::builtin_type(type{type::kind_enum::f32});
  const std::string tok = ctx->FLOAT()->getText();
  const llvm::APFloat ap(llvm::APFloat::IEEEsingle(), tok);
  if (ap.isInfinity()) throw std::runtime_error("f32: Out of range" + tok);
  const uint32_t bits =
      static_cast<uint32_t>(ap.bitcastToAPInt().getZExtValue());
  cl_f32 h;
  h.bits = bits;

  node->value = value(node->type, h, false);
  return std::static_pointer_cast<expr>(node);
}

std::any sema_builder::visitUnaryMinus(
    ClearLanguageParser::UnaryMinusContext* ctx) {
  const auto inner = std::any_cast<std::shared_ptr<expr>>(visit(ctx->inner));

  const auto node = std::make_shared<unary>();
  node->op = "-";
  node->inner = inner;
  node->type = inner->type;
  return std::static_pointer_cast<expr>(node);
}

std::any sema_builder::visitOrExpr(ClearLanguageParser::OrExprContext* ctx) {
  auto cur = std::any_cast<std::shared_ptr<expr>>(visit(ctx->left));

  if (ctx->right.empty()) return cur;

  if (!(cur->type.is_builtin() && is_int_kind(cur->type.builtin.kind)))
    throw std::runtime_error("operator 'or' expects integer operands (left)");

  for (size_t i = 0; i < ctx->right.size(); i++) {
    const auto rhs = std::any_cast<std::shared_ptr<expr>>(visit(ctx->right[i]));
    if (!(rhs->type.is_builtin() && is_int_kind(rhs->type.builtin.kind)))
      throw std::runtime_error("operator or expects integer operands (right)");

    const auto node = std::make_shared<bin_op>();
    node->op = "or";
    node->lhs = cur;
    node->rhs = rhs;
    node->type = boolean_type();
    cur = node;
  }
  return cur;
}

std::any sema_builder::visitAndExpr(ClearLanguageParser::AndExprContext* ctx) {
  auto cur = std::any_cast<std::shared_ptr<expr>>(visit(ctx->left));

  if (ctx->right.empty()) return cur;

  if (!(cur->type.is_builtin() && is_int_kind(cur->type.builtin.kind)))
    throw std::runtime_error("operator 'and' expects integer operands (left)");

  for (size_t i = 0; i < ctx->right.size(); i++) {
    const auto rhs = std::any_cast<std::shared_ptr<expr>>(visit(ctx->right[i]));
    if (!(rhs->type.is_builtin() && is_int_kind(rhs->type.builtin.kind)))
      throw std::runtime_error(
          "operator 'and' expects integer operands (right)");

    const auto node = std::make_shared<bin_op>();
    node->op = "and";
    node->lhs = cur;
    node->rhs = rhs;
    node->type = boolean_type();
    cur = node;
  }
  return cur;
}

std::any sema_builder::visitEqualExpr(
    ClearLanguageParser::EqualExprContext* ctx) {
  auto cur = std::any_cast<std::shared_ptr<expr>>(visit(ctx->left));

  for (size_t i = 0; i < ctx->right.size(); i++) {
    const auto rhs = std::any_cast<std::shared_ptr<expr>>(visit(ctx->right[i]));
    const std::string op = ctx->op[i]->getText();
    if (!(cur->type.is_builtin() && rhs->type.is_builtin()))
      throw std::runtime_error("equal operator requires builtin types");
    const auto lk = cur->type.builtin.kind;
    const auto rk = rhs->type.builtin.kind;

    if (!(is_num_kind(lk) && lk == rk))
      throw std::runtime_error(
          "type mismatch in equal op (only num same types)");

    const auto node = std::make_shared<bin_op>();
    node->op = op;
    node->lhs = cur;
    node->rhs = rhs;
    node->type = boolean_type();
    cur = node;
  }
  return cur;
}

std::any sema_builder::visitAddExpr(ClearLanguageParser::AddExprContext* ctx) {
  auto cur = std::any_cast<std::shared_ptr<expr>>(visit(ctx->left));
  for (size_t i = 0; i < ctx->right.size(); ++i) {
    const auto rhs = std::any_cast<std::shared_ptr<expr>>(visit(ctx->right[i]));
    std::string op = ctx->op[i]->getText();
    if (!(cur->type.is_builtin() && rhs->type.is_builtin() &&
          cur->type.builtin.kind == rhs->type.builtin.kind)) {
      throw std::runtime_error("type mismatch in binary op");
    }
    if (is_string(cur->type) && is_string(rhs->type)) {
      if (op != "+") {
        throw std::runtime_error(
            "only + operator is supported for string concatenation");
      }
    }
    const auto bin = std::make_shared<bin_op>();
    bin->op = op;
    bin->lhs = cur;
    bin->rhs = rhs;
    bin->type = cur->type;
    cur = bin;
  }
  return cur;
}

std::any sema_builder::visitMulExpr(ClearLanguageParser::MulExprContext* ctx) {
  auto cur = std::any_cast<std::shared_ptr<expr>>(visit(ctx->left));
  for (size_t i = 0; i < ctx->right.size(); ++i) {
    const auto rhs = std::any_cast<std::shared_ptr<expr>>(visit(ctx->right[i]));
    const std::string op = ctx->op[i]->getText();
    if (!(cur->type.is_builtin() && rhs->type.is_builtin() &&
          cur->type.builtin.kind == rhs->type.builtin.kind)) {
      throw std::runtime_error("type mismatch in binary op");
    }
    const auto bin = std::make_shared<bin_op>();
    bin->op = op;
    bin->lhs = cur;
    bin->rhs = rhs;
    bin->type = cur->type;
    cur = bin;
  }
  return cur;
}

std::any sema_builder::visitVarRef(ClearLanguageParser::VarRefContext* ctx) {
  std::string name;
  if (auto* q = ctx->qualifiedIdent())
    name = q->getText();
  else
    name = ctx->getText();

  if (auto* se = lookup_symbol(name)) {
    switch (se->kind) {
      case symbol_kind::variable: {
        auto vr = std::make_shared<var_ref>();
        vr->name = name;
        vr->type = se->type;
        return std::static_pointer_cast<expr>(vr);
      }
      case symbol_kind::function: {
        // fallback
        break;
      }
      case symbol_kind::constant: {
        if (!std::holds_alternative<std::monostate>(se->value)) {
          const auto lit = std::make_shared<literal>();
          lit->type = se->type;

          if (const auto* const pi = std::get_if<int64_t>(&se->value))
            lit->value = value{lit->type, *pi, false};
          else if (const auto* const pu = std::get_if<uint64_t>(&se->value))
            lit->value = value{lit->type, *pu, false};
          else if (const auto* const pb = std::get_if<bool>(&se->value))
            lit->value =
                value{lit->type, static_cast<int64_t>(*pb ? 1 : 0), false};
          else if (const auto* const pd = std::get_if<double>(&se->value)) {
            const float f = static_cast<float>(*pd);
            cl_f32 bits;
            std::memcpy(&bits, &f, sizeof(float));
            lit->value = value{lit->type, bits, false};
          } else if (const auto* const ps =
                         std::get_if<std::string>(&se->value)) {
            lit->value = make_string(*ps);
          }
          return std::static_pointer_cast<expr>(lit);
        }
        return se->const_expr;
      }
      case symbol_kind::type_name:
        throw std::runtime_error("type name used as value: " + name);
    }
  }

  try {
    const std::string fqn = resolve_function_name(name);
    auto* fse = lookup_symbol(fqn);

    if (!fse || fse->kind != symbol_kind::function)
      throw std::runtime_error("function symbol disappeared: " + fqn);

    const auto vr = std::make_shared<var_ref>();
    vr->name = fqn;
    vr->type = fse->type;
    return std::static_pointer_cast<expr>(vr);

  } catch (...) {
  }
  throw std::runtime_error("undefined identifier: " + name);
}

std::any sema_builder::visitBlock(ClearLanguageParser::BlockContext* ctx) {
  auto blk = std::make_shared<block>();
  scope_guard g(*this, scope_kind::block);

  for (auto* s : ctx->stmt()) {
    if (auto* sr = dynamic_cast<ClearLanguageParser::StmtReturnContext*>(s)) {
      blk->statements.push_back(
          std::any_cast<std::shared_ptr<stmt_return>>(visit(sr)));
    } else if (auto* vd =
                   dynamic_cast<ClearLanguageParser::StmtVarDeclContext*>(s)) {
      blk->statements.push_back(
          std::any_cast<std::shared_ptr<stmt_var_decl>>(visit(vd)));
    } else if (auto* se =
                   dynamic_cast<ClearLanguageParser::StmtExprContext*>(s)) {
      blk->statements.push_back(
          std::any_cast<std::shared_ptr<sema::stmt_expr>>(visit(se)));
    } else if (auto* si =
                   dynamic_cast<ClearLanguageParser::StmtIfContext*>(s)) {
      blk->statements.push_back(
          std::any_cast<std::shared_ptr<sema::stmt_if>>(visit(si)));
    } else {
      // currently pass expression statements
      visit(s);
    }
  }
  return blk;
}

std::any sema_builder::visitIfBlock(ClearLanguageParser::IfBlockContext* ctx) {
  const auto cond = std::any_cast<std::shared_ptr<expr>>(visit(ctx->expr()));

  const bool is_bool =
      cond->type.is_builtin() && is_bool_kind(cond->type.builtin.kind);
  const bool is_int =
      cond->type.is_builtin() && is_int_kind(cond->type.builtin.kind);

  if (!(is_bool || is_int))
    throw std::runtime_error("if condition must be boolean or int expression");

  auto node = std::make_shared<sema::stmt_if>();
  node->cond = cond;
  node->then_blk = std::any_cast<std::shared_ptr<block>>(visit(ctx->block(0)));

  node->else_blk = nullptr;
  const auto blks = ctx->block();
  auto* else_stmt_ctx = ctx->stmt();
  if (blks.size() >= 2) {
    node->else_blk =
        std::any_cast<std::shared_ptr<block>>(visit(ctx->block(1)));
  } else if (else_stmt_ctx != nullptr) {
    const auto else_blk = std::make_shared<block>();
    if (auto* sr = dynamic_cast<ClearLanguageParser::StmtReturnContext*>(
            else_stmt_ctx)) {
      else_blk->statements.push_back(
          std::any_cast<std::shared_ptr<stmt_return>>(visit(sr)));
    } else if (auto* vd =
                   dynamic_cast<ClearLanguageParser::StmtVarDeclContext*>(
                       else_stmt_ctx)) {
      else_blk->statements.push_back(
          std::any_cast<std::shared_ptr<stmt_var_decl>>(visit(vd)));
    } else if (auto* se = dynamic_cast<ClearLanguageParser::StmtExprContext*>(
                   else_stmt_ctx)) {
      else_blk->statements.push_back(
          std::any_cast<std::shared_ptr<sema::stmt_expr>>(visit(se)));
    } else if (auto* si = dynamic_cast<ClearLanguageParser::StmtIfContext*>(
                   else_stmt_ctx)) {
      else_blk->statements.push_back(
          std::any_cast<std::shared_ptr<sema::stmt_if>>(visit(si)));
    } else {
      visit(else_stmt_ctx);
    }
    node->else_blk = else_blk;
  }

  return node;
}

std::any sema_builder::visitIfSingle(
    ClearLanguageParser::IfSingleContext* ctx) {
  const auto cond = std::any_cast<std::shared_ptr<expr>>(visit(ctx->expr()));

  const bool is_bool =
      cond->type.is_builtin() && is_bool_kind(cond->type.builtin.kind);
  const bool is_int =
      cond->type.is_builtin() && is_int_kind(cond->type.builtin.kind);

  if (!(is_bool || is_int))
    throw std::runtime_error("if condition must be boolean or int expression");

  auto node = std::make_shared<sema::stmt_if>();
  node->cond = cond;

  const auto then_blk = std::make_shared<block>();
  auto* then_stmt_ctx = ctx->stmt(0);
  if (auto* sr = dynamic_cast<ClearLanguageParser::StmtReturnContext*>(
          then_stmt_ctx)) {
    then_blk->statements.push_back(
        std::any_cast<std::shared_ptr<stmt_return>>(visit(sr)));
  } else if (auto* vd = dynamic_cast<ClearLanguageParser::StmtVarDeclContext*>(
                 then_stmt_ctx)) {
    then_blk->statements.push_back(
        std::any_cast<std::shared_ptr<stmt_var_decl>>(visit(vd)));
  } else if (auto* se = dynamic_cast<ClearLanguageParser::StmtExprContext*>(
                 then_stmt_ctx)) {
    then_blk->statements.push_back(
        std::any_cast<std::shared_ptr<sema::stmt_expr>>(visit(se)));
  } else if (auto* si = dynamic_cast<ClearLanguageParser::StmtIfContext*>(
                 then_stmt_ctx)) {
    then_blk->statements.push_back(
        std::any_cast<std::shared_ptr<sema::stmt_if>>(visit(si)));
  } else {
    visit(then_stmt_ctx);
  }
  node->then_blk = then_blk;

  node->else_blk = nullptr;
  const auto& blks = ctx->getRuleContexts<ClearLanguageParser::BlockContext>();
  const auto& stmts_vec = ctx->stmt();
  if (!blks.empty()) {
    node->else_blk = std::any_cast<std::shared_ptr<block>>(visit(blks[0]));
  } else if (stmts_vec.size() >= 2) {
    const auto else_blk = std::make_shared<block>();
    auto* else_stmt_ctx = stmts_vec[1];
    if (auto* sr = dynamic_cast<ClearLanguageParser::StmtReturnContext*>(
            else_stmt_ctx)) {
      else_blk->statements.push_back(
          std::any_cast<std::shared_ptr<stmt_return>>(visit(sr)));
    } else if (auto* vd =
                   dynamic_cast<ClearLanguageParser::StmtVarDeclContext*>(
                       else_stmt_ctx)) {
      else_blk->statements.push_back(
          std::any_cast<std::shared_ptr<stmt_var_decl>>(visit(vd)));
    } else if (auto* se = dynamic_cast<ClearLanguageParser::StmtExprContext*>(
                   else_stmt_ctx)) {
      else_blk->statements.push_back(
          std::any_cast<std::shared_ptr<sema::stmt_expr>>(visit(se)));
    } else if (auto* si = dynamic_cast<ClearLanguageParser::StmtIfContext*>(
                   else_stmt_ctx)) {
      else_blk->statements.push_back(
          std::any_cast<std::shared_ptr<sema::stmt_if>>(visit(si)));
    } else {
      visit(else_stmt_ctx);
    }
    node->else_blk = else_blk;
  }

  return node;
}

std::any sema_builder::visitStmtVarDecl(
    ClearLanguageParser::StmtVarDeclContext* ctx) {
  auto* vd = ctx->varDecl();
  auto node = std::make_shared<stmt_var_decl>();
  node->name = vd->IDENT()->getText();
  node->decl_type = make_type_ref_from(vd->type());

  std::shared_ptr<expr> init_expr;
  if (vd->expr()) {
    init_expr = std::any_cast<std::shared_ptr<expr>>(visit(vd->expr()));
    if (init_expr->type.is_builtin() &&
        init_expr->type.builtin.kind == type::kind_enum::i32) {
      if (const auto lit = std::dynamic_pointer_cast<literal>(init_expr)) {
        if (lit->value.is_untyped_int) {
          const auto coerced =
              sema_utils::coerce_untyped_int_to(lit->value, node->decl_type);
          lit->value = coerced;
          lit->type = node->decl_type;
          init_expr->type = node->decl_type;
        }
      }
    }
    if (init_expr->type.is_builtin() &&
        init_expr->type.builtin.kind != node->decl_type.builtin.kind) {
      throw std::runtime_error("type mismatch in var init");
    }
  }
  node->init_expr = init_expr;

  symbol_entry e;
  e.kind = symbol_kind::variable;
  e.type = node->decl_type;
  e.is_mutable = true;
  if (!insert_symbol(node->name, e))
    throw std::runtime_error("redefinition in same scope: " + node->name);
  return node;
}

std::any sema_builder::visitConstantDecl(
    ClearLanguageParser::ConstantDeclContext* context) {
  throw std::runtime_error("not implement");
}

std::any sema_builder::visitStmtReturn(
    ClearLanguageParser::StmtReturnContext* ctx) {
  auto node = std::make_shared<stmt_return>();
  if (ctx->expr()) {
    node->value = std::any_cast<std::shared_ptr<expr>>(visit(ctx->expr()));
  }
  return node;
}

std::any sema_builder::visitStmtExpr(
    ClearLanguageParser::StmtExprContext* ctx) {
  auto node = std::make_shared<sema::stmt_expr>();
  node->expr = std::any_cast<std::shared_ptr<expr>>(visit(ctx->expr()));
  return node;
}

std::any sema_builder::visitUnaryPrimary(
    ClearLanguageParser::UnaryPrimaryContext* ctx) {
  // primary -> int/float/ident/paren/unit
  return visit(ctx->postfixExpr());
}

std::any sema_builder::visitPostfixExpr(
    ClearLanguageParser::PostfixExprContext* ctx) {
  auto cur = std::any_cast<std::shared_ptr<expr>>(visit(ctx->primary()));

  auto parse_func_for = [](const type::kind_enum k) -> const char* {
    switch (k) {
      case type::kind_enum::i8:
        return "__cl_parse_i8";
      case type::kind_enum::u8:
        return "__cl_parse_u8";
      case type::kind_enum::i16:
        return "__cl_parse_i16";
      case type::kind_enum::u16:
        return "__cl_parse_u16";
      case type::kind_enum::i32:
        return "__cl_parse_i32";
      case type::kind_enum::u32:
        return "__cl_parse_u32";
      case type::kind_enum::i64:
        return "__cl_parse_i64";
      case type::kind_enum::u64:
        return "__cl_parse_u64";
      case type::kind_enum::string:
      case type::kind_enum::f16:
      case type::kind_enum::f32:
      case type::kind_enum::noreturn:
      case type::kind_enum::unit:
        return nullptr;
    }
    return nullptr;
  };

  for (auto* child : ctx->children) {
    if (auto* cs =
            dynamic_cast<ClearLanguageParser::CallSuffixContext*>(child)) {
      const auto vr = std::dynamic_pointer_cast<var_ref>(cur);
      if (!vr) throw std::runtime_error("can only call functions by name");

      std::string callee_fqn = resolve_function_name(vr->name);
      auto* fse = lookup_symbol(callee_fqn);
      if (!fse || fse->kind != symbol_kind::function)
        throw std::runtime_error("call to undefined function: " + vr->name);

      const auto& sig = fse->function_sig;

      const auto call = std::make_shared<sema::call>();
      call->callee = callee_fqn;

      if (auto* const al = cs->argList()) {
        for (auto* ectx : al->expr()) {
          call->args.push_back(
              std::any_cast<std::shared_ptr<expr>>(visit(ectx)));
        }
      }

      if (sig->param_types.size() != call->args.size())
        throw std::runtime_error("argument count mismatch in function call: " +
                                 call->callee);

      for (size_t i = 0; i < call->args.size(); ++i) {
        if (!(call->args[i]->type.is_builtin() &&
              sig->param_types[i].is_builtin() &&
              call->args[i]->type.builtin.kind ==
                  sig->param_types[i].builtin.kind)) {
          throw std::runtime_error("argument type mismatch in function call: " +
                                   call->callee);
        }
      }
      call->type = *sig->return_type;

      cur = call;
      continue;
    }

    if (auto* as = dynamic_cast<ClearLanguageParser::AsSuffixContext*>(child)) {
      type_ref target = make_type_ref_from(as->type());

      if (const auto lit = std::dynamic_pointer_cast<literal>(cur)) {
        if (lit->value.is_untyped_int) {
          const auto coerced =
              sema_utils::coerce_untyped_int_to(lit->value, target);
          lit->value = coerced;
          lit->type = target;
          cur->type = target;
          continue;
        }
      }

      if (!(cur->type.is_builtin() && target.is_builtin())) {
        throw std::runtime_error("can only cast between builtin types");
      }

      const auto src_k = cur->type.builtin.kind;
      const auto dst_k = target.builtin.kind;

      if (src_k == dst_k) {
        cur->type = target;
        continue;
      }

      bool ok = false;
      if (is_int_kind(src_k) && is_int_kind(dst_k)) ok = true;
      if (is_int_kind(src_k) && dst_k == type::kind_enum::f16) ok = true;
      if (is_int_kind(src_k) && dst_k == type::kind_enum::f32) ok = true;
      if (src_k == type::kind_enum::f16 && is_int_kind(dst_k)) ok = true;
      if (src_k == type::kind_enum::f32 && is_int_kind(dst_k)) ok = true;

      if (src_k == type::kind_enum::string || dst_k == type::kind_enum::string)
        ok = false;

      if (!ok) {
        throw std::runtime_error(std::string("unsupported 'as' cast: ") +
                                 builtin_type_name(cur->type.builtin) + " -> " +
                                 builtin_type_name(target.builtin));
      }

      const auto cast = std::make_shared<sema::cast>();
      cast->inner = cur;
      cast->target_type = target;
      cast->type = target;
      cur = cast;
      continue;
    }

    if (auto* asf =
            dynamic_cast<ClearLanguageParser::AsForceSuffixContext*>(child)) {
      type_ref target = make_type_ref_from(asf->type());
      if (const auto lit = std::dynamic_pointer_cast<literal>(cur)) {
        if (lit->value.is_untyped_int) {
          const auto coerced =
              sema_utils::coerce_untyped_int_to(lit->value, target);
          lit->value = coerced;
          lit->type = target;
          cur->type = target;
          continue;
        }
      }
      if (!(cur->type.is_builtin() && target.is_builtin())) {
        throw std::runtime_error("can only cast between builtin types");
      }
      const auto src_k = cur->type.builtin.kind;
      const auto dst_k = target.builtin.kind;
      if (src_k == dst_k) {
        cur->type = target;
        continue;
      }

      if (src_k == type::kind_enum::string && is_int_kind(dst_k)) {
        if (const auto lit = std::dynamic_pointer_cast<literal>(cur)) {
          if (std::holds_alternative<std::string>(lit->value.v)) {
            const std::string& s = std::get<std::string>(lit->value.v);
            auto is_digits = [](const std::string& t) {
              if (t.empty()) return false;
              size_t i = (t[0] == '+' || t[0] == '-') ? 1u : 0u;
              if (i >= t.size()) return false;
              for (; i < t.size(); ++i)
                if (!std::isdigit(static_cast<unsigned char>(t[i])))
                  return false;
              return true;
            };
            if (is_digits(s)) {
              try {
                if (type_ref::is_unsigned(target)) {
                  uint64_t u = static_cast<uint64_t>(std::stoull(s));
                  if (!fits(target, std::variant<int64_t, uint64_t>(u)))
                    throw std::runtime_error("overflow");
                  lit->value = value{target, u, false};
                } else {
                  int64_t i = static_cast<int64_t>(std::stoll(s));
                  if (!fits(target, std::variant<int64_t, uint64_t>(i)))
                    throw std::runtime_error("overflow");
                  lit->value = value{target, i, false};
                }
                lit->type = target;
                cur->type = target;
                continue;
              } catch (...) {
              }
            }
          }
        }
        const char* callee = parse_func_for(dst_k);
        if (!callee) {
          throw std::runtime_error(
              "string to this target type via 'as! is not supported");
        }
        const auto call = std::make_shared<sema::call>();
        call->callee = callee;
        call->args.clear();
        call->args.push_back(cur);
        call->type = target;
        cur = call;
        continue;
      }

      bool ok = false;
      if (is_int_kind(src_k) && is_int_kind(dst_k)) ok = true;
      if (is_int_kind(src_k) && dst_k == type::kind_enum::f16) ok = true;
      if (is_int_kind(src_k) && dst_k == type::kind_enum::f32) ok = true;
      if (src_k == type::kind_enum::f16 && is_int_kind(dst_k)) ok = true;
      if (src_k == type::kind_enum::f32 && is_int_kind(dst_k)) ok = true;

      if (src_k == type::kind_enum::string || dst_k == type::kind_enum::string)
        ok = false;

      if (!ok) {
        throw std::runtime_error(std::string("unsupported 'as!' cast: ") +
                                 builtin_type_name(cur->type.builtin) + " -> " +
                                 builtin_type_name(target.builtin));
      }

      const auto cast = std::make_shared<sema::cast>();
      cast->inner = cur;
      cast->target_type = target;
      cast->type = target;
      cur = cast;
      continue;
    }
  }
  return cur;
}

std::any sema_builder::visitParenExpr(
    ClearLanguageParser::ParenExprContext* ctx) {
  return visit(ctx->expr());
}

std::any sema_builder::visitUnitLiteral(
    ClearLanguageParser::UnitLiteralContext* ctx) {
  const auto lit = std::make_shared<literal>();
  lit->type = type_ref::builtin_type(type{type::kind_enum::unit});
  lit->value = value{lit->type, static_cast<int64_t>(0), false};
  return std::static_pointer_cast<expr>(lit);
}

static std::string unescape_string_token(const std::string& tok) {
  std::string in = tok;
  if (in.size() >= 2 && in.front() == '"' && in.back() == '"') {
    in = in.substr(1, in.size() - 2);
  }
  std::string out;
  out.reserve(in.size());
  for (size_t i = 0; i < in.size(); ++i) {
    if (in[i] == '\\' && i + 1 < in.size()) {
      switch (const char next = in[i + 1]) {
        case 'b':
          out.push_back('\b');
          break;
        case 'f':
          out.push_back('\f');
          break;
        case 'n':
          out.push_back('\n');
          break;
        case 'r':
          out.push_back('\r');
          break;
        case 't':
          out.push_back('\t');
          break;
        case '\\':
          out.push_back('\\');
          break;
        case '\'':
          out.push_back('\'');
          break;
        case '"':
          out.push_back('"');
          break;
        default:
          out.push_back(next);
          break;
      }
      ++i;
    } else {
      out.push_back(in[i]);
    }
  }
  return out;
}

std::any sema_builder::visitStringLiteral(
    ClearLanguageParser::StringLiteralContext* ctx) {
  const auto node = std::make_shared<literal>();
  node->type = type_ref::builtin_type(type{type::kind_enum::string});
  const std::string raw = ctx->STRING()->getText();
  node->value = make_string(unescape_string_token(raw));
  return std::static_pointer_cast<expr>(node);
}

std::any sema_builder::visitBoolLiteral(
    ClearLanguageParser::BoolLiteralContext* ctx) {
  const auto node = std::make_shared<literal>();
  node->type = boolean_type();
  const bool is_true(ctx->TRUE() != nullptr);
  node->value = value{node->type, static_cast<int64_t>(is_true ? 1 : 0), false};
  return std::static_pointer_cast<expr>(node);
}
