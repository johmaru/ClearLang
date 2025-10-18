#pragma once

#include <string>
#include <vector>

namespace sema {
struct module;
}  // namespace sema

class build_command {
 public:
  explicit build_command(std::string build_script_path);
  void execute_build(bool debug) const;

 private:
  enum class build_kind : std::uint8_t { none, exe, dll };

  void parse_build_script();
  void extract_build_functions(const sema::module& mod);
  static std::vector<std::string> collect_source_files(const std::string& root);

 private:
  std::string build_script_path_;
  std::string entry_point_;
  std::string source_root_ = "src";
  std::string output_path_ = "build";
  build_kind kind_ = build_kind::exe;
  std::string app_name_ = "ClearApp";
};