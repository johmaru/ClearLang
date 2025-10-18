#pragma once
#include <string>
#include <vector>

class build_dsl {
 public:
  static void set_entry(std::string entry);
  static void add_source(std::string path);
  static void set_output(std::string path);
  static void add_dependency(std::string dep);
  static void set_target(std::string kind);
  static void set_app_name(std::string name);

 private:
  static inline std::string entry_point_;
  static inline std::vector<std::string> source_paths_;
  static inline std::string output_path_;
  static inline std::string target_kind_;
  static inline std::string app_name_;
};