#pragma once

#include <cstdint>
#include <string>
#include <vector>

namespace sema {
struct Module;
} // namespace sema

class BuildCommand {
  public:
    explicit BuildCommand(std::string build_script_path);
    void executeBuild(bool debug) const;

  private:
    enum class build_kind : std::uint8_t { NONE, EXE, DLL };

    void parseBuildScript();
    void extractBuildFunctions(const sema::Module& mod);
    static std::vector<std::string> collectSourceFiles(const std::string& root);

    std::string build_script_path_;
    std::string entry_point_;
    std::string source_root_ = "src";
    std::string output_path_ = "build";
    build_kind kind_ = build_kind::EXE;
    std::string app_name_ = "ClearApp";
};