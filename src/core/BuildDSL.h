#pragma once
#include <string>
#include <vector>

class BuildDsl {
  public:
    static void setEntry(std::string entry);
    static void addSource(std::string path);
    static void setOutput(std::string path);
    static void addDependency(std::string dep);
    static void setTarget(std::string kind);
    static void setAppName(std::string name);

  private:
    static inline std::string entry_point;
    static inline std::vector<std::string> source_paths;
    static inline std::string output_path;
    static inline std::string target_kind;
    static inline std::string app_name;
};