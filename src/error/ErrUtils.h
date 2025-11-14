#pragma once
#include <stdexcept>
#include <string>
#include <utility>

class ClearException : public std::runtime_error {
  public:
    struct MessageArgs {
        std::string file_path;
        size_t line = 0U;
        size_t column = 0U;
    };

    explicit ClearException(const std::string& msg, MessageArgs args)
        : std::runtime_error(msg), file_path_(std::move(args.file_path)), line_(args.line),
          column_(args.column) {}

    [[nodiscard]] const std::string& filePath() const noexcept {
        return file_path_;
    }
    [[nodiscard]] size_t line() const noexcept {
        return line_;
    }
    [[nodiscard]] size_t column() const noexcept {
        return column_;
    }

  private:
    std::string file_path_;
    size_t line_;
    size_t column_;
};