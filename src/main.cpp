#include <fstream>
#include <iostream>
#include <string>
#include <cstdint>
#include <filesystem>
#include "ClearLanguageLexer.h"
#include "ClearLanguageParser.h"
#include "ast/CLVisitor.cpp"

int main(int argc, const char* argv[]) {
    try {
        std::string code;
        if (argc > 1) {
            if (std::string(argv[1]) == "run") {
                // Now we don't have a language setting file,so this compile read file to /compiler-path/src/main.clr

                std::filesystem::path base = std::string(argv[0]);
                std::filesystem::path srcPath = base.parent_path() / "src" / "main.clr";

                std::ifstream ifs(srcPath);
                if (!ifs.fail()) {
                    code = std::string((std::istreambuf_iterator<char>(ifs)), std::istreambuf_iterator<char>());
                } else {
                    code = argv[2];
                }
            } 
        } else {
            if (!std::getline(std::cin, code)) {
                std::cerr << "no input\n";
                return 1;
            }
        }

        ANTLRInputStream input(code);
        ClearLanguageLexer lexer(&input);
        CommonTokenStream tokens(&lexer);
        ClearLanguageParser parser(&tokens);

        auto* tree = parser.start();

        EvalVisitor eval;
        auto anyResult = eval.visit(tree);
        // noreturn functions yield empty std::any; others hold std::variant<int64_t,uint64_t>
        if (!anyResult.has_value()) {
            // No value to print (noreturn). Exit successfully.
            return 0;
        }
        auto ret = std::any_cast<std::variant<int64_t, uint64_t>>(anyResult);
        std::visit([](auto x){ std::cout << x << std::endl; }, ret);
        return 0;
    } catch (const std::exception& ex) {
        std::cerr << "error: " << ex.what() << std::endl;
        return 1;
    }
}