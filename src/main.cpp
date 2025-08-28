#include <iostream>
#include <string>
#include <cstdint>
#include "ClearLanguageLexer.h"
#include "ClearLanguageParser.h"
#include "ast/CLVisitor.cpp"

int main(int argc, const char* argv[]) {
    try {
        std::string code;
        if (argc > 1) {
            code = argv[1];
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
        auto result = std::any_cast<int64_t>(anyResult);
        std::cout << result << std::endl;
        return 0;
    } catch (const std::exception& ex) {
        std::cerr << "error: " << ex.what() << std::endl;
        return 1;
    }
}