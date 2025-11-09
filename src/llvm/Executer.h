#pragma once

#include "../ir/IRGenFromSema.h"
#include "../sema/SemaBuilder.h"
#include "ClearLanguageParser.h"

#include <ConsoleErrorListener.h>
#include <cstdint>
#include <llvm/ExecutionEngine/Orc/LLJIT.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm/Transforms/Utils/Cloning.h>

int execute(int argc, const char* argv[], ClearLanguageParser& parser);

int supportExecuteDebug(IrGenFromSema& irg, std::unique_ptr<llvm::LLVMContext>& ctx);

namespace executer {
enum class ExecResult : std::uint8_t {
    SUCCESS = 0,
    SYNTAX_ERROR = 2,
    EXECUTION_ERROR = 3,
    RUNTIME_LIB_LOAD_ERROR = 4,
};
}
