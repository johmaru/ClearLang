#pragma once


#include <ConsoleErrorListener.h>
#include <iostream>
#include <filesystem>
#include <string>
#include "ClearLanguageParser.h"
#include "../sema/SemaBuilder.h"
#include "../ir/IRGenFromSema.h"
#include <llvm/Support/raw_ostream.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/ExecutionEngine/Orc/LLJIT.h>
#include <llvm/Transforms/Utils/Cloning.h>
#include "../core/CLType.h"

int execute(int argc,const char* argv[],ClearLanguageParser& parser);

int support_execute_debug(IRGenFromSema& ir, std::unique_ptr<llvm::LLVMContext>& ctx);
