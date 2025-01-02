// A tool for source code analysis.
//
// Author: Djordje Todorovic
//
// This version demonstrates how to:
//  1) Parse a set of function symbols from the current kernel environment
//     (using an existing compile_commands.json in the LKM build dir).
//  2) Compare those symbols' prototypes with a *target* kernel-headers
//  directory
//     that lacks compile_commands.json, by constructing a CompilationDatabase
//     in memory and a dummy .c file that includes needed headers.
//
// Usage (example):
//   cd /path/to/LKM/build   # This dir has compile_commands.json
//   ./proto-diff -p=. \
//                -symbols-file=input_symbols.txt \
//                -target-linux-source=/usr/src/linux-headers-6.8.0-foo \
//                some_source_in_lkm.c
//
// If everything goes well, it compares the function declarations found in the
// "current environment" vs. the "target" headers.

#include "clang/AST/ASTContext.h"
#include "clang/AST/RecursiveASTVisitor.h"
#include "clang/Frontend/CompilerInstance.h"
#include "clang/Frontend/FrontendAction.h"
#include "clang/Tooling/CommonOptionsParser.h"
#include "clang/Tooling/CompilationDatabase.h"
#include "clang/Tooling/Tooling.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/WithColor.h"
#include "llvm/Support/raw_ostream.h"

#include <algorithm>
#include <fstream>
#include <memory>
#include <string>
#include <vector>

// Use these namespaces for brevity
using namespace clang;
using namespace clang::tooling;
using namespace llvm;

//===----------------------------------------------------------------------===//
// Command-line options
//===----------------------------------------------------------------------===//

namespace {
cl::OptionCategory ToolCategory("proto-diff options");

cl::opt<std::string> SymbolsFile("symbols-file",
                                 cl::desc("Path to input_symbols.txt"),
                                 cl::value_desc("filename"),
                                 cl::cat(ToolCategory));

cl::opt<std::string>
    TargetLinuxSource("target-linux-source",
                      cl::desc("Path to new (target) linux-headers directory "
                               "(no compile_commands.json)"),
                      cl::value_desc("path"), cl::cat(ToolCategory));

// This is optional, for specifying which headers to include in dummy.c
// if you want to override defaults:
cl::list<std::string> TargetHeaders(
    "target-headers",
    cl::desc(
        "Headers (relative to 'target-linux-source') to include in dummy.c"),
    cl::value_desc("<linux/fs.h> ..."), cl::cat(ToolCategory));
} // end anonymous namespace

//===----------------------------------------------------------------------===//
// Data structures
//===----------------------------------------------------------------------===//

struct FunctionInfo {
  std::string Name;
  std::string ReturnType;
  std::vector<std::string> Parameters;
};

class FunctionDeclVisitor : public RecursiveASTVisitor<FunctionDeclVisitor> {
public:
  FunctionDeclVisitor(ASTContext *Context, std::vector<FunctionInfo> &Functions,
                      const std::vector<std::string> &SymbolsToCheck)
      : Context(Context), Functions(Functions), SymbolsToCheck(SymbolsToCheck) {
  }

  bool VisitFunctionDecl(FunctionDecl *FD) {
    // If we have a list of symbols, skip anything not in that list:
    if (!SymbolsToCheck.empty()) {
      std::string Name = FD->getNameAsString();
      auto it = std::find(SymbolsToCheck.begin(), SymbolsToCheck.end(), Name);
      if (it == SymbolsToCheck.end()) {
        // Not in the symbol list, skip
        return true;
      }
    }

    // Optionally skip if not in main file (but for kernel headers, we might
    // want to remove this check): if
    // (!Context->getSourceManager().isInMainFile(FD->getLocation())) {
    //   return true;
    // }

    FunctionInfo Info;
    Info.Name = FD->getNameAsString();
    Info.ReturnType = FD->getReturnType().getAsString();
    for (const auto *Param : FD->parameters()) {
      Info.Parameters.push_back(Param->getType().getAsString());
    }
    Functions.push_back(Info);
    return true;
  }

private:
  ASTContext *Context;
  std::vector<FunctionInfo> &Functions;
  const std::vector<std::string> &SymbolsToCheck;
};

class FunctionDeclASTConsumer : public ASTConsumer {
public:
  FunctionDeclASTConsumer(std::vector<FunctionInfo> &Functions,
                          const std::vector<std::string> &SymbolsToCheck,
                          ASTContext &Context)
      : Visitor(&Context, Functions, SymbolsToCheck) {}

  void HandleTranslationUnit(ASTContext &Context) override {
    Visitor.TraverseDecl(Context.getTranslationUnitDecl());
  }

private:
  FunctionDeclVisitor Visitor;
};

class FunctionDeclAction : public ASTFrontendAction {
public:
  FunctionDeclAction(std::vector<FunctionInfo> &Functions,
                     const std::vector<std::string> &SymbolsToCheck)
      : Functions(Functions), SymbolsToCheck(SymbolsToCheck) {}

  std::unique_ptr<ASTConsumer> CreateASTConsumer(CompilerInstance &CI,
                                                 StringRef File) override {
    return std::make_unique<FunctionDeclASTConsumer>(Functions, SymbolsToCheck,
                                                     CI.getASTContext());
  }

private:
  std::vector<FunctionInfo> &Functions;
  const std::vector<std::string> &SymbolsToCheck;
};

class FunctionDeclActionFactory : public FrontendActionFactory {
public:
  FunctionDeclActionFactory(std::vector<FunctionInfo> &Functions,
                            const std::vector<std::string> &SymbolsToCheck)
      : Functions(Functions), SymbolsToCheck(SymbolsToCheck) {}

  std::unique_ptr<FrontendAction> create() override {
    return std::make_unique<FunctionDeclAction>(Functions, SymbolsToCheck);
  }

private:
  std::vector<FunctionInfo> &Functions;
  const std::vector<std::string> &SymbolsToCheck;
};

//===----------------------------------------------------------------------===//
// CompareFunctions
//===----------------------------------------------------------------------===//

static void CompareFunctions(const std::vector<FunctionInfo> &File1Functions,
                             const std::vector<FunctionInfo> &File2Functions) {
  outs() << "=== Differences in Function Declarations ===\n";

  // Check for changed or removed functions
  for (const auto &Func1 : File1Functions) {
    auto It = std::find_if(
        File2Functions.begin(), File2Functions.end(),
        [&](const FunctionInfo &Func2) { return Func1.Name == Func2.Name; });

    if (It != File2Functions.end()) {
      const auto &Func2 = *It;
      if (Func1.Parameters != Func2.Parameters ||
          Func1.ReturnType != Func2.ReturnType) {
        WithColor(llvm::outs(), HighlightColor::String)
            << "Function \"" << Func1.Name << "\" has changed:\n";
        WithColor(llvm::outs(), HighlightColor::String)
            << "  Return type: " << Func1.ReturnType << " -> "
            << Func2.ReturnType << "\n";
        WithColor(llvm::outs(), HighlightColor::String) << "  Parameters: ";
        for (const auto &Param : Func1.Parameters)
          WithColor(llvm::outs(), HighlightColor::String) << Param << ", ";
        WithColor(llvm::outs(), HighlightColor::String) << " -> ";
        for (const auto &Param : Func2.Parameters)
          WithColor(llvm::outs(), HighlightColor::String) << Param << ", ";
        WithColor(llvm::outs(), HighlightColor::String) << "\n";
      }
    } else {
      WithColor(llvm::outs(), HighlightColor::String)
          << "Function \"" << Func1.Name << "\" removed.\n";
    }
  }

  // Check for newly added functions
  for (const auto &Func2 : File2Functions) {
    auto It = std::find_if(
        File1Functions.begin(), File1Functions.end(),
        [&](const FunctionInfo &Func1) { return Func2.Name == Func1.Name; });
    if (It == File1Functions.end()) {
      WithColor(llvm::outs(), HighlightColor::String)
          << "Function \"" << Func2.Name << "\" added.\n";
    }
  }
}

//===----------------------------------------------------------------------===//
// Helper function: Run ClangTool with a given compilation database & sources
//===----------------------------------------------------------------------===//

static bool RunToolWithDB(CompilationDatabase &CompDB,
                          const std::vector<std::string> &Sources,
                          const std::vector<std::string> &SymbolsToCheck,
                          std::vector<FunctionInfo> &OutFunctions) {
  ClangTool Tool(CompDB, Sources);
  auto Factory =
      std::make_unique<FunctionDeclActionFactory>(OutFunctions, SymbolsToCheck);
  // ClangTool returns 0 on success, non-zero on error:
  return (Tool.run(Factory.get()) != 0);
}

//===----------------------------------------------------------------------===//
// main()
//===----------------------------------------------------------------------===//

int main(int argc, const char **argv) {
  // 1) Parse command-line options for the "current" environment
  auto ExpectedParser = CommonOptionsParser::create(argc, argv, ToolCategory);
  if (!ExpectedParser) {
    WithColor::error() << "[Failed to parse arguments] "
                       << toString(ExpectedParser.takeError()) << "\n";
    return 1;
  }
  CommonOptionsParser &CurrentKernelOptions = *ExpectedParser;

  // 2) Read symbols from input_symbols.txt if provided
  std::vector<std::string> SymbolsToCheck;
  if (!SymbolsFile.empty()) {
    std::ifstream ifs(SymbolsFile);
    std::string sym;
    while (std::getline(ifs, sym)) {
      if (!sym.empty()) {
        SymbolsToCheck.push_back(sym);
      }
    }
  }

  // 3) Gather function prototypes from the "current" kernel build environment.
  //    This uses the compile_commands.json from the LKM build dir (i.e., from
  //    -p=).
  std::vector<FunctionInfo> File1Functions;
  {
    // The user passes the current kernel sources on the command line, e.g.:
    //   proto-diff -p=. some_lkm_source_file.c
    // or just '.' if we want to parse everything known from
    // compile_commands.json.
    ClangTool CurrentTool(CurrentKernelOptions.getCompilations(),
                          CurrentKernelOptions.getSourcePathList());
    if (CurrentTool.run(
            new FunctionDeclActionFactory(File1Functions, SymbolsToCheck))) {
      WithColor::error() << "Error processing current (LKM) environment.\n";
      return 1;
    }
  }

  // 4) Gather function prototypes from the "target" linux-headers (if given)
  std::vector<FunctionInfo> File2Functions;
  if (!TargetLinuxSource.empty()) {
    // We assume this is something like /usr/src/linux-headers-6.8.0-foo,
    // which does *not* contain a compile_commands.json. So we build
    // a CompilationDatabase pointing to a dummy .c file.

    // 4.1) We create a "dummy.c" in a temporary directory, or just in the
    //      current directory if that's acceptable. That dummy file #includes
    //      the headers we care about. If user doesn't specify
    //      -target-headers=..., we'll default to <linux/fs.h>, etc. (as an
    //      example).

    std::vector<std::string> DefaultHeaders = {
        "linux/fs.h", "linux/sched.h"
        // Add more if you like
    };

    // If user specified any -target-headers, use those. Otherwise use default:
    std::vector<std::string> HeadersToInclude =
        TargetHeaders.empty() ? DefaultHeaders
                              : std::vector<std::string>(TargetHeaders.begin(),
                                                         TargetHeaders.end());

    // 4.2) Actually create dummy.c content:
    std::string DummyContent;
    DummyContent += "#include <stdio.h>\n"; // optional
    for (auto &H : HeadersToInclude) {
      DummyContent += "#include <" + H + ">\n";
    }

    // Write dummy.c to a temporary file. For simplicity, let's just do
    // "dummy_target.c" in the current working directory. In production,
    // you'd want a real tmp dir with unique naming, error checks, etc.
    std::string DummyFilePath = "dummy_target.c";
    {
      std::error_code EC;
      raw_fd_ostream OS(DummyFilePath, EC, sys::fs::OF_Text);
      if (EC) {
        WithColor::error() << "Cannot create dummy_target.c: " << EC.message()
                           << "\n";
        return 1;
      }
      OS << DummyContent;
      OS.close();
    }

    // 4.3) Construct a FixedCompilationDatabase with minimal flags
    // We'll assume x86_64. Adjust as needed.
    // Clang might need additional includes or defines to parse kernel headers
    // properly. This is *very* minimal.
    std::string CompilerName = "clang"; // or "clang++"
    std::vector<std::string> CommandLine = {
        CompilerName,
        "-std=c11", // or c++17 if needed
        "-I" + TargetLinuxSource + "/include",
        "-I" + TargetLinuxSource + "/arch/x86/include",
        // Possibly: -D__KERNEL__ etc. if needed
        "dummy_target.c"};

    // Directory is the same as TargetLinuxSource or current dir. We'll just
    // pass TargetLinuxSource for clarity, so #includes relative to that can be
    // found.
    FixedCompilationDatabase TargetCDB(TargetLinuxSource, CommandLine);

    // 4.4) Run the tool on that single dummy file
    std::vector<std::string> DummySources{DummyFilePath};
    if (RunToolWithDB(TargetCDB, DummySources, SymbolsToCheck,
                      File2Functions)) {
      WithColor::error() << "Error processing target linux headers.\n";
      return 1;
    }
  } else {
    WithColor::warning()
        << "No -target-linux-source specified, skipping second parse.\n";
  }

  // 5) Compare the two sets of collected function prototypes
  CompareFunctions(File1Functions, File2Functions);

  return 0;
}
