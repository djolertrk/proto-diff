// A minimal proto-diff tool, doing two passes on the SAME files:
//  1) Read normal compile_commands.json (run #1).
//  2) Create an in-memory compile DB with replaced "directory" and flags
//     (run #2), on the SAME source files.
//
// No usage of "FixedCompilationDatabase" or "CompileCommand.File" is needed.
// We define local "startsWith" checks using manual string operations.
//
// By Djordje Todorovic

#include "clang/AST/ASTContext.h"
#include "clang/AST/RecursiveASTVisitor.h"
#include "clang/Frontend/CompilerInstance.h"
#include "clang/Frontend/FrontendAction.h"
#include "clang/Lex/PPCallbacks.h"
#include "clang/Tooling/CommonOptionsParser.h"
#include "clang/Tooling/CompilationDatabase.h"
#include "clang/Tooling/Tooling.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/WithColor.h"
#include "llvm/Support/raw_ostream.h"

#include <algorithm>
#include <fstream>
#include <memory>
#include <string>
#include <vector>

// ------------------------------------------------------------------------
// 1) Command-line options
// ------------------------------------------------------------------------

namespace {
llvm::cl::OptionCategory ToolCategory("proto-diff options");

llvm::cl::opt<std::string>
    SymbolsFile("symbols-file", llvm::cl::desc("Path to input_symbols.txt"),
                llvm::cl::value_desc("filename"), llvm::cl::cat(ToolCategory));

// You want to transform "directory" field to this:
llvm::cl::opt<std::string>
    TargetLinuxSource("target-linux-source",
                      llvm::cl::desc("New directory path to replace the "
                                     "original compile command's directory"),
                      llvm::cl::value_desc("path"),
                      llvm::cl::cat(ToolCategory));
} // end anonymous namespace

// ------------------------------------------------------------------------
// 2) Utility: startsWith (C++17-friendly, no std::string::starts_with)
// ------------------------------------------------------------------------
static bool startsWith(const std::string &Str, const std::string &Prefix) {
  if (Prefix.size() > Str.size())
    return false;
  return std::equal(Prefix.begin(), Prefix.end(), Str.begin());
}

// ------------------------------------------------------------------------
// 3) Data structures for function prototypes
// ------------------------------------------------------------------------

struct FunctionInfo {
  std::string Name;
  std::string ReturnType;
  std::vector<std::string> Parameters;
};

// ------------------------------------------------------------------------
// 4) AST visitor to gather function declarations
// ------------------------------------------------------------------------

class FunctionDeclVisitor
    : public clang::RecursiveASTVisitor<FunctionDeclVisitor> {
public:
  FunctionDeclVisitor(clang::ASTContext &Ctx,
                      std::vector<FunctionInfo> &OutFunctions,
                      const std::vector<std::string> &Symbols)
      : Ctx(Ctx), OutFunctions(OutFunctions), Symbols(Symbols) {}

  bool VisitFunctionDecl(clang::FunctionDecl *FD) {
    if (!Symbols.empty()) {
      // If we have a list of symbols, skip others
      std::string Name = FD->getNameAsString();
      if (std::find(Symbols.begin(), Symbols.end(), Name) == Symbols.end()) {
        return true; // skip
      }
    }

    FunctionInfo Info;
    Info.Name = FD->getNameAsString();
    Info.ReturnType = FD->getReturnType().getAsString();
    for (auto *Param : FD->parameters()) {
      Info.Parameters.push_back(Param->getType().getAsString());
    }
    OutFunctions.push_back(Info);
    return true;
  }

private:
  clang::ASTContext &Ctx;
  std::vector<FunctionInfo> &OutFunctions;
  const std::vector<std::string> &Symbols;
};

class FunctionDeclASTConsumer : public clang::ASTConsumer {
public:
  FunctionDeclASTConsumer(std::vector<FunctionInfo> &Out,
                          const std::vector<std::string> &Syms,
                          clang::ASTContext &Ctx)
      : Visitor(Ctx, Out, Syms) {}

  void HandleTranslationUnit(clang::ASTContext &Context) override {
    Visitor.TraverseDecl(Context.getTranslationUnitDecl());
  }

private:
  FunctionDeclVisitor Visitor;
};

class FunctionDeclAction : public clang::ASTFrontendAction {
public:
  FunctionDeclAction(std::vector<FunctionInfo> &Out,
                     const std::vector<std::string> &Syms)
      : OutFunctions(Out), Symbols(Syms) {}

  std::unique_ptr<clang::ASTConsumer>
  CreateASTConsumer(clang::CompilerInstance &CI,
                    clang::StringRef /*InFile*/) override {
    return std::make_unique<FunctionDeclASTConsumer>(OutFunctions, Symbols,
                                                     CI.getASTContext());
  }

private:
  std::vector<FunctionInfo> &OutFunctions;
  const std::vector<std::string> &Symbols;
};

// A small factory
class FunctionDeclActionFactory : public clang::tooling::FrontendActionFactory {
public:
  FunctionDeclActionFactory(std::vector<FunctionInfo> &Out,
                            const std::vector<std::string> &Syms)
      : OutFunctions(Out), Symbols(Syms) {}

  std::unique_ptr<clang::FrontendAction> create() override {
    return std::make_unique<FunctionDeclAction>(OutFunctions, Symbols);
  }

private:
  std::vector<FunctionInfo> &OutFunctions;
  const std::vector<std::string> &Symbols;
};

// ------------------------------------------------------------------------
// 5) Compare function prototypes
// ------------------------------------------------------------------------
static void compareFunctions(const std::vector<FunctionInfo> &OldF,
                             const std::vector<FunctionInfo> &NewF) {
  llvm::outs() << "=== Differences in Function Declarations ===\n";

  // check changed or removed
  for (auto &OF : OldF) {
    auto It = std::find_if(NewF.begin(), NewF.end(),
                           [&](auto &NF) { return NF.Name == OF.Name; });
    if (It != NewF.end()) {
      // check changes
      if (OF.ReturnType != It->ReturnType || OF.Parameters != It->Parameters) {
        llvm::WithColor(llvm::outs(), llvm::HighlightColor::String)
            << "Function \"" << OF.Name << "\" has changed:\n";
        llvm::outs() << "  Return type: " << OF.ReturnType << " -> "
                     << It->ReturnType << "\n";
        llvm::outs() << "  Parameters: ";
        for (auto &P : OF.Parameters)
          llvm::outs() << P << ", ";
        llvm::outs() << " -> ";
        for (auto &P : It->Parameters)
          llvm::outs() << P << ", ";
        llvm::outs() << "\n";
      }
    } else {
      llvm::WithColor(llvm::outs(), llvm::HighlightColor::String)
          << "Function \"" << OF.Name << "\" removed.\n";
    }
  }
  // check newly added
  for (auto &NF : NewF) {
    auto It = std::find_if(OldF.begin(), OldF.end(),
                           [&](auto &OF) { return OF.Name == NF.Name; });
    if (It == OldF.end()) {
      llvm::WithColor(llvm::outs(), llvm::HighlightColor::String)
          << "Function \"" << NF.Name << "\" added.\n";
    }
  }
}

// ------------------------------------------------------------------------
// 6) Helper function to run the function-decl action
// ------------------------------------------------------------------------
static bool runFunctionAction(clang::tooling::CompilationDatabase &DB,
                              const std::vector<std::string> &Files,
                              const std::vector<std::string> &Symbols,
                              std::vector<FunctionInfo> &Out) {
  clang::tooling::ClangTool Tool(DB, Files);
  auto Factory = std::make_unique<FunctionDeclActionFactory>(Out, Symbols);
  // returns 0 on success
  return (Tool.run(Factory.get()) != 0);
}

// ------------------------------------------------------------------------
// 7) Build a memory-based compile DB with updated "directory" + updated flags
// ------------------------------------------------------------------------
class MemoryCompileDB : public clang::tooling::CompilationDatabase {
public:
  // We store a single set of arguments for each file in "Files".
  MemoryCompileDB(std::vector<std::string> Files, std::string Directory,
                  std::vector<std::string> Args)
      : Files(std::move(Files)), Directory(std::move(Directory)),
        Args(std::move(Args)) {}

  // returns all files
  std::vector<std::string> getAllFiles() const override { return Files; }

  // returns compile commands for a given "FilePath"
  std::vector<clang::tooling::CompileCommand>
  getCompileCommands(llvm::StringRef FilePath) const override {
    // If FilePath is in "Files", produce one command with updated
    // directory/args
    std::vector<clang::tooling::CompileCommand> Commands;
    auto It = std::find(Files.begin(), Files.end(), FilePath.str());
    if (It != Files.end()) {
      // produce one command
      clang::tooling::CompileCommand CC;
      CC.Directory = Directory;
      CC.CommandLine = Args;
      CC.Filename =
          FilePath.str(); // old "File" replaced by "Filename" in older Clang
      // "Output" can remain empty
      Commands.push_back(std::move(CC));
    }
    return Commands;
  }

  // for completeness, "getCompileCommands()" for all files
  std::vector<clang::tooling::CompileCommand>
  getAllCompileCommands() const override {
    std::vector<clang::tooling::CompileCommand> Commands;
    for (auto &F : Files) {
      clang::tooling::CompileCommand CC;
      CC.Directory = Directory;
      CC.CommandLine = Args;
      CC.Filename = F;
      Commands.push_back(std::move(CC));
    }
    return Commands;
  }

private:
  std::vector<std::string> Files;
  std::string Directory;
  std::vector<std::string> Args;
};

// A helper to strip or rewrite flags
static std::vector<std::string>
transformFlags(const std::vector<std::string> &OldFlags) {
  std::vector<std::string> NewFlags;
  for (auto &Flag : OldFlags) {
    // For example, remove flags that cause clang warnings
    // e.g. -falign-jumps=1, -Wimplicit-fallthrough=5, etc.
    // Check "startsWith(Flag, "...")"
    if (startsWith(Flag, "-falign-jumps=1")) {
      continue; // skip
    }
    if (startsWith(Flag, "-Wimplicit-fallthrough=5")) {
      continue; // skip
    }
    // ... etc. Or rewrite -I/usr/src => -I/home/djtodor...
    // For minimal example, we just keep everything
    NewFlags.push_back(Flag);
  }
  return NewFlags;
}

// ------------------------------------------------------------------------
// 8) main()
// ------------------------------------------------------------------------

int main(int argc, const char **argv) {
  auto ExpectedParser =
      clang::tooling::CommonOptionsParser::create(argc, argv, ToolCategory);
  if (!ExpectedParser) {
    llvm::WithColor::error()
        << "Failed to parse arguments: "
        << llvm::toString(ExpectedParser.takeError()) << "\n";
    return 1;
  }
  clang::tooling::CommonOptionsParser &Options = *ExpectedParser;

  // read symbols from file if provided
  std::vector<std::string> Symbols;
  if (!SymbolsFile.empty()) {
    std::ifstream ifs(SymbolsFile);
    std::string line;
    while (std::getline(ifs, line)) {
      if (!line.empty()) {
        Symbols.push_back(line);
      }
    }
  }

  // 1) first pass: gather function decls with the real DB
  std::vector<FunctionInfo> OldFuncs;
  {
    auto &DB = Options.getCompilations();
    auto SourceFiles = Options.getSourcePathList();
    if (SourceFiles.empty()) {
      llvm::WithColor::warning() << "No source files provided.\n";
    }
    if (runFunctionAction(DB, SourceFiles, Symbols, OldFuncs)) {
      llvm::WithColor::error() << "Error in first pass on provided files.\n";
      return 1;
    }
  }

  // 2) second pass: build a memory-based DB with replaced "directory"
  //    and replaced flags. We run on the SAME source files again.
  std::vector<FunctionInfo> NewFuncs;
  {
    auto SourceFiles = Options.getSourcePathList();
    if (SourceFiles.empty()) {
      llvm::WithColor::warning() << "No source files.\n";
    }

    // pick some new directory (the user wants -target-linux-source)
    // If not provided, fallback to the old directory or something
    if (TargetLinuxSource.empty()) {
      llvm::WithColor::error()
          << "No -target-linux-source specified, skipping second pass.\n";
      compareFunctions(OldFuncs, NewFuncs);
      return 0;
    }

    // We'll keep the same flags from the first file, for example:
    // just pick the compile commands for the first file from the real DB
    auto &DB = Options.getCompilations();
    if (!SourceFiles.empty()) {
      auto CCVec = DB.getCompileCommands(SourceFiles.front());
      if (!CCVec.empty()) {
        // We'll transform them slightly:
        auto OldDir =
            CCVec.front().Directory; // e.g. "/usr/src/linux-headers-5.15..."
        auto OldArgs = CCVec.front().CommandLine;

        auto NewDir = TargetLinuxSource.getValue(); // user-specified
        auto NewArgs = transformFlags(OldArgs);     // strip out some flags

        // Build a memory DB with the same set of files, new directory, new args
        MemoryCompileDB MemDB(SourceFiles, NewDir, NewArgs);

        // run second pass
        if (runFunctionAction(MemDB, SourceFiles, Symbols, NewFuncs)) {
          llvm::WithColor::error() << "Error in second pass.\n";
          return 1;
        }
      } else {
        llvm::WithColor::warning()
            << "No compile command found for " << SourceFiles.front() << "\n";
      }
    }
  }

  // 3) compare
  compareFunctions(OldFuncs, NewFuncs);
  return 0;
}
