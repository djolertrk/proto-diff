// A minimal proto-diff tool that parses only the single file you pass,
// rather than all compile_commands.json entries.
//
// 1) Run on that single file with the real DB. Gather function prototypes.
// 2) Create a memory-based DB with replaced "directory"/flags, run again
//    on the SAME single file.
// 3) Compare.
//
// Author: Djordje Todorovic

#include "clang/AST/ASTContext.h"
#include "clang/AST/RecursiveASTVisitor.h"
#include "clang/Frontend/CompilerInstance.h"
#include "clang/Frontend/FrontendAction.h"
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

llvm::cl::opt<std::string> TargetLinuxSource(
    "target-linux-source",
    llvm::cl::desc(
        "New directory path to replace original compile command's directory"),
    llvm::cl::value_desc("path"), llvm::cl::cat(ToolCategory));
llvm::cl::opt<bool>
    ForceAllSymbols("force-all-symbols",
                    llvm::cl::desc("Force all symbols not just the ones "
                                   "specified in the input_symbols.txt"),
                    llvm::cl::init(false), llvm::cl::cat(ToolCategory));
llvm::cl::opt<bool> VeboseOutput("verbose", llvm::cl::desc("Verbose output"),
                                 llvm::cl::init(false),
                                 llvm::cl::cat(ToolCategory));
llvm::cl::opt<bool> SkipAdded("skip-added",
                              llvm::cl::desc("Skip newely added functions"),
                              llvm::cl::init(false),
                              llvm::cl::cat(ToolCategory));
llvm::cl::opt<bool> SkipRemoved("skip-removed",
                                llvm::cl::desc("Skip removed functions"),
                                llvm::cl::init(false),
                                llvm::cl::cat(ToolCategory));
} // end anonymous namespace

// ------------------------------------------------------------------------
// 2) Data structure for function prototypes
// ------------------------------------------------------------------------

struct FunctionInfo {
  std::string Name;
  std::string ReturnType;
  std::vector<std::string> Parameters;
};

// ------------------------------------------------------------------------
// 3) AST visitor to gather function declarations
// ------------------------------------------------------------------------

class FunctionDeclVisitor
    : public clang::RecursiveASTVisitor<FunctionDeclVisitor> {
public:
  FunctionDeclVisitor(clang::ASTContext &Ctx, std::vector<FunctionInfo> &Out,
                      const std::vector<std::string> &Symbols)
      : Ctx(Ctx), Out(Out), Symbols(Symbols) {}

  bool VisitFunctionDecl(clang::FunctionDecl *FD) {
    if (FD->getLocation().isMacroID()) {
      return true; // skip expansions from macros
    }

    if (!Symbols.empty() && !ForceAllSymbols) {
      // If we have a list of symbols, skip others
      std::string Name = FD->getNameAsString();
      if (std::find(Symbols.begin(), Symbols.end(), Name) == Symbols.end())
        return true;
    }

    FunctionInfo Info;
    Info.Name = FD->getNameAsString();
    Info.ReturnType = FD->getReturnType().getAsString();
    for (auto *Param : FD->parameters())
      Info.Parameters.push_back(Param->getType().getAsString());
    Out.push_back(std::move(Info));
    return true;
  }

private:
  clang::ASTContext &Ctx;
  std::vector<FunctionInfo> &Out;
  const std::vector<std::string> &Symbols;
};

class FunctionDeclASTConsumer : public clang::ASTConsumer {
public:
  FunctionDeclASTConsumer(std::vector<FunctionInfo> &Out,
                          const std::vector<std::string> &Symbols,
                          clang::ASTContext &Ctx)
      : Visitor(Ctx, Out, Symbols) {}

  void HandleTranslationUnit(clang::ASTContext &Context) override {
    Visitor.TraverseDecl(Context.getTranslationUnitDecl());
  }

private:
  FunctionDeclVisitor Visitor;
};

class FunctionDeclAction : public clang::ASTFrontendAction {
public:
  FunctionDeclAction(std::vector<FunctionInfo> &Out,
                     const std::vector<std::string> &Symbols)
      : Out(Out), Symbols(Symbols) {}

  std::unique_ptr<clang::ASTConsumer>
  CreateASTConsumer(clang::CompilerInstance &CI,
                    clang::StringRef /*InFile*/) override {
    return std::make_unique<FunctionDeclASTConsumer>(Out, Symbols,
                                                     CI.getASTContext());
  }

private:
  std::vector<FunctionInfo> &Out;
  const std::vector<std::string> &Symbols;
};

class FunctionDeclActionFactory : public clang::tooling::FrontendActionFactory {
public:
  FunctionDeclActionFactory(std::vector<FunctionInfo> &Out,
                            const std::vector<std::string> &Symbols)
      : Out(Out), Symbols(Symbols) {}

  std::unique_ptr<clang::FrontendAction> create() override {
    return std::make_unique<FunctionDeclAction>(Out, Symbols);
  }

private:
  std::vector<FunctionInfo> &Out;
  const std::vector<std::string> &Symbols;
};

// ------------------------------------------------------------------------
// 4) Compare function prototypes
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
      if (!SkipRemoved)
        llvm::WithColor(llvm::outs(), llvm::HighlightColor::String)
            << "Function \"" << OF.Name << "\" removed.\n";
    }
  }
  // check newly added
  for (auto &NF : NewF) {
    auto It = std::find_if(OldF.begin(), OldF.end(),
                           [&](auto &OF) { return OF.Name == NF.Name; });
    if (It == OldF.end() && !SkipAdded) {
      llvm::WithColor(llvm::outs(), llvm::HighlightColor::String)
          << "Function \"" << NF.Name << "\" added.\n";
    }
  }
}

// ------------------------------------------------------------------------
// 5) Minimal memory-based DB that changes "directory" and reuses the same flags
// ------------------------------------------------------------------------
class MemoryCompileDB : public clang::tooling::CompilationDatabase {
public:
  MemoryCompileDB(std::string SingleFile, std::string Directory,
                  std::vector<std::string> CommandLine)
      : File(std::move(SingleFile)), Directory(std::move(Directory)),
        CommandLine(std::move(CommandLine)) {}

  std::vector<std::string> getAllFiles() const override { return {File}; }

  std::vector<clang::tooling::CompileCommand>
  getCompileCommands(llvm::StringRef FilePath) const override {
    if (FilePath == File) {
      clang::tooling::CompileCommand CC;
      CC.Directory = Directory;
      CC.CommandLine = CommandLine;
      CC.Filename = File;
      return {CC};
    }
    return {};
  }

  std::vector<clang::tooling::CompileCommand>
  getAllCompileCommands() const override {
    clang::tooling::CompileCommand CC;
    CC.Directory = Directory;
    CC.CommandLine = CommandLine;
    CC.Filename = File;
    return {CC};
  }

private:
  std::string File;
  std::string Directory;
  std::vector<std::string> CommandLine;
};

// ------------------------------------------------------------------------
// 6) Helper to run the function action
// ------------------------------------------------------------------------
static bool runFunctionAction(clang::tooling::CompilationDatabase &DB,
                              const std::string &File,
                              const std::vector<std::string> &Symbols,
                              std::vector<FunctionInfo> &Out) {
  clang::tooling::ClangTool Tool(DB, {File});
  auto Factory = std::make_unique<FunctionDeclActionFactory>(Out, Symbols);
  return (Tool.run(Factory.get()) != 0); // 0 success
}

// ------------------------------------------------------------------------
// 7) main
// ------------------------------------------------------------------------

void printFlagsForFile(const clang::tooling::CompilationDatabase &DB,
                       llvm::StringRef LKMSourceFile) {
  // Get the compile commands for just this one file
  auto Commands = DB.getCompileCommands(LKMSourceFile);
  if (Commands.empty()) {
    llvm::errs() << "No compile command found for " << LKMSourceFile << "\n";
    return;
  }

  // Typically there's at most one or a small set of commands for the same file
  for (auto &Cmd : Commands) {
    llvm::errs() << "=== File: " << Cmd.Filename << " ===\n";
    llvm::errs() << "Directory: " << Cmd.Directory << "\n";
    llvm::errs() << "Arguments:\n";
    for (auto &Arg : Cmd.CommandLine) {
      llvm::errs() << "  " << Arg << "\n";
    }
    llvm::errs() << "\n";
  }
}

int main(int argc, const char **argv) {
  auto ExpectedParser =
      clang::tooling::CommonOptionsParser::create(argc, argv, ToolCategory);
  if (!ExpectedParser) {
    llvm::WithColor::error()
        << "Failed to parse arguments: "
        << llvm::toString(ExpectedParser.takeError()) << "\n";
    return 1;
  }
  clang::tooling::CommonOptionsParser &Parser = *ExpectedParser;

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

  // We expect exactly ONE .c file from the user:
  auto SourcePaths = Parser.getSourcePathList();
  if (SourcePaths.size() != 1) {
    llvm::WithColor::error() << "Please provide exactly one source file, e.g. "
                                "./proto-diff -p=. foo.c\n";
    return 1;
  }
  std::string LKMFile = SourcePaths.front();

  // 1) First pass: real DB on that single file
  std::vector<FunctionInfo> OldFuncs;
  {
    auto &RealDB = Parser.getCompilations();
    if (VeboseOutput)
      printFlagsForFile(RealDB, LKMFile);

    // find the compile command for LKMFile
    auto CCVec = RealDB.getCompileCommands(LKMFile);
    if (CCVec.empty()) {
      llvm::WithColor::error() << "No compile command found for " << LKMFile
                               << " in compile_commands.json.\n";
      return 1;
    }

    if (runFunctionAction(RealDB, LKMFile, Symbols, OldFuncs)) {
      llvm::WithColor::error() << "Error in first pass for " << LKMFile << "\n";
      return 1;
    }
  }

  // 2) Second pass: memory-based DB with replaced directory (and same flags).
  std::vector<FunctionInfo> NewFuncs;
  {
    if (TargetLinuxSource.empty()) {
      llvm::WithColor::error() << "No -target-linux-source specified.\n";
      compareFunctions(OldFuncs, NewFuncs);
      return 0;
    }

    auto &RealDB = Parser.getCompilations();
    auto CCVec = RealDB.getCompileCommands(LKMFile);
    if (!CCVec.empty()) {
      // We'll just pick the first command
      auto &CC = CCVec.front();

      // same flags
      auto NewCommandLine = CC.CommandLine;

      // create memory DB with the new directory
      MemoryCompileDB MemDB(LKMFile, TargetLinuxSource.getValue(),
                            NewCommandLine);

      if (VeboseOutput)
        printFlagsForFile(MemDB, LKMFile);

      if (runFunctionAction(MemDB, LKMFile, Symbols, NewFuncs)) {
        llvm::WithColor::error() << "Error in second pass.\n";
        return 1;
      }
    } else {
      llvm::WithColor::warning()
          << "No compile command again for " << LKMFile << "\n";
    }
  }

  // 3) compare
  compareFunctions(OldFuncs, NewFuncs);
  return 0;
}
