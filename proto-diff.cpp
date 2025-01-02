// A tool for source code analysis.
//
// Author: Djordje Todorovic
//
// This tool demonstrates a multi-step approach:
//   1) Collect function prototypes from the "current" LKM environment
//      (all sources in compile_commands.json).
//   2) Collect the actual #includes used by an LKM source file (positional
//   arg). 3) Find the original compile command for that LKM source file,
//      transform the "-I" referencing the old kernel to your
//      target linux-headers path.
//   4) Parse the discovered headers in the "target" kernel, preserving
//      the macros/flags from the LKM environment.
//   5) Compare the function prototypes.
//
// Requires Clang 18 or newer (overrides InclusionDirective(...), etc.).

#include "clang/AST/ASTContext.h"
#include "clang/AST/RecursiveASTVisitor.h"
#include "clang/Basic/SourceManager.h"
#include "clang/Frontend/CompilerInstance.h"
#include "clang/Frontend/FrontendAction.h"
#include "clang/Lex/PPCallbacks.h"
#include "clang/Lex/Preprocessor.h"
#include "clang/Tooling/CommonOptionsParser.h"
#include "clang/Tooling/CompilationDatabase.h"
#include "clang/Tooling/Tooling.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/Path.h"
#include "llvm/Support/WithColor.h"
#include "llvm/Support/raw_ostream.h"

#include <algorithm>
#include <fstream>
#include <iostream>
#include <memory>
#include <set>
#include <string>
#include <system_error>
#include <vector>

//---------------------------------------------------------------------------//
// 1) Command-line options
//---------------------------------------------------------------------------//

namespace {
llvm::cl::OptionCategory ToolCategory("proto-diff options");

llvm::cl::opt<std::string>
    SymbolsFile("symbols-file",
                llvm::cl::desc("Path to input_symbols.txt for function names"),
                llvm::cl::value_desc("filename"), llvm::cl::cat(ToolCategory));

llvm::cl::opt<std::string>
    TargetLinuxSource("target-linux-source",
                      llvm::cl::desc("Path to new (target) linux-headers "
                                     "directory (no compile_commands.json)"),
                      llvm::cl::value_desc("path"),
                      llvm::cl::cat(ToolCategory));

// If you want to parse the target headers as C or C++:
llvm::cl::opt<std::string> TargetLanguage(
    "target-lang",
    llvm::cl::desc("Language for parsing target headers (c, c-header, c++...)"),
    llvm::cl::init("c"), llvm::cl::cat(ToolCategory));
} // end anonymous namespace

//---------------------------------------------------------------------------//
// 2) Data structures for function prototypes
//---------------------------------------------------------------------------//

struct FunctionInfo {
  std::string Name;
  std::string ReturnType;
  std::vector<std::string> Parameters;
};

//---------------------------------------------------------------------------//
// 3) AST visitor to gather function declarations
//---------------------------------------------------------------------------//

class FunctionDeclVisitor
    : public clang::RecursiveASTVisitor<FunctionDeclVisitor> {
public:
  FunctionDeclVisitor(clang::ASTContext *Ctx,
                      std::vector<FunctionInfo> &OutFunctions,
                      const std::vector<std::string> &SymbolsToCheck)
      : Context(Ctx), OutFunctions(OutFunctions),
        SymbolsToCheck(SymbolsToCheck) {}

  bool VisitFunctionDecl(clang::FunctionDecl *FD) {
    // If we have a list of symbols, skip anything not in that list
    if (!SymbolsToCheck.empty()) {
      std::string Name = FD->getNameAsString();
      auto It = std::find(SymbolsToCheck.begin(), SymbolsToCheck.end(), Name);
      if (It == SymbolsToCheck.end()) {
        return true; // skip
      }
    }
    // Collect function info
    FunctionInfo Info;
    Info.Name = FD->getNameAsString();
    Info.ReturnType = FD->getReturnType().getAsString();
    for (const auto *Param : FD->parameters()) {
      Info.Parameters.push_back(Param->getType().getAsString());
    }
    OutFunctions.push_back(Info);
    return true;
  }

private:
  clang::ASTContext *Context;
  std::vector<FunctionInfo> &OutFunctions;
  const std::vector<std::string> &SymbolsToCheck;
};

class FunctionDeclASTConsumer : public clang::ASTConsumer {
public:
  FunctionDeclASTConsumer(std::vector<FunctionInfo> &OutFunctions,
                          const std::vector<std::string> &SymbolsToCheck,
                          clang::ASTContext &Ctx)
      : Visitor(&Ctx, OutFunctions, SymbolsToCheck) {}

  void HandleTranslationUnit(clang::ASTContext &Context) override {
    Visitor.TraverseDecl(Context.getTranslationUnitDecl());
  }

private:
  FunctionDeclVisitor Visitor;
};

class FunctionDeclAction : public clang::ASTFrontendAction {
public:
  FunctionDeclAction(std::vector<FunctionInfo> &OutFunctions,
                     const std::vector<std::string> &SymbolsToCheck)
      : OutFunctions(OutFunctions), SymbolsToCheck(SymbolsToCheck) {}

  std::unique_ptr<clang::ASTConsumer>
  CreateASTConsumer(clang::CompilerInstance &CI,
                    clang::StringRef InFile) override {
    return std::make_unique<FunctionDeclASTConsumer>(
        OutFunctions, SymbolsToCheck, CI.getASTContext());
  }

private:
  std::vector<FunctionInfo> &OutFunctions;
  const std::vector<std::string> &SymbolsToCheck;
};

class FunctionDeclActionFactory : public clang::tooling::FrontendActionFactory {
public:
  FunctionDeclActionFactory(std::vector<FunctionInfo> &OutFunctions,
                            const std::vector<std::string> &SymbolsToCheck)
      : OutFunctions(OutFunctions), SymbolsToCheck(SymbolsToCheck) {}

  std::unique_ptr<clang::FrontendAction> create() override {
    return std::make_unique<FunctionDeclAction>(OutFunctions, SymbolsToCheck);
  }

private:
  std::vector<FunctionInfo> &OutFunctions;
  const std::vector<std::string> &SymbolsToCheck;
};

//---------------------------------------------------------------------------//
// 4) PPCallbacks-based approach to gather #includes (Clang 18+)
//---------------------------------------------------------------------------//

class IncludeCollector : public clang::PPCallbacks {
public:
  IncludeCollector(clang::SourceManager &SM, std::set<std::string> &Collected)
      : SM(SM), Collected(Collected) {}

  // For Clang 18+: InclusionDirective is the correct override
  void InclusionDirective(clang::SourceLocation HashLoc,
                          const clang::Token &IncludeTok,
                          llvm::StringRef FileName, bool IsAngled,
                          clang::CharSourceRange FilenameRange,
                          clang::OptionalFileEntryRef File,
                          llvm::StringRef SearchPath,
                          llvm::StringRef RelativePath,
                          const clang::Module *Imported,
                          clang::SrcMgr::CharacteristicKind FileType) override {
    if (!File.has_value())
      return;
    auto &FE = File->getFileEntry();
    Collected.insert(FE.getName().str());
  }

private:
  clang::SourceManager &SM;
  std::set<std::string> &Collected;
};

class CollectIncludesAction : public clang::ASTFrontendAction {
public:
  CollectIncludesAction(std::set<std::string> &Collected)
      : Collected(Collected) {}

  std::unique_ptr<clang::ASTConsumer>
  CreateASTConsumer(clang::CompilerInstance &CI,
                    clang::StringRef InFile) override {
    // We don't traverse AST, so return empty consumer
    return std::make_unique<clang::ASTConsumer>();
  }

  bool BeginSourceFileAction(clang::CompilerInstance &CI) override {
    clang::Preprocessor &PP = CI.getPreprocessor();
    PP.addPPCallbacks(
        std::make_unique<IncludeCollector>(CI.getSourceManager(), Collected));
    return true; // success
  }

private:
  std::set<std::string> &Collected;
};

class CollectIncludesActionFactory
    : public clang::tooling::FrontendActionFactory {
public:
  CollectIncludesActionFactory(std::set<std::string> &Collected)
      : Collected(Collected) {}

  std::unique_ptr<clang::FrontendAction> create() override {
    return std::make_unique<CollectIncludesAction>(Collected);
  }

private:
  std::set<std::string> &Collected;
};

//---------------------------------------------------------------------------//
// 5) CompareFunctions
//---------------------------------------------------------------------------//

static void CompareFunctions(const std::vector<FunctionInfo> &File1Functions,
                             const std::vector<FunctionInfo> &File2Functions) {
  llvm::outs() << "=== Differences in Function Declarations ===\n";

  // Check for changed or removed
  for (const auto &Func1 : File1Functions) {
    auto It = std::find_if(
        File2Functions.begin(), File2Functions.end(),
        [&](const FunctionInfo &F2) { return Func1.Name == F2.Name; });
    if (It != File2Functions.end()) {
      const auto &Func2 = *It;
      if (Func1.Parameters != Func2.Parameters ||
          Func1.ReturnType != Func2.ReturnType) {
        llvm::WithColor(llvm::outs(), llvm::HighlightColor::String)
            << "Function \"" << Func1.Name << "\" has changed:\n";
        llvm::WithColor(llvm::outs(), llvm::HighlightColor::String)
            << "  Return type: " << Func1.ReturnType << " -> "
            << Func2.ReturnType << "\n";
        llvm::WithColor(llvm::outs(), llvm::HighlightColor::String)
            << "  Parameters: ";
        for (auto &P : Func1.Parameters)
          llvm::WithColor(llvm::outs(), llvm::HighlightColor::String)
              << P << ", ";
        llvm::WithColor(llvm::outs(), llvm::HighlightColor::String) << " -> ";
        for (auto &P : Func2.Parameters)
          llvm::WithColor(llvm::outs(), llvm::HighlightColor::String)
              << P << ", ";
        llvm::WithColor(llvm::outs(), llvm::HighlightColor::String) << "\n";
      }
    } else {
      llvm::WithColor(llvm::outs(), llvm::HighlightColor::String)
          << "Function \"" << Func1.Name << "\" removed.\n";
    }
  }

  // Check for newly added
  for (const auto &Func2 : File2Functions) {
    auto It = std::find_if(
        File1Functions.begin(), File1Functions.end(),
        [&](const FunctionInfo &F1) { return Func2.Name == F1.Name; });
    if (It == File1Functions.end()) {
      llvm::WithColor(llvm::outs(), llvm::HighlightColor::String)
          << "Function \"" << Func2.Name << "\" added.\n";
    }
  }
}

//---------------------------------------------------------------------------//
// 6) Helper: run ClangTool with a given CompilationDatabase & sources
//---------------------------------------------------------------------------//

static bool RunToolWithDB(clang::tooling::CompilationDatabase &CompDB,
                          const std::vector<std::string> &Sources,
                          const std::vector<std::string> &SymbolsToCheck,
                          std::vector<FunctionInfo> &OutFunctions) {
  clang::tooling::ClangTool Tool(CompDB, Sources);
  auto Factory =
      std::make_unique<FunctionDeclActionFactory>(OutFunctions, SymbolsToCheck);
  // returns true on error
  return (Tool.run(Factory.get()) != 0);
}

//---------------------------------------------------------------------------//
// 7) Retrieve and transform the compile command
//---------------------------------------------------------------------------//

// Retrieve original compile command for a specific .c file
static std::vector<std::string>
getCompileCommandFor(clang::tooling::CompilationDatabase &DB,
                     const std::string &FilePath) {
  auto Commands = DB.getCompileCommands(FilePath);
  if (Commands.empty()) {
    return {};
  }
  return Commands.front().CommandLine;
}

// Transform -I/usr/src/... => -I/target if needed
static std::vector<std::string>
transformCompileCommand(const std::vector<std::string> &OriginalArgs,
                        llvm::StringRef OldBase, llvm::StringRef NewBase) {
  std::vector<std::string> NewArgs;
  NewArgs.reserve(OriginalArgs.size());

  for (auto &Arg : OriginalArgs) {
    // skip the .c file
    llvm::StringRef ArgAsStringRef(Arg);
    if (ArgAsStringRef.endswith(".c") || ArgAsStringRef.endswith(".cc") ||
        ArgAsStringRef.endswith(".cpp")) {
      continue;
    }
    // if Arg is -I/usr/src/old => rewrite
    if (ArgAsStringRef.startswith("-I")) {
      llvm::StringRef Path = Arg.substr(2);
      if (Path.startswith(OldBase)) {
        std::string subpath = Path.substr(OldBase.size()).str();
        std::string Repl = (NewBase + subpath).str();
        NewArgs.push_back("-I" + Repl);
        continue;
      }
    }
    // keep everything else
    NewArgs.push_back(Arg);
  }

  return NewArgs;
}

//---------------------------------------------------------------------------//
// 8) Map old absolute header path => new absolute header path
//---------------------------------------------------------------------------//

static std::string mapOldToNew(const std::string &OldPath,
                               const std::string &OldBase,
                               const std::string &NewBase) {
  if (OldPath.find(OldBase) == 0) {
    std::string subpath = OldPath.substr(OldBase.size());
    return NewBase + subpath;
  }
  // fallback
  return NewBase + OldPath;
}

//---------------------------------------------------------------------------//
// 9) Main
//---------------------------------------------------------------------------//

int main(int argc, const char **argv) {
  // 0) Parse the "current" environment from the LKM build dir
  auto ExpectedParser =
      clang::tooling::CommonOptionsParser::create(argc, argv, ToolCategory);
  if (!ExpectedParser) {
    llvm::WithColor::error()
        << "[Failed to parse arguments] "
        << llvm::toString(ExpectedParser.takeError()) << "\n";
    return 1;
  }
  clang::tooling::CommonOptionsParser &CurrentKernelOptions = *ExpectedParser;

  // Must specify -target-linux-source
  if (TargetLinuxSource.empty()) {
    llvm::WithColor::error()
        << "No -target-linux-source specified. This argument is required.\n";
    return 1;
  }

  // The LKM file is taken as a positional argument from the SourcePathList,
  // after the normal CommonOptionsParser arguments. We expect exactly one:
  auto PositionalArgs = CurrentKernelOptions.getSourcePathList();
  if (PositionalArgs.empty()) {
    llvm::WithColor::error()
        << "No LKM source file specified as a positional argument.\n";
    return 1;
  }
  // We'll just take the first one as "LKMFileToAnalyze"
  std::string LKMFileToAnalyze = PositionalArgs.front();

  // Read symbols from input_symbols.txt if provided
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

  // 1) Gather function prototypes from the "current" environment
  std::vector<FunctionInfo> OldFunctions;
  {
    auto &DB = CurrentKernelOptions.getCompilations();
    std::vector<std::string> AllFiles = DB.getAllFiles();
    if (AllFiles.empty()) {
      llvm::WithColor::warning()
          << "No files found in the current compile_commands.json.\n";
    }
    if (RunToolWithDB(DB, AllFiles, SymbolsToCheck, OldFunctions)) {
      llvm::WithColor::error()
          << "Error processing current environment prototypes.\n";
      return 1;
    }
  }

  // 2) Collect includes from the LKMFileToAnalyze
  std::set<std::string> UsedHeaders;
  {
    auto &DB = CurrentKernelOptions.getCompilations();

    // Ensure we can find a compile command for LKMFileToAnalyze
    // (some DBs use absolute vs. relative paths)
    std::string FullCPath;
    {
      auto FilesInDB = DB.getAllFiles();
      for (auto &F : FilesInDB) {
        if (F.find(LKMFileToAnalyze) != std::string::npos) {
          FullCPath = F;
          break;
        }
      }
      if (FullCPath.empty() && !FilesInDB.empty()) {
        llvm::WithColor::warning()
            << "Cannot find " << LKMFileToAnalyze
            << " in compile_commands.json. Using first file.\n";
        FullCPath = FilesInDB.front();
      }
    }

    if (!FullCPath.empty()) {
      clang::tooling::ClangTool IncludesTool(DB, {FullCPath});
      auto Factory =
          std::make_unique<CollectIncludesActionFactory>(UsedHeaders);
      int Ret = IncludesTool.run(Factory.get());
      if (Ret != 0) {
        llvm::WithColor::error()
            << "Error collecting includes from " << FullCPath << "\n";
      }
    } else {
      llvm::WithColor::warning() << "No file to analyze for includes!\n";
    }
  }

  // 3) Map those used headers to the target kernel, parse them
  std::vector<FunctionInfo> NewFunctions;
  if (!UsedHeaders.empty()) {
    // Example: old base might be /usr/src/linux-headers-5.15.0-127-generic
    std::string OldBase = "/usr/src/linux-headers-5.15.0-127-generic";

    // 3a) We get the original compile command for LKMFileToAnalyze
    auto &DB = CurrentKernelOptions.getCompilations();
    std::vector<std::string> OriginalCmd =
        getCompileCommandFor(DB, LKMFileToAnalyze);
    if (OriginalCmd.empty()) {
      llvm::WithColor::warning()
          << "Could not retrieve a compile command for " << LKMFileToAnalyze
          << ". Using a minimal approach.\n";
    }

    // 3b) Transform that command, rewriting -I references from OldBase =>
    // TargetLinuxSource
    std::vector<std::string> EditedArgs;
    if (!OriginalCmd.empty()) {
      EditedArgs =
          transformCompileCommand(OriginalCmd, OldBase, TargetLinuxSource);
      // Optionally adjust macros if you want to emulate a new version:
      // EditedArgs.push_back("-DLINUX_VERSION_CODE=0x060800");
    } else {
      // fallback minimal approach
      EditedArgs = {
          "clang",
          "-x",
          TargetLanguage,
          "-std=gnu89",
          "-I" + TargetLinuxSource + "/include",
      };
    }

    // 3c) Build a new compile DB with those edited args
    clang::tooling::FixedCompilationDatabase TargetCDB(TargetLinuxSource,
                                                       EditedArgs);

    // 3d) For each header in UsedHeaders, map old => new
    std::vector<std::string> TargetHeaderFiles;
    TargetHeaderFiles.reserve(UsedHeaders.size());
    for (auto &H : UsedHeaders) {
      TargetHeaderFiles.push_back(mapOldToNew(H, OldBase, TargetLinuxSource));
    }

    // 3e) Parse them, collecting function prototypes
    if (RunToolWithDB(TargetCDB, TargetHeaderFiles, SymbolsToCheck,
                      NewFunctions)) {
      llvm::WithColor::error() << "Error processing target linux headers.\n";
      return 1;
    }
  } else {
    llvm::WithColor::warning() << "No headers discovered for "
                               << LKMFileToAnalyze << " (empty set).\n";
  }

  // 4) Compare
  CompareFunctions(OldFunctions, NewFunctions);
  return 0;
}
