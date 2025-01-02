// A tool for source code analysis.
//
// Author: Djordje Todorovic
//
// This tool demonstrates:
//   1) Collecting the function prototypes from the "current" LKM environment
//      by parsing all files in compile_commands.json.
//   2) Collecting the #includes used by a specific LKM source (e.g. sys.c)
//      via PPCallbacks, so Clang tells us which headers are actually included.
//   3) Mapping those old-environment header paths to the "target"
//   kernel-headers
//      directory, then parsing them *as standalone TUs* to gather function
//      decls.
//   4) Comparing the two sets of function prototypes.
//
// The key differences for Clang 18 are:
//   - We override `InclusionDirective(...)` instead of `FileIncluded(...)`.
//   - `BeginSourceFileAction(...)` must return `bool`, not `void`.
//   - `File` in `InclusionDirective` is `OptionalFileEntryRef`; we check
//   `File.has_value()`.
//
// USAGE (example):
//   cd /path/to/LKM/build    (has compile_commands.json)
//   ./proto-diff -p=. \
//       --symbols-file=input_symbols.txt \
//       --target-linux-source=/usr/src/linux-headers-6.8.0-foo

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
#include "llvm/ADT/Optional.h"
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
    SymbolsFile("symbols-file", llvm::cl::desc("Path to input_symbols.txt"),
                llvm::cl::value_desc("filename"), llvm::cl::cat(ToolCategory));

llvm::cl::opt<std::string>
    TargetLinuxSource("target-linux-source",
                      llvm::cl::desc("Path to new (target) linux-headers "
                                     "directory (no compile_commands.json)"),
                      llvm::cl::value_desc("path"),
                      llvm::cl::cat(ToolCategory));

// Which .c file to gather #includes from:
llvm::cl::opt<std::string> LKMFileToAnalyze(
    "lkm-file",
    llvm::cl::desc(
        "Which .c file in compile_commands.json to gather includes from"),
    llvm::cl::value_desc("file"),
    llvm::cl::init("src/sys.c"), // example default
    llvm::cl::cat(ToolCategory));

// If you want to parse the target headers as C or C++:
llvm::cl::opt<std::string> TargetLanguage(
    "target-lang",
    llvm::cl::desc("Language for parsing target headers (c, c-header, c++...)"),
    llvm::cl::init("c"), // default to C
    llvm::cl::cat(ToolCategory));
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
    // If we have symbols to check, skip anything not in that list
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
// 4) PPCallbacks-based approach to gather #includes
//    Note: for Clang 18, we override InclusionDirective, not FileIncluded.
//---------------------------------------------------------------------------//

class IncludeCollector : public clang::PPCallbacks {
public:
  IncludeCollector(clang::SourceManager &SM, std::set<std::string> &Collected)
      : SM(SM), Collected(Collected) {}

  // Clang 18+ signature for includes:
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
    // Get the actual FileEntry
    auto &FE = File->getFileEntry();
    // Insert the absolute path into the set
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
    return std::make_unique<clang::ASTConsumer>();
  }

  // Must return bool in Clang 18+ (was void in older versions)
  bool BeginSourceFileAction(clang::CompilerInstance &CI) override {
    clang::Preprocessor &PP = CI.getPreprocessor();
    PP.addPPCallbacks(
        std::make_unique<IncludeCollector>(CI.getSourceManager(), Collected));
    return true; // indicate success
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
// 6) Helper: run a ClangTool with a given CompilationDatabase & sources
//---------------------------------------------------------------------------//

static bool RunToolWithDB(clang::tooling::CompilationDatabase &CompDB,
                          const std::vector<std::string> &Sources,
                          const std::vector<std::string> &SymbolsToCheck,
                          std::vector<FunctionInfo> &OutFunctions) {
  clang::tooling::ClangTool Tool(CompDB, Sources);
  auto Factory =
      std::make_unique<FunctionDeclActionFactory>(OutFunctions, SymbolsToCheck);
  return (Tool.run(Factory.get()) != 0); // returns true on error
}

//---------------------------------------------------------------------------//
// 7) Example: Map old header path => new header path
//---------------------------------------------------------------------------//

static std::string mapOldToNew(const std::string &OldPath,
                               const std::string &OldBase,
                               const std::string &NewBase) {
  // If OldPath starts with OldBase, replace that portion.
  // This is naive; real logic may be more complicated.
  if (OldPath.find(OldBase) == 0) {
    std::string subpath = OldPath.substr(OldBase.size());
    return NewBase + subpath;
  }
  // fallback: just re-root in NewBase
  return NewBase + OldPath;
}

//---------------------------------------------------------------------------//
// 8) Main
//---------------------------------------------------------------------------//

int main(int argc, const char **argv) {
  // Parse the "current" environment from the LKM build dir
  auto ExpectedParser =
      clang::tooling::CommonOptionsParser::create(argc, argv, ToolCategory);
  if (!ExpectedParser) {
    llvm::WithColor::error()
        << "[Failed to parse arguments] "
        << llvm::toString(ExpectedParser.takeError()) << "\n";
    return 1;
  }
  clang::tooling::CommonOptionsParser &CurrentKernelOptions = *ExpectedParser;

  // We require -target-linux-source
  if (TargetLinuxSource.empty()) {
    llvm::WithColor::error()
        << "No -target-linux-source specified. This argument is required.\n";
    return 1;
  }

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

  // 1) Gather function prototypes from the "current" LKM build environment
  //    by parsing *all* files in compile_commands.json, or pick what you want.
  std::vector<FunctionInfo> OldFunctions;
  {
    auto &DB = CurrentKernelOptions.getCompilations();
    std::vector<std::string> AllFiles = DB.getAllFiles();
    if (AllFiles.empty()) {
      llvm::WithColor::warning()
          << "No files found in the current compile_commands.json.\n";
    }

    if (RunToolWithDB(DB, AllFiles, SymbolsToCheck, OldFunctions)) {
      llvm::WithColor::error() << "Error processing current (LKM) environment "
                                  "function prototypes.\n";
      return 1;
    }
  }

  // 2) Collect includes from the file we want to analyze (e.g. "src/sys.c")
  std::set<std::string> UsedHeaders;
  {
    auto &DB = CurrentKernelOptions.getCompilations();

    // Find the .c file in DB
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
            << " in compile_commands.json. Will pick first file.\n";
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
      llvm::WithColor::warning() << "No LKM file to analyze for includes!\n";
    }
  }

  // 3) Map those used headers to the target kernel, parse them
  std::vector<FunctionInfo> NewFunctions;
  if (!UsedHeaders.empty()) {
    std::string OldBase = "/usr/src/linux-headers-5.15.0-127-generic";
    // In a real scenario, detect or store old base from compile_commands.json

    std::vector<std::string> TargetHeaderFiles;
    TargetHeaderFiles.reserve(UsedHeaders.size());
    for (auto &H : UsedHeaders) {
      TargetHeaderFiles.push_back(mapOldToNew(H, OldBase, TargetLinuxSource));
    }

    // We'll parse these .h files as standalone TUs in the target environment
    std::vector<std::string> BaseFlags = {
        "clang",        "-x",
        TargetLanguage, // e.g., "c" or "c-header"
        "-std=gnu89",   "-I" + TargetLinuxSource + "/include",
        // add more -I or -D as needed
    };
    clang::tooling::FixedCompilationDatabase TargetCDB(TargetLinuxSource,
                                                       BaseFlags);

    if (RunToolWithDB(TargetCDB, TargetHeaderFiles, SymbolsToCheck,
                      NewFunctions)) {
      llvm::WithColor::error() << "Error processing target linux headers.\n";
      return 1;
    }
  } else {
    llvm::WithColor::warning() << "No headers used by " << LKMFileToAnalyze
                               << " were discovered. (Empty set)\n";
  }

  // 4) Compare
  CompareFunctions(OldFunctions, NewFunctions);
  return 0;
}
