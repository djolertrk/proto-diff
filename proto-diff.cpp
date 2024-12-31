// A tool for source code analysis.
//
// Author: Djordje Todorovic
//

#include "clang/AST/ASTContext.h"
#include "clang/AST/RecursiveASTVisitor.h"
#include "clang/Frontend/CompilerInstance.h"
#include "clang/Frontend/FrontendAction.h"
#include "clang/Tooling/CommonOptionsParser.h"
#include "clang/Tooling/Tooling.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/WithColor.h"
#include "llvm/Support/raw_ostream.h"

using namespace clang;
using namespace clang::tooling;
using namespace llvm;

// Struct to hold function details
struct FunctionInfo {
  std::string Name;
  std::string ReturnType;
  std::vector<std::string> Parameters;
};

// Visitor to collect function declarations
class FunctionDeclVisitor : public RecursiveASTVisitor<FunctionDeclVisitor> {
public:
  explicit FunctionDeclVisitor(ASTContext *Context,
                               std::vector<FunctionInfo> &Functions)
      : Context(Context), Functions(Functions) {}

  bool VisitFunctionDecl(FunctionDecl *FD) {
    if (Context->getSourceManager().isInMainFile(FD->getLocation())) {
      FunctionInfo Info;
      Info.Name = FD->getNameAsString();
      Info.ReturnType = FD->getReturnType().getAsString();

      for (const auto *Param : FD->parameters()) {
        Info.Parameters.push_back(Param->getType().getAsString());
      }

      Functions.push_back(Info);
    }
    return true;
  }

private:
  ASTContext *Context;
  std::vector<FunctionInfo> &Functions;
};

// ASTConsumer that drives the visitor
class FunctionDeclASTConsumer : public ASTConsumer {
public:
  FunctionDeclASTConsumer(std::vector<FunctionInfo> &Functions,
                          ASTContext &Context)
      : Visitor(&Context, Functions) {}

  void HandleTranslationUnit(ASTContext &Context) override {
    // Traverse the entire translation unit.
    Visitor.TraverseDecl(Context.getTranslationUnitDecl());
  }

private:
  FunctionDeclVisitor Visitor;
};

// FrontendAction to initiate AST traversal
class FunctionDeclAction : public ASTFrontendAction {
public:
  explicit FunctionDeclAction(std::vector<FunctionInfo> &Functions)
      : Functions(Functions) {}

  std::unique_ptr<ASTConsumer> CreateASTConsumer(CompilerInstance &CI,
                                                 StringRef File) override {
    // Return our custom consumer for the translation unit
    return std::make_unique<FunctionDeclASTConsumer>(Functions,
                                                     CI.getASTContext());
  }

private:
  std::vector<FunctionInfo> &Functions;
};

// Custom FrontendActionFactory
class FunctionDeclActionFactory : public FrontendActionFactory {
public:
  explicit FunctionDeclActionFactory(std::vector<FunctionInfo> &Functions)
      : Functions(Functions) {}

  std::unique_ptr<FrontendAction> create() override {
    return std::make_unique<FunctionDeclAction>(Functions);
  }

private:
  std::vector<FunctionInfo> &Functions;
};

// Function to compare two sets of function declarations
void CompareFunctions(const std::vector<FunctionInfo> &File1Functions,
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

int main(int argc, const char **argv) {
  cl::OptionCategory ToolCategory("proto-diff options");
  auto ExpectedParser = CommonOptionsParser::create(argc, argv, ToolCategory);
  if (!ExpectedParser) {
    WithColor::error() << "[Failed to parse arguments] "
                       << toString(ExpectedParser.takeError()) << "\n";
    return 1;
  }
  CommonOptionsParser &OptionsParser = *ExpectedParser;
  ClangTool Tool(OptionsParser.getCompilations(),
                 OptionsParser.getSourcePathList());

  std::vector<FunctionInfo> File1Functions;
  std::vector<FunctionInfo> File2Functions;

  // First file processing
  if (Tool.run(new FunctionDeclActionFactory(File1Functions))) {
    WithColor::error() << "Error processing the first file.";
    return 1;
  }

  // Second file processing
  if (Tool.run(new FunctionDeclActionFactory(File2Functions))) {
    WithColor::error() << "Error processing the second file.";
    return 1;
  }

  // Compare functions from both files
  CompareFunctions(File1Functions, File2Functions);

  return 0;
}
