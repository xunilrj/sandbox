//===- PrintFunctionNames.cpp ---------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// Example clang plugin which simply prints the names of all the top-level decls
// in the input file.
//
//===----------------------------------------------------------------------===//

#include <memory>
#include <vector>
#include <sstream>
#include <fstream>
#include "json.hpp"
using json = nlohmann::json;

#include "clang/Frontend/FrontendPluginRegistry.h"
#include "clang/AST/AST.h"
#include "clang/AST/ASTConsumer.h"
#include "clang/AST/RecordLayout.h"
#include "clang/AST/RecursiveASTVisitor.h"
#include "clang/Frontend/CompilerInstance.h"
#include "clang/Sema/Sema.h"
#include "llvm/Support/raw_ostream.h"
using namespace clang;

llvm::raw_ostream &operator<<(llvm::raw_ostream &out, json &j)
{
  std::stringstream ss;
  ss << j.dump(4);
  return out << ss.str();
}

namespace
{

  json exported;

  //https://clang.llvm.org/doxygen/classclang_1_1QualType.html
  //https://clang.llvm.org/doxygen/classclang_1_1Type.html
  //https://clang.llvm.org/doxygen/classclang_1_1ASTContext.html
  //https://clang.llvm.org/doxygen/classclang_1_1ConstantArrayType.html
  //http://llvm.org/doxygen/classllvm_1_1APInt.html
  json toJson(const QualType &type, ASTContext &ctx)
  {
    auto isArray = type->isArrayType();
    auto *array = ctx.getAsConstantArrayType(type);
    uint64_t arraySize = 1;
    if (isArray)
    {
      arraySize = *array->getSize().getRawData();
    }

    auto typeInfo = ctx.getTypeInfo(type);
    uint64_t typeSize = typeInfo.Width / 8 / arraySize;

    return {
        {"type", type.getAsString()},
        {"isReference", type->isReferenceType()},
        {"isPointer", type->isPointerType()},
        {"isArray", type->isArrayType()},
        {"arraySize", arraySize},
        {"size", typeSize},
    };
  }

#include <signal.h>
#define BREAK_HERE raise(SIGINT)

  class ExtractContractASTVisitor
      : public RecursiveASTVisitor<ExtractContractASTVisitor>
  {
    CompilerInstance &compiler;

  public:
    ExtractContractASTVisitor(CompilerInstance &compiler)
        : compiler{compiler}
    {
    }

    //https://clang.llvm.org/doxygen/classclang_1_1FunctionDecl.html
    bool VisitFunctionDecl(FunctionDecl *D)
    {
      std::string name;

      auto shouldExport = false;

      //https://clang.llvm.org/doxygen/classclang_1_1Attr.html
      auto attrs = D->getAttrs();
      for (auto &&x : attrs)
      {
        if (x->getKind() == clang::attr::Kind::WebAssemblyExportName)
        {
          WebAssemblyExportNameAttr *xx = (WebAssemblyExportNameAttr *)x;
          name = xx->getExportName();
          shouldExport = true;
        }
      }

      if (!shouldExport)
        return true;

      // D->dump();

      auto &astCtx = D->getASTContext();
      auto hasReturn = !D->isNoReturn();
      //https://clang.llvm.org/doxygen/classclang_1_1QualType.html
      auto returnType = D->getReturnType()
                            .getLocalUnqualifiedType()
                            .getCanonicalType();

      auto *typePtr = returnType.getTypePtr();
      if (typePtr->isRecordType())
      {
        auto decl = typePtr->getAsCXXRecordDecl();
        pushRecord(decl, &returnType);
      }

      json f = {
          {"name", name},
          {"returnType", toJson(returnType, astCtx)},
      };

      auto parameters = D->parameters();
      for (auto p : parameters)
      {
        auto name = p->getNameAsString();
        auto original_type = p->getOriginalType();
        auto unqual_type = original_type.getLocalUnqualifiedType();
        auto type = unqual_type.getCanonicalType();

        // type->dump();

        f["arguments"].push_back({{"name", name},
                                  {"type", toJson(type, astCtx)}});
      }

      exported["functions"].push_back(f);

      //auto& astCtx = D->getASTContext();
      // auto opts = astCtx.getLangOpts ();
      // auto attrs = D->getAttrs();
      // for(auto a : attrs) {
      //   auto txt = Lexer::getSourceText(
      //     CharSourceRange::getTokenRange(a->getRange()),
      //     compiler.getSourceManager(),
      //     opts).str().c_str();
      //   llvm::errs() << a->getSpelling() << " " << txt << "\n";
      // }
      return true;
    }

    //   bool VisitVarDecl(VarDecl *VD) {
    //     std::pair<uint64_t, unsigned> FieldInfo = VD->getASTContext().getTypeInfo(VD->getType());
    //     uint64_t TypeSize = FieldInfo.first;
    //     unsigned FieldAlign = FieldInfo.second;
    //     llvm::outs() << VD->getNameAsString() << " Size: " << TypeSize/8 << " Alignment: " << FieldAlign/8 << '\n';
    // }

    //https://clang.llvm.org/doxygen/classclang_1_1RecordDecl.html
    //https://clang.llvm.org/doxygen/classclang_1_1ClassTemplateSpecializationDecl.html
    //https://clang.llvm.org/doxygen/DeclBase_8cpp_source.html#l00380
    //https://clang.llvm.org/doxygen/classclang_1_1ASTContext.html
    bool VisitRecordDecl(const RecordDecl *D)
    {
      if (D->isTemplated())
      {
        return true;
      }

      if (!D->isCompleteDefinition())
        return true;

      // pushRecord(D);

      return true;
    }

    void pushRecord(const RecordDecl *D, const QualType *t)
    {
      auto name = D->getQualifiedNameAsString();
      auto &astCtx = D->getASTContext();
      auto &typeLayout = astCtx.getASTRecordLayout(D);
      auto typeSize = typeLayout.getSize().getQuantity();

      if (t != nullptr)
        name = t->getAsString();

      json r = {
          {"name", name},
          {"sizeBytes", typeSize}};

      // llvm::errs() << "record '" << name << "' with " << typeSize << " bytes\n";

      //https://clang.llvm.org/doxygen/classclang_1_1FieldDecl.html

      for (auto &&field : D->fields())
      {
        auto qualType = field->getType()
                            .getLocalUnqualifiedType()
                            .getCanonicalType();

        // qualType->dump();

        if (qualType->isRecordType())
        {
          auto *type_ptr = qualType.getTypePtr();
          auto decl = type_ptr->getAsCXXRecordDecl();
          pushRecord(decl, &qualType);
        }
        else if (qualType->isArrayType())
        {
          auto *array = qualType->getAsArrayTypeUnsafe();
          auto element_type = array->getElementType();
          auto *type_ptr = element_type.getTypePtr();
          auto *decl = type_ptr->getAsCXXRecordDecl();
          pushRecord(decl, &element_type);
        }

        auto typeInfo = astCtx.getTypeInfo(qualType);
        uint64_t typeSize = typeInfo.Width;
        size_t fieldOffset = typeLayout.getFieldOffset(
            field->getFieldIndex());

        r["fields"].push_back({{"name", field->getNameAsString()},
                               {"type", toJson(qualType, astCtx)},
                               {"size", typeSize / 8},
                               {"offset", fieldOffset / 8}});
      }

      exported["records"].push_back(r);
    }

    // bool VisitCXXRecordDecl(CXXRecordDecl *Declaration) {
    //   // For debugging, dumping the AST nodes will show which nodes are already
    //   // being visited.
    //   Declaration->dump();

    //   // The return value indicates whether we want the visitation to proceed.
    //   // Return false to stop the traversal of the AST.
    //   return true;
    // }
  };

  struct Arguments
  {
    std::string jsonOutFile;
  };
  Arguments Args;

  class ExtractContractASTConsumer : public ASTConsumer
  {
  public:
    ExtractContractASTConsumer(CompilerInstance &compiler)
        : Visitor{compiler}
    {
    }

    virtual void HandleTranslationUnit(
        clang::ASTContext &Context)
    {
      auto tu = Context.getTranslationUnitDecl();
      Visitor.TraverseDecl(tu);

      // llvm::errs() << "OUT: " << Args.jsonOutFile;
      std::ofstream jsonFile;
      jsonFile.open(Args.jsonOutFile);
      jsonFile << exported.dump(4);
      jsonFile.close();
    }

  private:
    ExtractContractASTVisitor Visitor;
  };

  class ExtractContractAction
      : public PluginASTAction
  {

    std::set<std::string> ParsedTemplates;

  protected:
    bool ParseArgs(const CompilerInstance &CI, const std::vector<std::string> &args) override
    {
      // llvm::errs() << "ExtractContractAction::ParseArgs: " << args[0] << "\n";
      Args.jsonOutFile = args[0];
      return true;
    }

    std::unique_ptr<ASTConsumer> CreateASTConsumer(
        CompilerInstance &compiler,
        llvm::StringRef) override
    {
      return std::make_unique<ExtractContractASTConsumer>(compiler);
    }
  };

} // namespace

static FrontendPluginRegistry ::Add<ExtractContractAction> wasmExtractContract(
    "wasm-extract-contract",
    "Extract contract info for WASM.");
