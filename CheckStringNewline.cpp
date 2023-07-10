// Copyright (c) 2023 Cory Fields
// Distributed under the MIT software license, see the accompanying
// file COPYING or http://www.opensource.org/licenses/mit-license.php.

#include "clang/AST/ASTContext.h"
#include "clang/AST/Attr.h"
#include "clang/Sema/ParsedAttr.h"
#include "clang/Sema/Sema.h"
#include "clang/Sema/SemaDiagnostic.h"
#include "llvm/IR/Attributes.h"

#include "clang/AST/RecursiveASTVisitor.h"
#include "clang/Frontend/FrontendPluginRegistry.h"


// This is a VERY ugly first attempt at a plugin that uses attributes rather
// than hard-coded knowledge of code. It is purely experimental and mostly
// copied/pasted from other places. No attempt has been made to make this tidy
// or readable.

using namespace clang;

namespace {

/* Mostly borrowed from llvm's SemaDeclAttr.cpp */

/// If Expr is a valid integer constant, get the value of the integer
/// expression and return success or failure. May output an error.
///
/// Negative argument is implicitly converted to unsigned, unless
/// \p StrictlyUnsigned is true.
template <typename AttrInfo>
static bool checkUInt32Argument(Sema &S, const AttrInfo &AI, const Expr *Expr,
                                uint32_t &Val, unsigned Idx = UINT_MAX,
                                bool StrictlyUnsigned = false) {
  std::optional<llvm::APSInt> I = llvm::APSInt(32);
  if (Expr->isTypeDependent() ||
      !(I = Expr->getIntegerConstantExpr(S.Context))) {
    if (Idx != UINT_MAX)
      S.Diag(AI.getLoc(), diag::err_attribute_argument_n_type)
          << &AI << Idx << AANT_ArgumentIntegerConstant
          << Expr->getSourceRange();
    else
      S.Diag(AI.getLoc(), diag::err_attribute_argument_type)
          << &AI << AANT_ArgumentIntegerConstant << Expr->getSourceRange();
    return false;
  }

  if (!I->isIntN(32)) {
    S.Diag(Expr->getExprLoc(), diag::err_ice_too_large)
        << toString(*I, 10, false) << 32 << /* Unsigned */ 1;
    return false;
  }

  if (StrictlyUnsigned && I->isSigned() && I->isNegative()) {
    S.Diag(AI.getLoc(), diag::err_attribute_requires_positive_integer)
        << &AI << /*non-negative*/ 1;
    return false;
  }

  Val = (uint32_t)I->getZExtValue();
  return true;
}

bool CheckStringLiteralNewline(const StringLiteral& Node)
{
    size_t len = Node.getLength();
    if(len == 0) {
        return true;
    }
    if(Node.getCodeUnit(len-1) == '\n') {
        return true;
    }
    if (len > 2 &&
        Node.getCodeUnit(len-1) == '.' &&
        Node.getCodeUnit(len-2) == '.' &&
        Node.getCodeUnit(len-3) == '.')
    {
        return true;
    }
    return false;
}

struct NewlineAttrInfo : public ParsedAttrInfo {
  NewlineAttrInfo() {
    NumArgs = 1;
    static constexpr Spelling S[] = {{ParsedAttr::AS_GNU, "require_newline"},
                                     {ParsedAttr::AS_C2x, "require_newline"},
                                     {ParsedAttr::AS_CXX11, "require_newline"}};
    Spellings = S;
  }

  bool diagAppertainsToDecl(Sema &S, const ParsedAttr &Attr,
                            const Decl *D) const override {
    // This attribute appertains to functions only.
    if (!isa<FunctionDecl>(D)) {
      S.Diag(Attr.getLoc(), diag::warn_attribute_wrong_decl_type_str)
          << Attr << "functions";
      return false;
    }
    return true;
  }
  AttrHandling handleDeclAttribute(Sema &S, Decl *D,
                                   const ParsedAttr &Attr) const override {
    Expr *E = Attr.getArgAsExpr(0);
    uint32_t ArgNo;
    if (!checkUInt32Argument(S, Attr, E, ArgNo, /*Idx=*/1)) {
        unsigned ID = S.getDiagnostics().getCustomDiagID(
            DiagnosticsEngine::Error, "first argument to the 'require_newline' "
                                      "attribute must be an integer");
        S.Diag(Attr.getLoc(), ID);
        return AttributeNotApplied;
    }

    SmallVector<Expr *, 16> ArgsBuf;
    ArgsBuf.push_back(E);
    D->addAttr(AnnotateAttr::Create(S.Context, "require_newline", ArgsBuf.data(),
                                      ArgsBuf.size(), Attr.getRange()));
    return AttributeApplied;
  }
};

class DeclVisitor : public RecursiveASTVisitor<DeclVisitor> {
public:
  DeclVisitor(ASTContext &Context) : Context(Context){}
  bool VisitCallExpr(CallExpr *CallExpr) {
    auto& Diags = Context.getDiagnostics();
    auto* decl = CallExpr->getDirectCallee();

    if (!decl) return true;
    assert(decl);

    int arg = 0;
    Expr* argExpr = nullptr; 
    auto attrs = decl->getAttrs();
    for (auto attr : attrs) {
        if (auto *AA = dyn_cast<AnnotateAttr>(attr)) {
            argExpr = *AA->args_begin();
            break;
        }
    }
    if (!argExpr) {
        return true;
    }
    std::optional<llvm::APSInt> argInt = argExpr->getIntegerConstantExpr(Context);
    arg = (uint32_t)argInt->getZExtValue();

    //TODO: This is probably off-by-one for c++ member functions
    if ((arg < 1) || (arg > CallExpr->getNumArgs())) {
      unsigned WarningBadIndex = Diags.getCustomDiagID(DiagnosticsEngine::Error, "bad index for attribute");
      Diags.Report(decl->getLocation(), WarningBadIndex);
      return false;
    }
    const Expr* callExprArg = CallExpr->getArg(arg - 1);
    assert(callExprArg);

    const StringLiteral* foundLiteral = dyn_cast<StringLiteral>(callExprArg->IgnoreParenCasts());
    if (!foundLiteral) {
      if (const DeclRefExpr* declRef = dyn_cast<DeclRefExpr>(callExprArg->IgnoreParenCasts())) {
        auto foundDecl = declRef->getFoundDecl();
        if (const auto *StrVD = dyn_cast<VarDecl>(declRef->getDecl()))
          if (const Expr *StrInit = StrVD->getInit())
            if (const auto *StrSL = dyn_cast<StringLiteral>(StrInit->IgnoreImpCasts())) {
              foundLiteral = StrSL;
            }
      }
    }
    if (!foundLiteral)
        return true;

    if (!CheckStringLiteralNewline(*foundLiteral)) {
      unsigned WarningNewLine = Diags.getCustomDiagID(DiagnosticsEngine::Warning, "does not end in newline");
      Diags.Report(foundLiteral->getEndLoc(), WarningNewLine);
    }
    return true;
  }
private:
    ASTContext &Context;
};

class CheckStringLiteralConsumer : public ASTConsumer {
public:
  void HandleTranslationUnit(ASTContext &Context) override {
    DeclVisitor Visitor(Context);
    Visitor.TraverseDecl(Context.getTranslationUnitDecl());
  }
};

class CheckStringLiteralAction : public PluginASTAction {
public:
  std::unique_ptr<ASTConsumer> CreateASTConsumer(CompilerInstance &CI,
                                                 llvm::StringRef) override {
    return std::make_unique<CheckStringLiteralConsumer>();
  }

  bool ParseArgs(const CompilerInstance &CI,
                 const std::vector<std::string> &args) override {
    return true;
  }

  PluginASTAction::ActionType getActionType() override {
    return AddBeforeMainAction;
  }
};

} // namespace

static ParsedAttrInfoRegistry::Add<NewlineAttrInfo> X("require_newline", "");

static FrontendPluginRegistry::Add<CheckStringLiteralAction>
    Y("require_newline_plugin", "clang plugin, checks string literals in annotated functions for newlines");

