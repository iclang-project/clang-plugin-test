#include <memory>
#include <string>
#include <vector>
#include <sstream>

#include "clang/Frontend/FrontendPluginRegistry.h"
#include "clang/AST/AST.h"
#include "clang/AST/ASTConsumer.h"
#include "clang/AST/RecursiveASTVisitor.h"
#include "clang/AST/Mangle.h"
#include "clang/Frontend/CompilerInstance.h"
#include "clang/Sema/Sema.h"
#include "llvm/Support/raw_ostream.h"
#include "clang/Lex/Preprocessor.h"
#include "clang/Lex/PPCallbacks.h"

#include "llvm/Support/FileSystem.h"
#include "llvm/Support/Path.h"

using namespace clang;

namespace {
    struct ExRange {
        bool isValid;
        unsigned startLine, startColumn, endLine, endColumn;
        unsigned startOffset, endOffset;
        unsigned fileIdHash;

        [[nodiscard]] std::string toString() const {
            if (!isValid) {
                return "<invalid>";
            } else {
                std::ostringstream oss;
                oss << "<" << startLine << ":" << startColumn << "," << endLine << ":" << endColumn << ">" <<
                    "(" << startOffset << "," << endOffset << ")[" << fileIdHash << "]";
                return oss.str();
            }
        }
    };

    class ExRangeManager {
    public:
        ASTContext &context;
        SourceManager &sourceManager;

        explicit ExRangeManager(const CompilerInstance &instance) : context(instance.getASTContext()),
                sourceManager(instance.getSourceManager()) {}

        [[nodiscard]] std::unique_ptr<ExRange> dumpExpansionRange(const SourceRange &sourceRange) const {
            auto res = std::make_unique<ExRange>();

            res->isValid = false;

            // start
            const FullSourceLoc startFullSourceLoc(sourceRange.getBegin(), sourceManager);
            if (startFullSourceLoc.isInvalid()) {
                return res;
            }
            res->startLine = startFullSourceLoc.getExpansionLineNumber();
            res->startColumn = startFullSourceLoc.getExpansionColumnNumber();

            // end
            const SourceLocation endSourceLoc = Lexer::getLocForEndOfToken(sourceRange.getEnd(), 0, sourceManager,
                                                                     context.getLangOpts());
            const FullSourceLoc endFullSourceLoc(endSourceLoc, sourceManager);
            if (endFullSourceLoc.isInvalid()) {
                return res;
            }
            res->endLine = endFullSourceLoc.getExpansionLineNumber();
            res->endColumn = endFullSourceLoc.getExpansionColumnNumber();

            res->isValid = true;

            res->startOffset = startFullSourceLoc.getFileOffset();
            res->endOffset = endFullSourceLoc.getFileOffset();

            res->fileIdHash = startFullSourceLoc.getFileID().getHashValue();

            return res;
        }
    };

    class MyPPCallbacks final : public PPCallbacks {
    public:
        explicit MyPPCallbacks(const CompilerInstance &CI) : em(CI), fm(CI.getFileManager()) {}

        void InclusionDirective(SourceLocation HashLoc,
                                          const Token &IncludeTok, StringRef FileName,
                                          bool IsAngled, CharSourceRange FilenameRange,
                                          OptionalFileEntryRef File,
                                          StringRef SearchPath, StringRef RelativePath,
                                          const Module *Imported,
                                          SrcMgr::CharacteristicKind FileType) override {
            std::string pos;
            if (const FullSourceLoc startFullSourceLoc(HashLoc, em.sourceManager); !startFullSourceLoc.isInvalid()) {
                const unsigned fileIdHash = startFullSourceLoc.getFileID().getHashValue();
                const unsigned startLine = startFullSourceLoc.getExpansionLineNumber();
                const unsigned startColumn = startFullSourceLoc.getExpansionColumnNumber();
                pos = "<" + std::to_string(startLine) + "," + std::to_string(startColumn) + ">[" + std::to_string(fileIdHash) + "]";
            } else {
                pos = "<>[]";
            }
            llvm::errs() << pos << "Included: " << FileName.str() << ", dir:" << File->getFileEntry().getName() << "\n";

            auto t = File->getFileEntry().getModificationTime();
            llvm::errs() << "time: " << t << "\n";
            // time_t time = std::chrono::system_clock::to_time_t(t);

            std::string ResolvedPath =  File->getFileEntry().getName().str();

            llvm::SmallString<256> AbsolutePath(ResolvedPath);
            if (llvm::sys::fs::make_absolute(AbsolutePath)) {
                llvm::errs() << "!!!!!!!!!!!!!!!!!!!!\n";
                exit(1);
            }
            llvm::errs() << "dot abs:" << AbsolutePath << "\n";
            llvm::sys::path::remove_dots(AbsolutePath); // 可选：移除 . 和 ..
            llvm::errs() << "abs:" << AbsolutePath << "\n";
        }

    private:
        ExRangeManager em;
        FileManager &fm;
    };

    class MyVisitor : public RecursiveASTVisitor<MyVisitor> {
    public:
        ASTContext &context;
        ExRangeManager em;
        ASTNameGenerator astNameGenerator;
        int depth = 0;

        explicit MyVisitor(const CompilerInstance &instance) :
                context(instance.getASTContext()),
                em(instance),
                astNameGenerator(instance.getASTContext()) {}

        bool shouldVisitTemplateInstantiations() const { return true; }

        bool TraverseDecl(Decl *decl) {
            if (!decl) {
                return true;
            }

            depth += 1;
            const std::string space = calSpaces(depth);

            llvm::errs() << space << dumpDecl(decl) << " ";

            if (auto *typedefDecl = dyn_cast<TypedefDecl>(decl)) {
                llvm::errs() << "first:" << typedefDecl->getFirstDecl() << " ";
            } else if (auto *typeAliasDecl = dyn_cast<TypeAliasDecl>(decl)) {
                llvm::errs() << "first:" << typeAliasDecl->getFirstDecl() << " ";
            } else if (auto *namespaceAliasDecl = dyn_cast<NamespaceAliasDecl>(decl)) {
                llvm::errs() << "first:" << namespaceAliasDecl->getFirstDecl() << " ";
            } else if (auto *functionDecl = dyn_cast<FunctionDecl>(decl)) {
                llvm::errs() << "first:" << (void *) functionDecl->getFirstDecl() << " ";
                llvm::errs() << "def:" << (void *) functionDecl->getDefinition() << " ";

                llvm::errs() << "namePrefix:{" << expandPrefix(functionDecl) << "} ";
                llvm::errs() << "qName:" << functionDecl->getQualifiedNameAsString() << " ";
                llvm::errs() << "nameSR:{" <<
                             em.dumpExpansionRange(functionDecl->getQualifierLoc().getSourceRange())->toString() << ", " <<
                             em.dumpExpansionRange(functionDecl->getNameInfo().getSourceRange())->toString() << "} ";

                // clang/lib/AST/TypePrinter.cpp
                auto retType = functionDecl->getReturnType();
                llvm::errs() << "rType:{" << retType->getTypeClassName() << " " <<
                             em.dumpExpansionRange(functionDecl->getReturnTypeSourceRange())->toString() << " ";
                if (const auto typeClass = retType->getTypeClass(); typeClass == Type::Pointer) {
                    const auto *pointerType = llvm::cast<PointerType>(retType);
                    llvm::errs() << "->" << pointerType->getPointeeType()->getTypeClassName() << " ";
                } else if (typeClass == Type::Builtin) {
                    // ...
                } else if (typeClass == Type::Elaborated) {
                    const auto *elaboratedType = llvm::cast<ElaboratedType>(retType);
                    const QualType namedType = elaboratedType->getNamedType();
                    llvm::errs() << "| nameType: " << namedType->getTypeClassName() << " ";
                    Decl *nameTarget = nullptr;
                    if (const TypedefType *typedefType = namedType->getAs<TypedefType>()) {
                        nameTarget = typedefType->getDecl();
                    } else if (const auto *tagType = namedType->getAs<TagType>()) {
                        nameTarget = tagType->getDecl();
                    }
                    llvm::errs() << "| nameTarget: " << (void *) nameTarget << " ";
                    if (nameTarget != nullptr) {
                        llvm::errs() << "'" << expandPrefix(nameTarget) << "' ";
                    }
                    auto *target = elaboratedType->getAsTagDecl();
                    llvm::errs() << "| target: " << (void *) target << " ";
                    if (target != nullptr) {
                        llvm::errs() << "'" << expandPrefix(target) << "' ";
                    }
                }
                llvm::errs() << "} ";

                llvm::errs() << "Params:{";
                for (unsigned i = 0; i < functionDecl->getNumParams(); ++i) {
                    const ParmVarDecl *param = functionDecl->getParamDecl(i);
                    llvm::errs() << " @" << param->getNameAsString();
                    if (param->hasDefaultArg()) {
                        llvm::errs() << " (default value present)";
                    }
                }
                llvm::errs() << " } ";

                llvm::errs() << "attrs:{ ";
                if (functionDecl->hasAttrs()) {
                    for (auto &attr: functionDecl->attrs()) {
                        if (attr->getLocation().isValid()) {
                            const SourceLocation AttrBegin = attr->getLocation();
                            if (const FullSourceLoc fl(AttrBegin, em.sourceManager); fl.isValid()) {
                                llvm::errs() << "<" << fl.getExpansionLineNumber() << ","
                                             << fl.getExpansionColumnNumber() << "> ";
                            }
                        }
                        if (attr->getAttrName()) {
                            llvm::errs() << attr->getAttrName()->getName() << " ";
                        } else if (isa<AsmLabelAttr>(attr)) {
                            llvm::errs() << "__asm__ ";
                        }
                    }
                }
                llvm::errs() << "} ";

                llvm::errs() << "temp: {" << functionDecl->isTemplateInstantiation() << " "
                << functionDecl->getTemplatedKind() << "} ";
                llvm::errs() << "PriTemp:" << (void *) functionDecl->getPrimaryTemplate() << " ";
                llvm::errs() << "DesTemp:" << (void *) functionDecl->getDescribedFunctionTemplate() << " ";
                const auto linkInfo = functionDecl->getLinkageAndVisibility();
                llvm::errs() << "link:{" << linkInfo.getVisibility() << "," << (int) linkInfo.getLinkage() << "}";
                llvm::errs() << "static:" << functionDecl->isStatic() << " ";
                llvm::errs() << "operator:" << functionDecl->getOverloadedOperator() << " ";
                llvm::errs() << "inline:{" << functionDecl->isInlineSpecified() << "," << functionDecl->isInlined()
                             << "} ";
                llvm::errs() << "friend:" << functionDecl->getFriendObjectKind() << " ";

                if (const auto *cxxMethodDecl = dyn_cast<CXXMethodDecl>(decl)) {
                    llvm::errs() << "virtual:" << cxxMethodDecl->isVirtual() << " ";
                }

                llvm::errs() << "@@inst type: ";
                auto *tempArgList = functionDecl->getTemplateSpecializationArgs();
                if (tempArgList) {
                    for (int i = 0; i < tempArgList->size(); i++) {
                        llvm::errs() << i << ":";
                        auto &arg = tempArgList->get(i);
                        llvm::errs() << arg.getKind() << " ";
                        if (arg.getKind() == clang::TemplateArgument::Type) {
                            clang::QualType argType = arg.getAsType();
                            llvm::errs() << argType.getAsString() << " ";
                            // argType.dump();
                        }
                        llvm::errs() << ", ";
                    }
                }
                llvm::errs() << "@@ ";
            } else if (auto *varDecl = dyn_cast<VarDecl>(decl)) {
                llvm::errs() << "first:" << (void *) varDecl->getFirstDecl() << " ";
                llvm::errs() << "def: " << (void *) varDecl->getDefinition() << " ";

                const QualType type = varDecl->getType();
                llvm::errs() << "type:{typeClassName:" << type->getTypeClassName() <<
                             ",typeAsString:" << type.getAsString() <<
                             ",dumpType:" << dumpType(type) << "} ";

                llvm::errs() << "DesVarTemplate:" << (void *) varDecl->getDescribedVarTemplate() << " ";

                if (const auto *varSpecDecl =
                        llvm::dyn_cast<clang::VarTemplateSpecializationDecl>(decl)) {
                    llvm::errs() << "(spec)template:" << (void *) varSpecDecl->getSpecializedTemplate() << " ";
                }
            } else if (auto *cxxRecordDecl = dyn_cast<CXXRecordDecl>(decl)) {

                llvm::errs() << "first:" << (void *) cxxRecordDecl->getFirstDecl() << " ";
                llvm::errs() << "def:" << (void *) cxxRecordDecl->getDefinition() << " ";

                llvm::errs() << "extends:{ ";
                std::ostringstream extendsOss;
                dumpExtendsTree(cxxRecordDecl, extendsOss);
                llvm::errs() << extendsOss.str();
                llvm::errs() << "} ";

                llvm::errs() << "DesClasstemplate:" << (void *) cxxRecordDecl->getDescribedClassTemplate() << " ";

                if (const auto *classSpecDecl = dyn_cast<ClassTemplateSpecializationDecl>(decl)) {
                    llvm::errs() << "(sepc)template:" << (void *) classSpecDecl->getSpecializedTemplate() << " ";

                    llvm::errs() << "@@inst type: ";
                    auto &tempArgList = classSpecDecl->getTemplateArgs();
                    for (int i = 0; i < tempArgList.size(); i++) {
                        llvm::errs() << i << ":";
                        auto &arg = tempArgList.get(i);
                        llvm::errs() << arg.getKind() << " ";
                        if (arg.getKind() == clang::TemplateArgument::Type) {
                            clang::QualType argType = arg.getAsType();
                            llvm::errs() << argType.getAsString() << " ";
                            // argType.dump();
                        }
                        llvm::errs() << ", ";
                    }
                    llvm::errs() << "@@ ";
                }
            } else if (const auto *linkageSpecDecl = dyn_cast<LinkageSpecDecl>(decl)) {
                llvm::errs() << "hasBraces:" << linkageSpecDecl->hasBraces() << " ";
            } else if (const auto *tempDecl = dyn_cast<TemplateDecl>(decl)) {
                llvm::errs() << "templatedDecl: " << (void*)tempDecl->getTemplatedDecl();
            } else if (const auto *usingDecl = dyn_cast<UsingShadowDecl>(decl)) {
                llvm::errs() << "UnderlygingDecl: " << (void*)usingDecl->getTargetDecl();
            } else if (const auto *usUsingDecl =
                    llvm::dyn_cast<UnresolvedUsingValueDecl>(decl)) {
                llvm::errs() << "usUsing: " << usUsingDecl->getNameAsString() << " ";
            }

            llvm::errs() << "\n";

            const bool res = RecursiveASTVisitor::TraverseDecl(decl);

            depth -= 1;

            return res;
        }

        bool TraverseStmt(Stmt *stmt, DataRecursionQueue *queue = nullptr) {
            if (!stmt) {
                return true;
            }

            depth += 1;
            const std::string space = calSpaces(depth + 1);

            llvm::errs() << space << stmt->getStmtClassName() << " ";

            if (auto *declRefExpr = dyn_cast<DeclRefExpr>(stmt)) {
                llvm::errs() << "dest:" << dumpDecl(declRefExpr->getDecl()) << " ";
            } else if (const auto *memberExpr = dyn_cast<MemberExpr>(stmt)) {
                llvm::errs() << "dest:" << dumpDecl(memberExpr->getMemberDecl()) << " ";
            } else if (const auto *unresolvedLookupExpr = dyn_cast<UnresolvedLookupExpr>(stmt)) {
                llvm::errs() << "dest name(unresolved):{" << unresolvedLookupExpr->getName().getAsString() << ",";
                for (auto *decl: unresolvedLookupExpr->decls()) {
                    llvm::errs() << ((void *) (decl)) << ",";
                }
                llvm::errs() << "} ";
            } else if (const auto *unresolvedMemberExpr = dyn_cast<UnresolvedMemberExpr>(stmt)) {
                llvm::errs() << "dest name(unresolved member):{" << unresolvedMemberExpr->getName().getAsString() << ",";
                for (auto *decl: unresolvedMemberExpr->decls()) {
                    llvm::errs() << ((void *) (decl)) << ",";
                }
                llvm::errs() << "} ";
            } else if (const auto *tempMemberExpr = dyn_cast<CXXDependentScopeMemberExpr>(stmt)) {
                llvm::errs() << "dest member(unresolved):" << tempMemberExpr->getMemberNameInfo().getName() << " ";
            } else if (const auto *dsdExpr = dyn_cast<DependentScopeDeclRefExpr>(stmt)) {
                llvm::errs() << dsdExpr->getNameInfo().getName() << " ";
            }

            llvm::errs() << "\n";

            const bool res = RecursiveASTVisitor::TraverseStmt(stmt, queue);

            depth -= 1;

            return res;
        }

        bool VisitType(Type *type) {
            if (!type) {
                return true;
            }

            const std::string space = calSpaces(depth + 1);

            llvm::errs() << space << "[" << type->getTypeClassName() << "] ";

            llvm::errs() << "tag:" << dumpDecl(type->getAsTagDecl()) << " ";

            if (const auto *tempType = dyn_cast<TemplateSpecializationType>(type)) {
                auto *targetTemplate = tempType->getTemplateName().getAsTemplateDecl();
                llvm::errs() << "template:" << dumpDecl(targetTemplate) << " ";
            } else if (const auto *typedefType = dyn_cast<TypedefType>(type)) {
                llvm::errs() << "typedef:" << dumpDecl(typedefType->getDecl()) << " ";
            } else if (const auto *recordType = dyn_cast<RecordType>(type)) {
                llvm::errs() << "record:" << dumpDecl(recordType->getDecl()) << " ";
            } else if (const auto *enumType = dyn_cast<EnumType>(type)) {
                llvm::errs() << "enum:" << dumpDecl(enumType->getDecl()) << " ";
            } else if (const auto *dependentNameType = dyn_cast<DependentNameType>(type)) {
                llvm::errs() << "dependentName:" << dependentNameType->getIdentifier()->getName().str() << " ";
            }

            llvm::errs() << "\n";

            return true;
        }

    private:
        std::string getMangledName(const NamedDecl *decl) {
            if (decl && decl->getDeclName()) {
                if (isa<RequiresExprBodyDecl>(decl->getDeclContext())) {
                    return "";
                }
                if (auto *varDecl = dyn_cast<VarDecl>(decl); varDecl && varDecl->hasLocalStorage()) {
                    return "";
                }
                return astNameGenerator.getName(decl);
            }
            return "";
        }

        [[nodiscard]] std::string dumpOriginalCode(const SourceRange &sourceRange) const {
            const SourceLocation startLoc = sourceRange.getBegin();
            const SourceLocation endLoc = Lexer::getLocForEndOfToken(sourceRange.getEnd(), 0, em.sourceManager,
                                                               context.getLangOpts());
            const char *start = em.sourceManager.getCharacterData(startLoc);
            const char *end = em.sourceManager.getCharacterData(endLoc);
            if (start == nullptr || end == nullptr || end - start <= 0) {
                return "";
            }
            return {start, end};
        }

        [[nodiscard]] std::string dumpType(const QualType type) const {
            std::string res;

            SplitQualType splitType = type.split();
            if (!type.isNull()) {
                splitType = type.getSplitDesugaredType();
            }

            res.append(QualType::getAsString(splitType, context.getLangOpts()));

            return res;
        }

        std::string dumpDecl(Decl *decl) {
            if (decl == nullptr) {
                return "{}";
            }

            std::ostringstream oss;
            // if (auto *ctx = decl->getDeclContext(); ctx != nullptr) {
            //     oss << "[" << ctx << "," << ctx->getDeclKindName() << " ";
            //     oss << "Parent: " << ctx->getParent();
            //     oss << "]";
            // }
            oss << "[" << decl->getID() << "] ";
            if (decl->isImplicit()) {
                oss << "(impl)";
            }
            if (decl->isOutOfLine()) {
                oss << "(out-of-line)";
            }
            if (decl->isUsed()) {
                oss << "(used)";
            }
            if (decl->isReferenced())
            {
                oss << "(ref)";
            }
            oss << "{" << decl->getDeclKindName() << "Decl " << (void *) decl << " " <<
                em.dumpExpansionRange(decl->getSourceRange())->toString();

            if (const auto *namedDecl = dyn_cast<NamedDecl>(decl)) {
                oss << " <" << namedDecl->getNameAsString() << "> <" << getMangledName(namedDecl) << "> ";
            }

            oss << "DesTemplate:" << (void *) decl->getDescribedTemplate() << " ";

            oss << "inFunc:" << isLocalDecl(decl) << " ";

            if (const auto *valueDecl = llvm::dyn_cast<ValueDecl>(decl)) {
                oss << "Weak:" << valueDecl->isWeak() << " ";
            }

            oss << "Templated: " << decl->isTemplated() << " ";

            oss << "}";

            return oss.str();
        }

        static bool isLocalDecl(Decl *decl) {
            auto *declContext = decl->getDeclContext();
            while (declContext != nullptr) {
                if (declContext->isFunctionOrMethod()) {
                    return true;
                }
                declContext = declContext->getParent();
            }
            return false;
        }

        static std::string calSpaces(const int num) {
            std::string space;
            for (int i = 0; i < num; i += 1) {
                space.push_back(' ');
                space.push_back(' ');
                space.push_back('|');
            }
            return space;
        }

        static std::string expandPrefix(Decl *decl) {
            std::ostringstream oss;

            // cal namespaces and class prefix
            SmallVector<const DeclContext *, 8> ctxs;
            for (const DeclContext *ctx = decl->getDeclContext(); ctx;
                 ctx = ctx->getParent()) {
                // Skip non-named contexts such as linkage specifications and ExportDecls.
                if (!dyn_cast<NamedDecl>(ctx)) {
                    continue;
                }
                ctxs.push_back(ctx);
            }
            for (const DeclContext *ctx: llvm::reverse(ctxs)) {
                llvm::errs() << "[" << ctx->getDeclKindName() << "]";
                if (const auto *nd = dyn_cast<NamespaceDecl>(ctx)) {
                    oss << nd->getNameAsString() << "::";
                } else if (const auto *rd = dyn_cast<RecordDecl>(ctx)) {
                    if (rd->getDescribedTemplate() != nullptr) {
                        oss << "(T)";
                    }
                    if (llvm::dyn_cast<ClassTemplateSpecializationDecl>(rd) != nullptr) {
                        oss << "(S)";
                    }
                    oss << rd->getNameAsString() + "::";
                } else if (const auto *spec = dyn_cast<ClassTemplateSpecializationDecl>(ctx)) {
                    oss << "(Spec)::";
                }
            }
            return oss.str();
        }

        void dumpExtendsTree(CXXRecordDecl *cxxRecordDecl, std::ostringstream &oss) {
            if (auto *classSpecDecl = llvm::dyn_cast<ClassTemplateSpecializationDecl>(cxxRecordDecl)) {
                oss << classSpecDecl->getNameAsString() << "(Spec) ";
            } else {
                oss << cxxRecordDecl->getNameAsString() << " ";
            }
            oss << "(";
            auto extends = parseExtends(cxxRecordDecl);
            for (auto *fatherDecl : extends) {
                dumpExtendsTree(fatherDecl, oss);
            }
            oss << ")";
        }

        static std::vector<CXXRecordDecl*> parseExtends(CXXRecordDecl *cxxRecordDecl) {
            std::vector<CXXRecordDecl*> res;
            cxxRecordDecl = cxxRecordDecl->getDefinition();
            if (cxxRecordDecl == nullptr) {
                return res;
            }
            for (auto &base: cxxRecordDecl->bases()) {
                auto fClass = base.getType()->getAsCXXRecordDecl();
                if (fClass != nullptr) {
                    res.push_back(fClass);
                }
            }
            return res;
        }

    };

    class MyConsumer final : public ASTConsumer {
        CompilerInstance &Instance;

    public:
        explicit MyConsumer(CompilerInstance &Instance) : Instance(Instance) {}

        void HandleTranslationUnit(ASTContext &context) override {
            const SourceManager &sm = Instance.getSourceManager();
            if (const FileEntry *FE = sm.getFileEntryForID(sm.getMainFileID()); FE) {
                llvm::errs() << "File: " << FE->getName().str() << " has size: " << FE->getSize() << " bytes\n";
            } else {
                llvm::errs() << "Failed to get FileEntry\n";
            }

            const unsigned sz = sm.local_sloc_entry_size();
            for (int i = 0; i < sz; i++) {
                if (auto t = sm.getLocalSLocEntry(i); t.isFile()) {
                    auto tt = t.getFile();
                    llvm::errs() << "file" << i << ":" << tt.getName() << "\n";
                    FullSourceLoc fullsl(tt.getIncludeLoc(), sm);
                    llvm::errs() << fullsl.getLineNumber() << " " << fullsl.getColumnNumber() << "\n";
                }
            }

            MyVisitor v(Instance);
            v.TraverseDecl(context.getTranslationUnitDecl());
        }
    };

    class MyPluginAction final : public PluginASTAction {
    protected:
        std::unique_ptr<ASTConsumer> CreateASTConsumer(CompilerInstance &CI,
                                                       llvm::StringRef) override {
            CI.getPreprocessor().addPPCallbacks(std::make_unique<MyPPCallbacks>(CI));
            return std::make_unique<MyConsumer>(CI);
        }

        bool ParseArgs(const CompilerInstance &CI,
                       const std::vector<std::string> &args) override {
            for (unsigned i = 0, e = args.size(); i != e; ++i) {
                llvm::errs() << "arg " << i << " :" << args[i] << "\n";
            }
            return true;
        }

        // Automatically run the plugin after the main AST action
        PluginASTAction::ActionType getActionType() override {
            return AddAfterMainAction;
        }

    };
}

static FrontendPluginRegistry::Add <MyPluginAction>
        X("myplugin", "my plugin");