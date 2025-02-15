#include <memory>
#include <string>
#include <vector>
#include <sstream>
#include <fstream>
#include <unordered_map>
#include <unordered_set>

#include "clang/Frontend/FrontendPluginRegistry.h"
#include "clang/AST/AST.h"
#include "clang/AST/ASTConsumer.h"
#include "clang/AST/RecursiveASTVisitor.h"
#include "clang/AST/Mangle.h"
#include "clang/Frontend/CompilerInstance.h"
#include "llvm/Support/raw_ostream.h"

using namespace clang;

namespace {

    class ASTUtils {
    public:
        static ASTUtils& getInstance() {
            static ASTUtils single;
            return single;
        }

        void init(const CompilerInstance &instance) {
            context = &instance.getASTContext();
            sm = &instance.getSourceManager();
            astNameGenerator = std::make_unique<ASTNameGenerator>(instance.getASTContext());
        }

        std::string getSourceRange(const Decl *decl) const {
            const auto sr = decl->getSourceRange();
            const FullSourceLoc begin(sr.getBegin(), *sm);
            const FullSourceLoc end(sr.getEnd(), *sm);
            if (begin.isInvalid() || end.isInvalid()) {
                return "<>";
            }
            const unsigned beginLine = begin.getExpansionLineNumber();
            const unsigned beginColumn = begin.getExpansionColumnNumber();
            const unsigned endLine = end.getExpansionLineNumber();
            const unsigned endColumn = end.getExpansionColumnNumber();
            std::ostringstream oss;
            oss << "<" << beginLine << ":" << beginColumn << "," << endLine << ":" << endColumn << ">";
            return oss.str();
        }

        std::string getMangledName(const NamedDecl *decl) {
            if (decl && decl->getDeclName()) {
                if (isa<RequiresExprBodyDecl>(decl->getDeclContext())) {
                    return "";
                }
                if (auto *varDecl = dyn_cast<VarDecl>(decl); varDecl && varDecl->hasLocalStorage()) {
                    return "";
                }
                return astNameGenerator->getName(decl);
            }
            return "";
        }

        std::string dumpDecl(const Decl *decl) {
            if (decl == nullptr) {
                return "{}";
            }

            std::ostringstream oss;
            oss << "{" << decl->getDeclKindName() << "Decl " << (void *) decl << " ";

            if (const auto *namedDecl = dyn_cast<NamedDecl>(decl)) {
                oss << " <" << namedDecl->getNameAsString() << "> <" << getMangledName(namedDecl) << "> ";
            }

            oss << getSourceRange(decl);

            oss << "}";

            return oss.str();
        }

        // ==================================================

        // Get enclosing classes for the given FunctionDecl.
        // For example:
        // class A { public:
        //   class B { public:
        //     void test() {}
        // };};
        // // test -> B, A
        static std::vector<const CXXRecordDecl *>
        getEnclosingClasses(const Decl *decl) {
            std::vector<const CXXRecordDecl *> res;
            const DeclContext *dc = decl->getDeclContext();
            while (dc && !isa<TranslationUnitDecl>(dc)) {
                if (const CXXRecordDecl *classDecl = dyn_cast<CXXRecordDecl>(dc)) {
                    res.push_back(classDecl);
                }
                dc = dc->getParent();
            }
            return res;
        }

        static const FunctionDecl *getFuncSpec(const FunctionDecl *funcDecl) {
            if (funcDecl->getPrimaryTemplate() != nullptr) {
                return funcDecl;
            }
            return nullptr;
        }

        static const ClassTemplateSpecializationDecl *
        getClassSpec(const CXXRecordDecl *cxxRecordDecl) {
            if (const auto *classSpecDecl =
                    dyn_cast<ClassTemplateSpecializationDecl>(cxxRecordDecl)) {
                if (classSpecDecl->getSpecializedTemplate() != nullptr) {
                    return classSpecDecl;
                }
            }
            return nullptr;
        }

        // Get inst type (Decl) for FunctionDecl (whose getPrimaryTemplate() !=
        // nullptr). succ == false means that the result is invalid.
        static std::unordered_set<const Decl *>
        getFuncInstTypes(const FunctionDecl *funcDecl, bool &success) {
            success = true;
            std::unordered_set<const Decl *> res;
            const auto *specArgList = funcDecl->getTemplateSpecializationArgs();
            if (specArgList == nullptr) {
                return res;
            }
            for (int i = 0; i < specArgList->size(); i++) {
                auto &arg = specArgList->get(i);
                if (arg.getKind() == TemplateArgument::Type) {
                    auto argType = arg.getAsType();
                    if (!typeExtraction(argType, res)) {
                        success = false;
                        return res;
                    }
                }
            }
            return res;
        }

        // Get inst type (Decl) for ClassTemplateSpecializationDecl (whose
        // getSpecializedTemplate() != nullptr). succ == false means that the result
        // is invalid.
        static std::unordered_set<const Decl *>
        getClassInstTypes(const ClassTemplateSpecializationDecl *classSpecDecl,
                          bool &success) {
            success = true;
            std::unordered_set<const Decl *> res;
            auto &specArgs = classSpecDecl->getTemplateArgs();
            for (int i = 0; i < specArgs.size(); i++) {
                auto &arg = specArgs.get(i);
                if (arg.getKind() == TemplateArgument::Type) {
                    auto argType = arg.getAsType();
                    if (!typeExtraction(argType, res)) {
                        success = false;
                        return res;
                    }
                }
            }
            return res;
        }

        // Traverse types and extract all included Decls.
        // This function won't parse class specs recursively, you need to parse them yourself.
        // This function only support combinations of Builtin, Pointer, Reference, Decl,
        // and it will return false when encountering any unsupported type.
        static bool typeExtraction(const QualType &type, std::unordered_set<const Decl *> &decls) {
            if (type->isBuiltinType()) {
                return true;
            }
            if (type->isPointerType() || type->isReferenceType()) {
                auto pteType = type->getPointeeType();
                if (!typeExtraction(pteType, decls)) {
                    return false;
                }
                return true;
            }
            if (type->isRecordType()) {
                const RecordType *recordType = type->getAs<RecordType>();
                const RecordDecl *recordDecl = recordType->getDecl();
                if (recordDecl == nullptr) {
                    return false;
                }
                decls.insert(recordDecl);
                return true;
            }
            // Do not support other types, such as array type, function type, ...
            return false;
        }

    private:
        ASTContext *context = nullptr;
        SourceManager *sm = nullptr;
        std::unique_ptr<ASTNameGenerator> astNameGenerator;

        ASTUtils() {};
        ~ASTUtils() {};
        ASTUtils(const ASTUtils &astUtils) = delete;
        const ASTUtils &operator=(const ASTUtils &astUtils) = delete;
    };

    class MyVisitor : public RecursiveASTVisitor<MyVisitor> {
    public:
        int declId = 0;
        // Safe: the lastId strictly < safeId
        int safeId = 0;
        // decl -> {id, lastId}
        std::unordered_map<const Decl*, std::pair<int, int>> declToId;

        MyVisitor() {}

        // bool shouldVisitTemplateInstantiations() const { return true; }

        bool TraverseDecl(Decl *decl) {
            if (!decl) {
                return true;
            }

            update(decl);

            // Ignore local Decl.
            bool res = true;
            if (dyn_cast<FunctionDecl>(decl) == nullptr) {
                res = RecursiveASTVisitor::TraverseDecl(decl);
            }

            return res;
        }

    private:
        static bool isValidDecl(const Decl *decl) {
            if (decl->isImplicit()) {
                return false;
            }
            if (dyn_cast<ClassTemplateDecl>(decl) == nullptr &&
                dyn_cast<FunctionTemplateDecl>(decl) == nullptr &&
                dyn_cast<FunctionDecl>(decl) == nullptr &&
                dyn_cast<CXXRecordDecl>(decl) == nullptr) {
                return false;
            }
            return true;
        }

        static bool isSafeIdFunc(const Decl* decl) {
            const auto *funcDecl = dyn_cast<FunctionDecl>(decl);
            return funcDecl != nullptr && funcDecl->getNameAsString() == "safe__id";
        }

        void update(const Decl *decl) {
            if (!isValidDecl(decl)) {
                return;
            }

            auto &astUtils = ASTUtils::getInstance();

            declId += 1;

            if (isSafeIdFunc(decl)) {

                llvm::errs() << "********** safe id **********\n";

                safeId = declId;
            }

            llvm::errs() << astUtils.dumpDecl(decl) << " " << declId << "\n";

            declToId[decl] = {declId, declId};

            if (decl->isOutOfLine()) {
                for (auto *ctx = decl->getDeclContext(); ctx != nullptr; ctx = ctx->getParent()) {
                    auto *namedDecl = llvm::dyn_cast<NamedDecl>(ctx);
                    if (namedDecl == nullptr || !isValidDecl(namedDecl)) {
                        continue;
                    }

                    llvm::errs() << "--| " << astUtils.dumpDecl(namedDecl) << "\n";

                    auto it = declToId.find(namedDecl);
                    if (it != declToId.end()) {
                        it->second.second = declId;
                    }
                }
            }

            for (const auto *prevDecl = decl->getPreviousDecl(); prevDecl != nullptr; prevDecl = prevDecl->getPreviousDecl()) {
                if (!isValidDecl(prevDecl)) {
                    continue;
                }

                llvm::errs() << "==| " << astUtils.dumpDecl(prevDecl) << "\n";

                auto it = declToId.find(prevDecl);
                if (it != declToId.end()) {
                    it->second.second = declId;
                }
            }
        }
    };

    class MyVisitor2 : public RecursiveASTVisitor<MyVisitor2> {
    public:
        bool inClassSpec = false;

        std::vector<const FunctionDecl *> instFuncs;

        MyVisitor2() {}

        bool shouldVisitTemplateInstantiations() const { return true; }

        bool TraverseDecl(Decl *decl) {
            if (!decl) {
                return true;
            }

            bool oldInClassSpec = inClassSpec;
            if (dyn_cast<ClassTemplateSpecializationDecl>(decl) != nullptr) {
                inClassSpec = true;
            }

            update(decl);

            // Ignore local Decl.
            bool res = true;
            if (dyn_cast<FunctionDecl>(decl) == nullptr) {
                res = RecursiveASTVisitor::TraverseDecl(decl);
            }

            inClassSpec = oldInClassSpec;

            return res;
        }
    private:
        void update(const Decl *decl) {
            auto &astUtils = ASTUtils::getInstance();

            const FunctionDecl *funcDecl = dyn_cast<FunctionDecl>(decl);
            if (decl->isImplicit() || funcDecl == nullptr || !funcDecl->isThisDeclarationADefinition()) {
                return;
            }

            const std::string mangledName = astUtils.getMangledName(funcDecl);
            if (mangledName.empty()) {
                return;
            }

            llvm::errs() << astUtils.dumpDecl(decl) << " ";
            if (inClassSpec || funcDecl->getPrimaryTemplate() != nullptr) {
                llvm::errs() << "is inst";
                instFuncs.push_back(funcDecl);
            } else {
                llvm::errs() << "is not inst";
            }
            llvm::errs() << "\n";
        }
    };

    class MyConsumer final : public ASTConsumer {
        CompilerInstance &Instance;

    public:
        explicit MyConsumer(CompilerInstance &Instance) : Instance(Instance) {}

        bool checkFunc(const FunctionDecl *funcDecl) {
            return true;
        }

        bool checkClass(const CXXRecordDecl *classDecl) {

        }

        bool checkType() {

        }

        void HandleTranslationUnit(ASTContext &context) override {
            auto &astUtils = ASTUtils::getInstance();
            astUtils.init(Instance);

            llvm::errs() << "========== Visit1 ==========\n";
            MyVisitor v;
            v.TraverseDecl(context.getTranslationUnitDecl());

            llvm::errs() << "========== Visit2 ==========\n";
            MyVisitor2 v2;
            v2.TraverseDecl(context.getTranslationUnitDecl());

            llvm::errs() << "========== Res ==========\n";

            std::ofstream ofs("res.txt");
            if (!ofs.is_open()) {
                llvm::errs() << "Can not open res.txt\n";
                exit(1);
            }

            llvm::errs() << "SafeId: " << v.safeId << "\n";
            ofs << v.safeId << "\n";

            llvm::errs() << "declToId:\n";
            std::vector<std::pair<const Decl*, std::pair<int, int>>> items(v.declToId.begin(), v.declToId.end());
            std::sort(items.begin(), items.end(), [](const auto &lhs, const auto &rhs) {
                return lhs.second.first < rhs.second.first;
            });

            for (const auto &item : items) {
                llvm::errs() << astUtils.dumpDecl(item.first) << " " << item.second.first << " " << item.second.second << "\n";

                ofs << item.second.first << " " << item.second.second << "\n";

            }

            llvm::errs() << "Check:\n";
            for (const auto &funcDecl : v2.instFuncs) {
                bool res = checkFunc(funcDecl);

                llvm::errs() << astUtils.dumpDecl(funcDecl) << ": " << res << "\n";
                ofs << astUtils.getMangledName(funcDecl) << ": " << res << "\n";
            }

            ofs.close();
        }
    };

    class MyPluginAction final : public PluginASTAction {
    protected:
        std::unique_ptr<ASTConsumer> CreateASTConsumer(CompilerInstance &CI,
                                                       llvm::StringRef) override {
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
        ActionType getActionType() override {
            return AddAfterMainAction;
        }

    };
}

static FrontendPluginRegistry::Add <MyPluginAction>
        X("funcxable", "funcxable test plugin");