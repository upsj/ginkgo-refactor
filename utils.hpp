#pragma once

#include <string_view>

#include "clang/ASTMatchers/ASTMatchers.h"

enum class qualifier_mode { only_const, only_mutable, both };

namespace matchers {

using namespace ::clang::ast_matchers;

/**
 * matches any decl node that is declared inside the given namespace
 */
inline auto isInNamespace(std::string_view name) {
  return hasAncestor(namespaceDecl(hasName(name)));
}

/**
 * matches the type of a pointer (const or non-const or both depending on mode)
 * pointing to an instantiated class template with the given name.
 */
inline auto instantiatedClassTemplatePointerType(std::string_view name,
                                                 qualifier_mode mode) {
  auto dense_class_matcher = recordType(
      hasDeclaration(classTemplateSpecializationDecl(hasName(name))));
  auto pointer_matcher = [&] {
    switch (mode) {
    case qualifier_mode::only_const:
      return pointerType(pointee(isConstQualified(), dense_class_matcher));
    case qualifier_mode::only_mutable:
      return pointerType(
          pointee(unless(isConstQualified()), dense_class_matcher));
    case qualifier_mode::both:
    default:
      return pointerType(pointee(dense_class_matcher));
    }
  }();
  // we need to get the canonical type in case this is the return type of a
  // smart pointer's .get() function, which involves a SubstTemplateTypeParmType
  // wrapper
  return hasCanonicalType(pointer_matcher);
}

inline auto densePointerType(qualifier_mode mode) {
  return instantiatedClassTemplatePointerType("::gko::matrix::Dense", mode);
}

inline auto executorType() {
  return hasUnqualifiedDesugaredType(
      recordType(hasDeclaration(classTemplateSpecializationDecl(
          isInStdNamespace(), hasName("shared_ptr"),
          hasTemplateArgument(
              0, templateArgument(refersToType(hasDeclaration(
                     cxxRecordDecl(anyOf(isDerivedFrom("::gko::Executor"),
                                         hasName("::gko::Executor")))))))))));
}

inline auto executorExpr() { return expr(hasType(executorType())); }

inline auto namedMemberExpr(std::string_view name, auto object_expr,
                            auto... other_matchers) {
  return memberExpr(hasDeclaration(namedDecl(hasName(name))),
                    hasObjectExpression(object_expr), other_matchers...);
}

inline auto smartPtrMemberExpr(std::string_view name, auto object_expr) {
  return namedMemberExpr(name, cxxOperatorCallExpr(hasArgument(0, object_expr)),
                         isArrow());
}

inline auto smartPtrGetExpr() {
  return materializeTemporaryExpr(
      has(cxxMemberCallExpr(callee(namedMemberExpr("get", expr())))));
}

AST_MATCHER(clang::CallExpr, isMakeFunction) {
  auto CD = Node.getCalleeDecl();
  if (!CD) {
    return false;
  }
  auto F = CD->getAsFunction();
  if (!F) {
    return false;
  }
  return F->getName().starts_with("make_");
}

inline auto maybeBind(auto matcher, std::optional<std::string_view> name)
    -> decltype(matcher.bind(*name)) {
  if (name) {
    return matcher.bind(*name);
  } else {
    return matcher;
  }
}

} // namespace matchers