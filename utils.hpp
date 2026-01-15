#pragma once

#include <string_view>

#include "clang/ASTMatchers/ASTMatchers.h"

enum class qualifier_mode { only_const, only_mutable, both };

namespace matchers {

using namespace ::clang::ast_matchers;

/**
 * matches any decl node that is declared inside the given namespace
 */
auto inline isInNamespace(std::string_view name) {
  return hasAncestor(namespaceDecl(hasName(name)));
}

/**
 * matches the type of a pointer (const or non-const or both depending on mode)
 * pointing to an instantiated class template with the given name.
 */
auto inline instantiatedClassTemplatePointerType(std::string_view name,
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

} // namespace matchers