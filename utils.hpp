#pragma once

#include <string_view>

#include "clang/ASTMatchers/ASTMatchers.h"

enum class qualifier_mode { only_const, only_mutable, both };

namespace matchers {

using namespace ::clang::ast_matchers;

inline auto classTemplateSpecializationType(std::string_view name) {
  return templateSpecializationType(
      hasDeclaration(classTemplateDecl(hasName("::gko::matrix::Dense"))));
}

} // namespace matchers