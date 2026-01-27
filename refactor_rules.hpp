#include <clang/Tooling/Transformer/RewriteRule.h>

using OurRewriteRule = clang::transformer::RewriteRuleWith<std::string>;

OurRewriteRule createRefactorKernelsDenseToViewRule();
OurRewriteRule createRefactorCoreDenseToViewRule();
OurRewriteRule createRefactorKernelDeclsDenseToViewRule();
