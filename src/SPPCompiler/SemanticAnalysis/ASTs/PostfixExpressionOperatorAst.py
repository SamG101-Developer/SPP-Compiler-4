from SPPCompiler.SemanticAnalysis.ASTs import (
    PostfixExpressionOperatorFunctionCallAst, PostfixExpressionOperatorMemberAccessAst,
    PostfixExpressionOperatorEarlyReturnAst, PostfixExpressionOperatorNotKeywordAst)

type PostfixExpressionOperatorAst = (
    PostfixExpressionOperatorFunctionCallAst | PostfixExpressionOperatorMemberAccessAst |
    PostfixExpressionOperatorEarlyReturnAst | PostfixExpressionOperatorNotKeywordAst)

__all__ = ["PostfixExpressionOperatorAst"]
