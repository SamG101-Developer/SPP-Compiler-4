from SPPCompiler.SemanticAnalysis.ASTs.PostfixExpressionOperatorFunctionCallAst import PostfixExpressionOperatorFunctionCallAst
from SPPCompiler.SemanticAnalysis.ASTs.PostfixExpressionOperatorMemberAccessAst import PostfixExpressionOperatorMemberAccessAst
from SPPCompiler.SemanticAnalysis.ASTs.PostfixExpressionOperatorEarlyReturnAst import PostfixExpressionOperatorEarlyReturnAst

type PostfixExpressionOperatorAst = (
        PostfixExpressionOperatorFunctionCallAst |
        PostfixExpressionOperatorMemberAccessAst |
        PostfixExpressionOperatorEarlyReturnAst)

__all__ = ["PostfixExpressionOperatorAst"]
