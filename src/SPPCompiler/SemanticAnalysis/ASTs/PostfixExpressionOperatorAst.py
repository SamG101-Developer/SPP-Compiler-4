from SPPCompiler.SemanticAnalysis.ASTs import PostfixExpressionOperatorFunctionCallAst
from SPPCompiler.SemanticAnalysis.ASTs import PostfixExpressionOperatorMemberAccessAst
from SPPCompiler.SemanticAnalysis.ASTs import PostfixExpressionOperatorEarlyReturnAst
from SPPCompiler.SemanticAnalysis.ASTs import PostfixExpressionOperatorNotKeywordAst

type PostfixExpressionOperatorAst = (
        PostfixExpressionOperatorFunctionCallAst |
        PostfixExpressionOperatorMemberAccessAst |
        PostfixExpressionOperatorEarlyReturnAst |
        PostfixExpressionOperatorNotKeywordAst)

__all__ = ["PostfixExpressionOperatorAst"]
