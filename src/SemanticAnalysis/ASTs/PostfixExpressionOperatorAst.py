from src.SemanticAnalysis.ASTs.PostfixExpressionOperatorFunctionCallAst import PostfixExpressionOperatorFunctionCallAst
from src.SemanticAnalysis.ASTs.PostfixExpressionOperatorMemberAccessAst import PostfixExpressionOperatorMemberAccessAst
from src.SemanticAnalysis.ASTs.PostfixExpressionOperatorEarlyReturnAst import PostfixExpressionOperatorEarlyReturnAst

type PostfixExpressionOperatorAst = (
        PostfixExpressionOperatorFunctionCallAst |
        PostfixExpressionOperatorMemberAccessAst |
        PostfixExpressionOperatorEarlyReturnAst)

__all__ = ["PostfixExpressionOperatorAst"]
