from src.SemanticAnalysis.ASTs.BinaryExpressionAst import BinaryExpressionAst
from src.SemanticAnalysis.ASTs.PostfixExpressionAst import PostfixExpressionAst
from src.SemanticAnalysis.ASTs.TokenAst import TokenAst
from src.SemanticAnalysis.ASTs.UnaryExpressionAst import UnaryExpressionAst
from src.SemanticAnalysis.ASTs.PrimaryExpressionAst import PrimaryExpressionAst

type ExpressionAst = BinaryExpressionAst | PostfixExpressionAst | TokenAst | UnaryExpressionAst | PrimaryExpressionAst

__all__ = ["ExpressionAst"]
