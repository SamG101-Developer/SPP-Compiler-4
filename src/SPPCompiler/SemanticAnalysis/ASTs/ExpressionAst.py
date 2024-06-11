from SPPCompiler.SemanticAnalysis.ASTs.BinaryExpressionAst import BinaryExpressionAst
from SPPCompiler.SemanticAnalysis.ASTs.PostfixExpressionAst import PostfixExpressionAst
from SPPCompiler.SemanticAnalysis.ASTs.TokenAst import TokenAst
from SPPCompiler.SemanticAnalysis.ASTs.UnaryExpressionAst import UnaryExpressionAst
from SPPCompiler.SemanticAnalysis.ASTs.PrimaryExpressionAst import PrimaryExpressionAst

type ExpressionAst = BinaryExpressionAst | PostfixExpressionAst | TokenAst | UnaryExpressionAst | PrimaryExpressionAst

__all__ = ["ExpressionAst"]
