from SPPCompiler.SemanticAnalysis.ASTs import (
    BinaryExpressionAst, PostfixExpressionAst, TokenAst, UnaryExpressionAst, PrimaryExpressionAst)

type ExpressionAst = BinaryExpressionAst | PostfixExpressionAst | TokenAst | UnaryExpressionAst | PrimaryExpressionAst

__all__ = ["ExpressionAst"]
