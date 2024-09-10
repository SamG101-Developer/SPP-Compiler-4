from SPPCompiler.SemanticAnalysis.ASTs import (
    IdentifierAst, CaseExpressionAst, InnerScopeAst, LambdaPrototypeAst, LiteralAst, ObjectInitializerAst,
    ParenthesizedExpressionAst, TokenAst, TypeAst, LoopExpressionAst, WithExpressionAst, GenExpressionAst)

type PrimaryExpressionAst = (
    LiteralAst | IdentifierAst | ParenthesizedExpressionAst | ObjectInitializerAst | LambdaPrototypeAst |
    CaseExpressionAst | LoopExpressionAst | WithExpressionAst | GenExpressionAst | InnerScopeAst | TypeAst |
    TokenAst)

__all__ = ["PrimaryExpressionAst"]
