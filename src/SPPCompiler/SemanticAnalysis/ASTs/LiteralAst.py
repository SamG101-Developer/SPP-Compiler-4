from SPPCompiler.SemanticAnalysis.ASTs import (
    BooleanLiteralAst, NumberLiteralAst, RegexLiteralAst, StringLiteralAst, TupleLiteralAst)

type LiteralAst = BooleanLiteralAst | NumberLiteralAst | RegexLiteralAst | StringLiteralAst | TupleLiteralAst

__all__ = ["LiteralAst"]
