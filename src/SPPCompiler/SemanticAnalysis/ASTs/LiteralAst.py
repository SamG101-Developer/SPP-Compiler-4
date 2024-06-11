from SPPCompiler.SemanticAnalysis.ASTs.BooleanLiteralAst import BooleanLiteralAst
from SPPCompiler.SemanticAnalysis.ASTs.NumberLiteralAst import NumberLiteralAst
from SPPCompiler.SemanticAnalysis.ASTs.RegexLiteralAst import RegexLiteralAst
from SPPCompiler.SemanticAnalysis.ASTs.StringLiteralAst import StringLiteralAst
from SPPCompiler.SemanticAnalysis.ASTs.TupleLiteralAst import TupleLiteralAst

type LiteralAst = BooleanLiteralAst | NumberLiteralAst | RegexLiteralAst | StringLiteralAst | TupleLiteralAst

__all__ = ["LiteralAst"]
