from src.SemanticAnalysis.ASTs.ArrayLiteralAst import ArrayLiteralAst
from src.SemanticAnalysis.ASTs.BooleanLiteralAst import BooleanLiteralAst
from src.SemanticAnalysis.ASTs.NumberLiteralAst import NumberLiteralAst
from src.SemanticAnalysis.ASTs.RegexLiteralAst import RegexLiteralAst
from src.SemanticAnalysis.ASTs.StringLiteralAst import StringLiteralAst
from src.SemanticAnalysis.ASTs.TupleLiteralAst import TupleLiteralAst

type LiteralAst = (
        ArrayLiteralAst | BooleanLiteralAst | NumberLiteralAst |
        RegexLiteralAst | StringLiteralAst | TupleLiteralAst)

__all__ = ["LiteralAst"]
