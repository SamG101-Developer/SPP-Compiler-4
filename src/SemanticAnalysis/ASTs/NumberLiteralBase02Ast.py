from dataclasses import dataclass

from src.SemanticAnalysis.ASTs.NumberLiteralBaseNAst import NumberLiteralBaseNAst


@dataclass
class NumberLiteralBase02Ast(NumberLiteralBaseNAst):
    """
    The LiteralNumberBase02Ast node is used to represent a number literal in base 2 (binary). This is a literal number
    prefixed with "0b".
    """


__all__ = ["NumberLiteralBase02Ast"]
