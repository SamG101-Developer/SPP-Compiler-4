from dataclasses import dataclass

from src.SemanticAnalysis.ASTs.NumberLiteralBaseNAst import NumberLiteralBaseNAst


@dataclass
class NumberLiteralBase16Ast(NumberLiteralBaseNAst):
    """
    The LiteralNumberBase16Ast node is used to represent a number literal in base 16 (hexadecimal). This is a literal
    number prefixed with "0x".
    """


__all__ = ["NumberLiteralBase16Ast"]
