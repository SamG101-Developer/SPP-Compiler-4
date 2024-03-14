from src.SemanticAnalysis.ASTs.NumberLiteralBase02Ast import NumberLiteralBase02Ast
from src.SemanticAnalysis.ASTs.NumberLiteralBase10Ast import NumberLiteralBase10Ast
from src.SemanticAnalysis.ASTs.NumberLiteralBase16Ast import NumberLiteralBase16Ast

type NumberLiteralAst = NumberLiteralBase02Ast | NumberLiteralBase10Ast | NumberLiteralBase16Ast

__all__ = ["NumberLiteralAst"]
