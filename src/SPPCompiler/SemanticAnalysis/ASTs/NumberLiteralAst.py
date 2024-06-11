from SPPCompiler.SemanticAnalysis.ASTs.NumberLiteralBase02Ast import NumberLiteralBase02Ast
from SPPCompiler.SemanticAnalysis.ASTs.NumberLiteralBase10Ast import NumberLiteralBase10Ast
from SPPCompiler.SemanticAnalysis.ASTs.NumberLiteralBase16Ast import NumberLiteralBase16Ast

type NumberLiteralAst = NumberLiteralBase02Ast | NumberLiteralBase10Ast | NumberLiteralBase16Ast

__all__ = ["NumberLiteralAst"]
