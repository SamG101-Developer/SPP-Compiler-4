from SPPCompiler.SemanticAnalysis.ASTs import NumberLiteralBase02Ast, NumberLiteralBase10Ast, NumberLiteralBase16Ast

type NumberLiteralAst = NumberLiteralBase02Ast | NumberLiteralBase10Ast | NumberLiteralBase16Ast

__all__ = ["NumberLiteralAst"]
