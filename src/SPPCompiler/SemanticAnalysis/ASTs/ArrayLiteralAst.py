from SPPCompiler.SemanticAnalysis.ASTs.ArrayLiteral0ElementAst import ArrayLiteral0ElementAst
from SPPCompiler.SemanticAnalysis.ASTs.ArrayLiteralNElementAst import ArrayLiteralNElementAst

type ArrayLiteralAst = ArrayLiteral0ElementAst | ArrayLiteralNElementAst

__all__ = ["ArrayLiteralAst"]
