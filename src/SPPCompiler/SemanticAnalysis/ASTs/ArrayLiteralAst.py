from SPPCompiler.SemanticAnalysis.ASTs.ArrayLiteralEmptyAst import ArrayLiteralEmptyAst
from SPPCompiler.SemanticAnalysis.ASTs.ArrayLiteralNonEmptyAst import ArrayLiteralNonEmptyAst

type ArrayLiteralAst = ArrayLiteralNonEmptyAst | ArrayLiteralEmptyAst

__all__ = ["ArrayLiteralAst"]
