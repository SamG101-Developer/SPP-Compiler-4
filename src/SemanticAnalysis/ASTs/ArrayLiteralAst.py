from src.SemanticAnalysis.ASTs.ArrayLiteralEmptyAst import ArrayLiteralEmptyAst
from src.SemanticAnalysis.ASTs.ArrayLiteralNonEmptyAst import ArrayLiteralNonEmptyAst

type ArrayLiteralAst = ArrayLiteralNonEmptyAst | ArrayLiteralEmptyAst

__all__ = ["ArrayLiteralAst"]
