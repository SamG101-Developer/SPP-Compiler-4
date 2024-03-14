from src.SemanticAnalysis.ASTs.LetStatementInitializedAst import LetStatementInitializedAst
from src.SemanticAnalysis.ASTs.LetStatementUninitializedAst import LetStatementUninitializedAst

type LetStatementAst = LetStatementInitializedAst | LetStatementUninitializedAst

__all__ = ["LetStatementAst"]
