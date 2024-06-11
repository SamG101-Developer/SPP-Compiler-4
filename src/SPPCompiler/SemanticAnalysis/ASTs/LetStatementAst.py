from SPPCompiler.SemanticAnalysis.ASTs.LetStatementInitializedAst import LetStatementInitializedAst
from SPPCompiler.SemanticAnalysis.ASTs.LetStatementUninitializedAst import LetStatementUninitializedAst

type LetStatementAst = LetStatementInitializedAst | LetStatementUninitializedAst

__all__ = ["LetStatementAst"]
