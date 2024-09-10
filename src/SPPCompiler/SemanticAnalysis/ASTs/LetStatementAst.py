from SPPCompiler.SemanticAnalysis.ASTs import LetStatementInitializedAst, LetStatementUninitializedAst

type LetStatementAst = LetStatementInitializedAst | LetStatementUninitializedAst

__all__ = ["LetStatementAst"]
