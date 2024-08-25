from SPPCompiler.SemanticAnalysis.ASTs import (UseStatementImportMultipleTypesAst, UseStatementImportSingleTypeAst)

type UseStatementImportTypesAst = (
        UseStatementImportMultipleTypesAst |
        UseStatementImportSingleTypeAst)

__all__ = ["UseStatementImportTypesAst"]
