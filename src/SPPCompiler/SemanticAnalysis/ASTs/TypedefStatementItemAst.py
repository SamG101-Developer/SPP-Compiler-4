from SPPCompiler.SemanticAnalysis.ASTs.TypedefStatementSpecificItemAst import TypedefStatementSpecificItemAst
from SPPCompiler.SemanticAnalysis.ASTs.TypedefStatementSpecificItemsAst import TypedefStatementSpecificItemsAst
from SPPCompiler.SemanticAnalysis.ASTs.TypedefStatementAllItemsAst import TypedefStatementAllItemsAst

type TypedefStatementItemAst = (
        TypedefStatementSpecificItemAst |
        TypedefStatementSpecificItemsAst |
        TypedefStatementAllItemsAst)

__all__ = ["TypedefStatementItemAst"]
