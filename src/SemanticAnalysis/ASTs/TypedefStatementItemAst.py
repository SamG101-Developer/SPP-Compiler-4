from src.SemanticAnalysis.ASTs.TypedefStatementSpecificItemAst import TypedefStatementSpecificItemAst
from src.SemanticAnalysis.ASTs.TypedefStatementSpecificItemsAst import TypedefStatementSpecificItemsAst
from src.SemanticAnalysis.ASTs.TypedefStatementAllItemsAst import TypedefStatementAllItemsAst

type TypedefStatementItemAst = (
        TypedefStatementSpecificItemAst |
        TypedefStatementSpecificItemsAst |
        TypedefStatementAllItemsAst)

__all__ = ["TypedefStatementItemAst"]
