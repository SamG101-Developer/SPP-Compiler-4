from src.SemanticAnalysis.ASTs.IdentifierAst import IdentifierAst
from src.SemanticAnalysis.ASTs.GenericIdentifierAst import GenericIdentifierAst
from src.SemanticAnalysis.ASTs.TokenAst import TokenAst

type TypePartAst = IdentifierAst | GenericIdentifierAst | TokenAst

__all__ = ["TypePartAst"]
