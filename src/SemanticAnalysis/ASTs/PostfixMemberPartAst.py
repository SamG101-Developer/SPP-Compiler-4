from src.SemanticAnalysis.ASTs.IdentifierAst import IdentifierAst
from src.SemanticAnalysis.ASTs.TokenAst import TokenAst

type PostfixMemberPartAst = IdentifierAst | TokenAst

__all__ = ["PostfixMemberPartAst"]
