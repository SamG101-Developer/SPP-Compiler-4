from SPPCompiler.SemanticAnalysis.ASTs.IdentifierAst import IdentifierAst
from SPPCompiler.SemanticAnalysis.ASTs.TokenAst import TokenAst

type PostfixMemberPartAst = IdentifierAst | TokenAst

__all__ = ["PostfixMemberPartAst"]
