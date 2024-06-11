from SPPCompiler.SemanticAnalysis.ASTs.IdentifierAst import IdentifierAst
from SPPCompiler.SemanticAnalysis.ASTs.GenericIdentifierAst import GenericIdentifierAst
from SPPCompiler.SemanticAnalysis.ASTs.TokenAst import TokenAst

type TypePartAst = IdentifierAst | GenericIdentifierAst | TokenAst

__all__ = ["TypePartAst"]
