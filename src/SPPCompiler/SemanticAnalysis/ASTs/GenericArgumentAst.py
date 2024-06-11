from __future__ import annotations
from SPPCompiler.SemanticAnalysis.ASTs.GenericArgumentNamedAst import GenericArgumentNamedAst
from SPPCompiler.SemanticAnalysis.ASTs.GenericArgumentNormalAst import GenericArgumentNormalAst

type GenericArgumentAst = GenericArgumentNormalAst | GenericArgumentNamedAst

__all__ = ["GenericArgumentAst"]
