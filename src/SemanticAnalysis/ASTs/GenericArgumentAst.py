from __future__ import annotations
from src.SemanticAnalysis.ASTs.GenericArgumentNamedAst import GenericArgumentNamedAst
from src.SemanticAnalysis.ASTs.GenericArgumentNormalAst import GenericArgumentNormalAst

type GenericArgumentAst = GenericArgumentNormalAst | GenericArgumentNamedAst

__all__ = ["GenericArgumentAst"]
