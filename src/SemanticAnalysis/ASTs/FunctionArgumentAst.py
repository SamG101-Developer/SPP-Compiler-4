from __future__ import annotations
from src.SemanticAnalysis.ASTs.FunctionArgumentNamedAst import FunctionArgumentNamedAst
from src.SemanticAnalysis.ASTs.FunctionArgumentNormalAst import FunctionArgumentNormalAst

type FunctionArgumentAst = FunctionArgumentNamedAst | FunctionArgumentNormalAst

__all__ = ['FunctionArgumentAst']
