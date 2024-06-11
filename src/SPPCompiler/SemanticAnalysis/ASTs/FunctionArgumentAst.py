from __future__ import annotations
from SPPCompiler.SemanticAnalysis.ASTs.FunctionArgumentNamedAst import FunctionArgumentNamedAst
from SPPCompiler.SemanticAnalysis.ASTs.FunctionArgumentNormalAst import FunctionArgumentNormalAst

type FunctionArgumentAst = FunctionArgumentNamedAst | FunctionArgumentNormalAst

__all__ = ['FunctionArgumentAst']
