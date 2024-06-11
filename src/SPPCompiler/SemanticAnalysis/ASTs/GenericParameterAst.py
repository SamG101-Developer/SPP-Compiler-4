from __future__ import annotations
from SPPCompiler.SemanticAnalysis.ASTs.GenericParameterRequiredAst import GenericParameterRequiredAst
from SPPCompiler.SemanticAnalysis.ASTs.GenericParameterOptionalAst import GenericParameterOptionalAst
from SPPCompiler.SemanticAnalysis.ASTs.GenericParameterVariadicAst import GenericParameterVariadicAst

type GenericParameterAst = GenericParameterRequiredAst | GenericParameterOptionalAst | GenericParameterVariadicAst

__all__ = ["GenericParameterAst"]
