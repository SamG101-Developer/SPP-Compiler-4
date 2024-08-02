from SPPCompiler.SemanticAnalysis.ASTs import (
    GenericParameterRequiredAst, GenericParameterOptionalAst, GenericParameterVariadicAst)

type GenericParameterAst = GenericParameterRequiredAst | GenericParameterOptionalAst | GenericParameterVariadicAst

__all__ = ["GenericParameterAst"]
