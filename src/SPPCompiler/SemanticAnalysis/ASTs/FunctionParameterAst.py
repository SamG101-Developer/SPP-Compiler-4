from SPPCompiler.SemanticAnalysis.ASTs import (
    FunctionParameterSelfAst, FunctionParameterRequiredAst, FunctionParameterOptionalAst, FunctionParameterVariadicAst)

type FunctionParameterAst = (
    FunctionParameterSelfAst | FunctionParameterRequiredAst | FunctionParameterOptionalAst |
    FunctionParameterVariadicAst)

__all__ = ["FunctionParameterAst"]
