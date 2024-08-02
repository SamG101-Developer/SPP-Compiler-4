from SPPCompiler.SemanticAnalysis.ASTs import FunctionArgumentNamedAst, FunctionArgumentNormalAst

type FunctionArgumentAst = FunctionArgumentNamedAst | FunctionArgumentNormalAst

__all__ = ["FunctionArgumentAst"]
