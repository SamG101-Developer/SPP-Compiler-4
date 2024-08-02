from SPPCompiler.SemanticAnalysis.ASTs import GenericArgumentNamedAst, GenericArgumentNormalAst

type GenericArgumentAst = GenericArgumentNormalAst | GenericArgumentNamedAst

__all__ = ["GenericArgumentAst"]
