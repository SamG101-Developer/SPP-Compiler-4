from SPPCompiler.SemanticAnalysis.ASTs import ConventionMovAst, ConventionRefAst, ConventionMutAst

type ConventionAst = ConventionMovAst | ConventionRefAst | ConventionMutAst

__all__ = ["ConventionAst"]
