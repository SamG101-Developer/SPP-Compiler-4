from SPPCompiler.SemanticAnalysis.ASTs.ConventionMovAst import ConventionMovAst
from SPPCompiler.SemanticAnalysis.ASTs.ConventionRefAst import ConventionRefAst
from SPPCompiler.SemanticAnalysis.ASTs.ConventionMutAst import ConventionMutAst

type ConventionAst = ConventionMovAst | ConventionRefAst | ConventionMutAst

__all__ = ["ConventionAst"]
