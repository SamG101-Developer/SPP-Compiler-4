from src.SemanticAnalysis.ASTs.ConventionMovAst import ConventionMovAst
from src.SemanticAnalysis.ASTs.ConventionRefAst import ConventionRefAst
from src.SemanticAnalysis.ASTs.ConventionMutAst import ConventionMutAst

type ConventionAst = ConventionMovAst | ConventionRefAst | ConventionMutAst

__all__ = ["ConventionAst"]
