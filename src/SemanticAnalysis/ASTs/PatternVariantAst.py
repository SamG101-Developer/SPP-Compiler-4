from src.SemanticAnalysis.ASTs.PatternVariantBoolMemberAst import PatternVariantBoolMemberAst
from src.SemanticAnalysis.ASTs.PatternVariantDestructureAst import PatternVariantDestructureAst
from src.SemanticAnalysis.ASTs.PatternVariantElseAst import PatternVariantElseAst
from src.SemanticAnalysis.ASTs.PatternVariantLiteralAst import PatternVariantLiteralAst
from src.SemanticAnalysis.ASTs.PatternVariantTupleAst import PatternVariantTupleAst
from src.SemanticAnalysis.ASTs.PatternVariantVariableAst import PatternVariantVariableAst

type PatternVariantAst = (
        PatternVariantBoolMemberAst | PatternVariantDestructureAst | PatternVariantElseAst |
        PatternVariantLiteralAst | PatternVariantTupleAst | PatternVariantVariableAst)

__all__ = ["PatternVariantAst"]
