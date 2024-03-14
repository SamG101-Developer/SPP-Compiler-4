from src.SemanticAnalysis.ASTs.PatternVariantVariableAssignmentAst import PatternVariantVariableAssignmentAst
from src.SemanticAnalysis.ASTs.PatternVariantTupleAst import PatternVariantTupleAst
from src.SemanticAnalysis.ASTs.PatternVariantDestructureAst import PatternVariantDestructureAst
from src.SemanticAnalysis.ASTs.PatternVariantVariableAst import PatternVariantVariableAst
from src.SemanticAnalysis.ASTs.PatternVariantLiteralAst import PatternVariantLiteralAst
from src.SemanticAnalysis.ASTs.PatternVariantSkipArgumentAst import PatternVariantSkipArgumentAst

type PatternVariantNestedAst = (
        PatternVariantVariableAssignmentAst |
        PatternVariantTupleAst |
        PatternVariantDestructureAst |
        PatternVariantVariableAst |
        PatternVariantLiteralAst |
        PatternVariantSkipArgumentAst)

__all__ = ["PatternVariantNestedAst"]
