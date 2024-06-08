from SPPCompiler.SemanticAnalysis.ASTs.PatternVariantVariableAssignmentAst import PatternVariantVariableAssignmentAst
from SPPCompiler.SemanticAnalysis.ASTs.PatternVariantTupleAst import PatternVariantTupleAst
from SPPCompiler.SemanticAnalysis.ASTs.PatternVariantDestructureAst import PatternVariantDestructureAst
from SPPCompiler.SemanticAnalysis.ASTs.PatternVariantVariableAst import PatternVariantVariableAst
from SPPCompiler.SemanticAnalysis.ASTs.PatternVariantLiteralAst import PatternVariantLiteralAst
from SPPCompiler.SemanticAnalysis.ASTs.PatternVariantSkipArgumentAst import PatternVariantSkipArgumentAst

type PatternVariantNestedAst = (
        PatternVariantVariableAssignmentAst |
        PatternVariantTupleAst |
        PatternVariantDestructureAst |
        PatternVariantVariableAst |
        PatternVariantLiteralAst |
        PatternVariantSkipArgumentAst)


__all__ = ["PatternVariantNestedAst"]
