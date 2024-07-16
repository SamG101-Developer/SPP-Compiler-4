from SPPCompiler.SemanticAnalysis.ASTs.PatternVariantVariableAssignmentAst import PatternVariantVariableAssignmentAst
from SPPCompiler.SemanticAnalysis.ASTs.PatternVariantTupleAst import PatternVariantTupleAst
from SPPCompiler.SemanticAnalysis.ASTs.PatternVariantDestructureAst import PatternVariantDestructureAst
from SPPCompiler.SemanticAnalysis.ASTs.PatternVariantVariableAst import PatternVariantVariableAst
from SPPCompiler.SemanticAnalysis.ASTs.PatternVariantLiteralAst import PatternVariantLiteralAst
from SPPCompiler.SemanticAnalysis.ASTs.PatternVariantSkipArgumentsAst import PatternVariantSkipArgumentsAst

type PatternVariantNestedAst = (
        PatternVariantVariableAssignmentAst |
        PatternVariantTupleAst |
        PatternVariantDestructureAst |
        PatternVariantVariableAst |
        PatternVariantLiteralAst |
        PatternVariantSkipArgumentsAst)


__all__ = ["PatternVariantNestedAst"]
