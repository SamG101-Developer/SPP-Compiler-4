from SPPCompiler.SemanticAnalysis.ASTs.PatternVariantBoolMemberAst import PatternVariantBoolMemberAst
from SPPCompiler.SemanticAnalysis.ASTs.PatternVariantDestructureAst import PatternVariantDestructureAst
from SPPCompiler.SemanticAnalysis.ASTs.PatternVariantElseAst import PatternVariantElseAst
from SPPCompiler.SemanticAnalysis.ASTs.PatternVariantLiteralAst import PatternVariantLiteralAst
from SPPCompiler.SemanticAnalysis.ASTs.PatternVariantTupleAst import PatternVariantTupleAst
from SPPCompiler.SemanticAnalysis.ASTs.PatternVariantVariableAst import PatternVariantVariableAst

type PatternVariantAst = (
        PatternVariantBoolMemberAst | PatternVariantDestructureAst | PatternVariantElseAst |
        PatternVariantLiteralAst | PatternVariantTupleAst | PatternVariantVariableAst)

__all__ = ["PatternVariantAst"]
