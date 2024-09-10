from SPPCompiler.SemanticAnalysis.ASTs.PatternVariantAttributeBindingAst import PatternVariantAttributeBindingAst
from SPPCompiler.SemanticAnalysis.ASTs.PatternVariantElseAst import PatternVariantElseAst
from SPPCompiler.SemanticAnalysis.ASTs.PatternVariantLiteralAst import PatternVariantLiteralAst
from SPPCompiler.SemanticAnalysis.ASTs.PatternVariantObjectDestructureAst import PatternVariantObjectDestructureAst
from SPPCompiler.SemanticAnalysis.ASTs.PatternVariantSingleIdentifierAst import PatternVariantSingleIdentifierAst
from SPPCompiler.SemanticAnalysis.ASTs.PatternVariantSkipArgumentAst import PatternVariantSkipArgumentAst
from SPPCompiler.SemanticAnalysis.ASTs.PatternVariantSkipArgumentsAst import PatternVariantSkipArgumentsAst
from SPPCompiler.SemanticAnalysis.ASTs.PatternVariantTupleDestructureAst import PatternVariantTupleDestructureAst

type PatternVariantAst = (
    PatternVariantObjectDestructureAst | PatternVariantElseAst | PatternVariantLiteralAst |
    PatternVariantTupleDestructureAst | PatternVariantSingleIdentifierAst)

type PatternGroupDestructureAst = (
    PatternVariantTupleDestructureAst | PatternVariantObjectDestructureAst)

type PatternVariantNestedForObjectDestructureAst = (
    PatternVariantAttributeBindingAst | PatternVariantSingleIdentifierAst | PatternVariantSkipArgumentsAst)

type PatternVariantNestedForTupleDestructureAst = (
    PatternVariantTupleDestructureAst | PatternVariantObjectDestructureAst | PatternVariantSingleIdentifierAst |
    PatternVariantLiteralAst | PatternVariantSkipArgumentsAst | PatternVariantSkipArgumentAst)

type PatternVariantNestedForAttributeBindingAst = (
    PatternVariantTupleDestructureAst | PatternVariantObjectDestructureAst | PatternVariantSingleIdentifierAst |
    PatternVariantLiteralAst)

__all__ = [
    "PatternVariantAst", "PatternGroupDestructureAst", "PatternVariantNestedForObjectDestructureAst",
    "PatternVariantNestedForTupleDestructureAst", "PatternVariantNestedForAttributeBindingAst"]
