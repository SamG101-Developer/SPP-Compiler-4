from SPPCompiler.SemanticAnalysis.ASTs import (
    PatternVariantObjectDestructureAst, PatternVariantElseAst, PatternVariantLiteralAst,
    PatternVariantTupleDestructureAst, PatternVariantSingleIdentifierAst, PatternVariantAttributeBindingAst,
    PatternVariantSkipArgumentAst, PatternVariantSkipArgumentsAst, PatternVariantUnionDestructureAst)

type PatternVariantAst = (
    PatternVariantObjectDestructureAst | PatternVariantElseAst | PatternVariantLiteralAst |
    PatternVariantTupleDestructureAst | PatternVariantSingleIdentifierAst)

type PatternVariantDestructureAst = (
    PatternVariantTupleDestructureAst | PatternVariantObjectDestructureAst | PatternVariantUnionDestructureAst)

type PatternVariantNestedForObjectDestructureAst = (
    PatternVariantAttributeBindingAst, PatternVariantSingleIdentifierAst, PatternVariantSkipArgumentsAst)

type PatternVariantNestedForTupleDestructureAst = (
    PatternVariantTupleDestructureAst, PatternVariantObjectDestructureAst, PatternVariantSingleIdentifierAst,
    PatternVariantLiteralAst, PatternVariantSkipArgumentsAst, PatternVariantSkipArgumentAst)

type PatternVariantNestedForAttributeBindingAst = (
    PatternVariantTupleDestructureAst, PatternVariantObjectDestructureAst, PatternVariantSingleIdentifierAst,
    PatternVariantLiteralAst)

__all__ = [
    "PatternVariantAst", "PatternVariantDestructureAst", "PatternVariantNestedForObjectDestructureAst",
    "PatternVariantNestedForTupleDestructureAst", "PatternVariantNestedForAttributeBindingAst"]
