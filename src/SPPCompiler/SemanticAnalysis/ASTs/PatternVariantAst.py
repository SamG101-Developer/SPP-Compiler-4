from SPPCompiler.SemanticAnalysis.ASTs import (
    PatternVariantObjectDestructureAst, PatternVariantElseAst, PatternVariantLiteralAst,
    PatternVariantTupleDestructureAst, PatternVariantSingleIdentifierAst, PatternVariantAttributeBindingAst,
    PatternVariantSkipArgumentAst, PatternVariantSkipArgumentsAst)

type PatternVariantAst = (
    PatternVariantObjectDestructureAst | PatternVariantElseAst | PatternVariantLiteralAst |
    PatternVariantTupleDestructureAst | PatternVariantSingleIdentifierAst)

type PatternGroupDestructureAst = (
    PatternVariantTupleDestructureAst | PatternVariantObjectDestructureAst)

type PatternVariantNestedForObjectDestructureAst = (
    PatternVariantAttributeBindingAst, PatternVariantSingleIdentifierAst, PatternVariantSkipArgumentsAst)

type PatternVariantNestedForTupleDestructureAst = (
    PatternVariantTupleDestructureAst, PatternVariantObjectDestructureAst, PatternVariantSingleIdentifierAst,
    PatternVariantLiteralAst, PatternVariantSkipArgumentsAst, PatternVariantSkipArgumentAst)

type PatternVariantNestedForAttributeBindingAst = (
    PatternVariantTupleDestructureAst, PatternVariantObjectDestructureAst, PatternVariantSingleIdentifierAst,
    PatternVariantLiteralAst)

__all__ = [
    "PatternVariantAst", "PatternGroupDestructureAst", "PatternVariantNestedForObjectDestructureAst",
    "PatternVariantNestedForTupleDestructureAst", "PatternVariantNestedForAttributeBindingAst"]
