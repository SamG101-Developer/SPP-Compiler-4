from SPPCompiler.SemanticAnalysis.ASTs import (
    LocalVariableObjectDestructureAst, LocalVariableSingleIdentifierAst, LocalVariableTupleDestructureAst,
    LocalVariableAttributeBindingAst, LocalVariableSkipArgumentAst, LocalVariableSkipArgumentsAst)

type LocalVariableAst = (
    LocalVariableObjectDestructureAst | LocalVariableSingleIdentifierAst | LocalVariableTupleDestructureAst)

type LocalVariableNestedForObjectDestructureAst = (
    LocalVariableAttributeBindingAst | LocalVariableSingleIdentifierAst | LocalVariableSkipArgumentsAst)

type LocalVariableNestedForTupleDestructureAst = (
    LocalVariableTupleDestructureAst | LocalVariableNestedForObjectDestructureAst | LocalVariableSingleIdentifierAst,
    LocalVariableSkipArgumentAst | LocalVariableSkipArgumentsAst)

type LocalVariableNestedForAttributeBindingAst = (
    LocalVariableNestedForObjectDestructureAst | LocalVariableTupleDestructureAst | LocalVariableSingleIdentifierAst)

__all__ = [
    "LocalVariableAst", "LocalVariableNestedForObjectDestructureAst", "LocalVariableNestedForTupleDestructureAst",
    "LocalVariableNestedForAttributeBindingAst"]
