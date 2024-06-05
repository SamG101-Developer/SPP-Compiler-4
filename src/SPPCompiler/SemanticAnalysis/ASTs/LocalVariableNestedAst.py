from SPPCompiler.SemanticAnalysis.ASTs.LocalVariableAssignmentAst import LocalVariableAssignmentAst
from SPPCompiler.SemanticAnalysis.ASTs.LocalVariableDestructureAst import LocalVariableDestructureAst
from SPPCompiler.SemanticAnalysis.ASTs.LocalVariableSingleAst import LocalVariableSingleAst
from SPPCompiler.SemanticAnalysis.ASTs.LocalVariableSkipArgumentAst import LocalVariableSkipArgumentAst
from SPPCompiler.SemanticAnalysis.ASTs.LocalVariableTupleAst import LocalVariableTupleAst

type LocalVariableNestedAst = (
        LocalVariableAssignmentAst | LocalVariableDestructureAst | LocalVariableSingleAst |
        LocalVariableSkipArgumentAst | LocalVariableTupleAst)

__all__ = ["LocalVariableNestedAst"]
