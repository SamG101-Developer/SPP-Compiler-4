from SPPCompiler.SemanticAnalysis.ASTs.LocalVariableAssignmentAst import LocalVariableAssignmentAst
from SPPCompiler.SemanticAnalysis.ASTs.LocalVariableDestructureAst import LocalVariableDestructureAst
from SPPCompiler.SemanticAnalysis.ASTs.LocalVariableSingleAst import LocalVariableSingleAst
from SPPCompiler.SemanticAnalysis.ASTs.LocalVariableSkipArgumentsAst import LocalVariableSkipArgumentsAst
from SPPCompiler.SemanticAnalysis.ASTs.LocalVariableTupleAst import LocalVariableTupleAst

type LocalVariableNestedAst = (
        LocalVariableAssignmentAst | LocalVariableDestructureAst | LocalVariableSingleAst |
        LocalVariableSkipArgumentsAst | LocalVariableTupleAst)

__all__ = ["LocalVariableNestedAst"]
