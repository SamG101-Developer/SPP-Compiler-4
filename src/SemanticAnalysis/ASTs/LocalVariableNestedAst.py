from src.SemanticAnalysis.ASTs.LocalVariableAssignmentAst import LocalVariableAssignmentAst
from src.SemanticAnalysis.ASTs.LocalVariableDestructureAst import LocalVariableDestructureAst
from src.SemanticAnalysis.ASTs.LocalVariableSingleAst import LocalVariableSingleAst
from src.SemanticAnalysis.ASTs.LocalVariableSkipArgumentAst import LocalVariableSkipArgumentAst
from src.SemanticAnalysis.ASTs.LocalVariableTupleAst import LocalVariableTupleAst

type LocalVariableNestedAst = (
        LocalVariableAssignmentAst | LocalVariableDestructureAst | LocalVariableSingleAst |
        LocalVariableSkipArgumentAst | LocalVariableTupleAst)

__all__ = ["LocalVariableNestedAst"]
