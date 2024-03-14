from src.SemanticAnalysis.ASTs.LocalVariableDestructureAst import LocalVariableDestructureAst
from src.SemanticAnalysis.ASTs.LocalVariableSingleAst import LocalVariableSingleAst
from src.SemanticAnalysis.ASTs.LocalVariableTupleAst import LocalVariableTupleAst

type LocalVariableAst = LocalVariableDestructureAst | LocalVariableSingleAst | LocalVariableTupleAst

__all__ = ["LocalVariableAst"]
