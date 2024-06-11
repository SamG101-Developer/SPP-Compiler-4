from SPPCompiler.SemanticAnalysis.ASTs.LocalVariableDestructureAst import LocalVariableDestructureAst
from SPPCompiler.SemanticAnalysis.ASTs.LocalVariableSingleAst import LocalVariableSingleAst
from SPPCompiler.SemanticAnalysis.ASTs.LocalVariableTupleAst import LocalVariableTupleAst

type LocalVariableAst = LocalVariableDestructureAst | LocalVariableSingleAst | LocalVariableTupleAst

__all__ = ["LocalVariableAst"]
