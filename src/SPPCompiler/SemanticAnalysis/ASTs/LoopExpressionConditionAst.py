from SPPCompiler.SemanticAnalysis.ASTs import LoopExpressionConditionBooleanAst
from SPPCompiler.SemanticAnalysis.ASTs import LoopExpressionConditionIterableAst

type LoopExpressionConditionAst = LoopExpressionConditionBooleanAst | LoopExpressionConditionIterableAst

__all__ = ["LoopExpressionConditionAst"]
