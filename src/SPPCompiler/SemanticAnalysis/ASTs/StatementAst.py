from SPPCompiler.SemanticAnalysis.ASTs.AssignmentStatementAst import AssignmentStatementAst
from SPPCompiler.SemanticAnalysis.ASTs.ExpressionAst import ExpressionAst
from SPPCompiler.SemanticAnalysis.ASTs.LetStatementAst import LetStatementAst
from SPPCompiler.SemanticAnalysis.ASTs.ReturnStatementAst import ReturnStatementAst
# from SPPCompiler.SemanticAnalysis.ASTs.TypedefStatementAst import TypedefStatementAst

type StatementAst = AssignmentStatementAst | LetStatementAst | ReturnStatementAst | ExpressionAst  # | TypedefStatementAst

__all__ = ["StatementAst"]
