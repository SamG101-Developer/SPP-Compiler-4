from src.SemanticAnalysis.ASTs.AssignmentStatementAst import AssignmentStatementAst
from src.SemanticAnalysis.ASTs.ExpressionAst import ExpressionAst
from src.SemanticAnalysis.ASTs.LetStatementAst import LetStatementAst
from src.SemanticAnalysis.ASTs.ReturnStatementAst import ReturnStatementAst
from src.SemanticAnalysis.ASTs.TypedefStatementAst import TypedefStatementAst

type StatementAst = AssignmentStatementAst | LetStatementAst | ReturnStatementAst | TypedefStatementAst | ExpressionAst

__all__ = ["StatementAst"]
