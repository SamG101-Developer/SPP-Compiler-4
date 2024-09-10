from SPPCompiler.SemanticAnalysis.ASTs import (
    AssignmentStatementAst, ExpressionAst, LetStatementAst, ReturnStatementAst, UseStatementAst, PinStatementAst,
    RelStatementAst, LoopControlFlowStatementAst)

type StatementAst = (
    AssignmentStatementAst | LetStatementAst | ReturnStatementAst | ExpressionAst | UseStatementAst | PinStatementAst |
    RelStatementAst | LoopControlFlowStatementAst)

__all__ = ["StatementAst"]
