from dataclasses import dataclass
from typing import List

from SPPCompiler.SemanticAnalysis.ASTs.Meta.Ast import Ast
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstMixins import SemanticAnalyser
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstPrinter import *
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstUtils import ensure_memory_integrity, InferredType
from SPPCompiler.SemanticAnalysis.Utils.CommonTypes import CommonTypes
from SPPCompiler.SemanticAnalysis.Utils.SemanticError import SemanticErrors
from SPPCompiler.SemanticAnalysis.Utils.Scopes import ScopeHandler
from SPPCompiler.Utils.Sequence import Seq


@dataclass
class LoopControlFlowStatementAst(Ast, SemanticAnalyser):
    """
    The LoopControlFlowStatementAst represents a control flow keyword(s) for a loop. This might look like "exit exit 1",
    which exits ("break"s) from the current loop, the immediate outer loop, and returns "1" to the let statement that
    started the loop. Instead of "1", the "skip" keyword might be used, so "exit skip" would exit the current loop, then
    start the outer loop from the top again (like a "continue" would). Finally, "skip" can be used on its own to go back
    to the top of the current loop.

    Note that whilst the "exit_tokens" and "skip_or_expr" can both be empty, one of them will always be present due to
    alternative parsing rules.

    Attributes:
        exit_tokens: The list of "exit" tokens for the loop control flow statement (can be empty).
        skip_or_expr: The "skip" token or expression to return to the let statement that started the loop (can be None).
    """

    exit_tokens: List["TokenAst"]
    skip_or_expr: "ExpressionAst"

    def print(self, printer: AstPrinter) -> str:
        # Print the LoopControlFlowStatementAst.
        s = ""
        s += f"{Seq(self.exit_tokens).print(printer)} "
        s += f"{self.skip_or_expr.print(printer)}" if self.skip_or_expr else ""
        return s

    def do_semantic_analysis(self, scope_handler: ScopeHandler, **kwargs) -> None:
        from SPPCompiler.LexicalAnalysis.Tokens import TokenType
        from SPPCompiler.SemanticAnalysis.ASTs import TokenAst, ConventionMovAst

        # Get the number of control flow statements, and the number of loops present.
        number_of_controls = len(self.exit_tokens) + (self.skip_or_expr == TokenAst.dummy(TokenType.KwSkip))
        number_of_loops    = kwargs.get("loop-count", 0)

        # Ensure that the number of loops being controlled from this statement is actually present.
        if number_of_controls > number_of_loops:
            raise SemanticErrors.CONTROL_FLOW_TOO_MANY_LOOPS(self, number_of_controls, number_of_loops)

        # Analyse the exiting expression if there is one.
        if self.skip_or_expr and self.skip_or_expr != TokenAst.dummy(TokenType.KwSkip):
            self.skip_or_expr.do_semantic_analysis(scope_handler, **kwargs)

        # Save or compare the exiting type to other exit-statement's types to ensure they match.
        if not isinstance(self.skip_or_expr, TokenAst):
            ensure_memory_integrity(self, self.skip_or_expr, self.exit_tokens[-1], scope_handler)

            match self.skip_or_expr:
                case None: this_exit_type = InferredType.from_type_ast(CommonTypes.void(self.exit_tokens[-1].pos))
                case _: this_exit_type = self.skip_or_expr.infer_type(scope_handler, **kwargs)

            n = number_of_loops - number_of_controls + 1
            if n not in kwargs["loop-types"]:
                kwargs["loop-types"][n] = (self.skip_or_expr or self.exit_tokens[-1], this_exit_type)
            else:
                that, that_exit_type = kwargs["loop-types"][n]
                if not this_exit_type.symbolic_eq(that_exit_type, scope_handler.current_scope):
                    raise SemanticErrors.CONTROL_FLOW_TYPE_MISMATCH(that, self.skip_or_expr or self.exit_tokens[-1], that_exit_type, this_exit_type)


__all__ = ["LoopControlFlowStatementAst"]
