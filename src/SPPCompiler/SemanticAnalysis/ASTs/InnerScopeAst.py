from dataclasses import dataclass
from typing import List, Tuple, Type

from SPPCompiler.SemanticAnalysis.ASTMixins.SemanticAnalyser import SemanticAnalyser
from SPPCompiler.SemanticAnalysis.ASTMixins.TypeInfer import TypeInfer, InferredType
from SPPCompiler.SemanticAnalysis.Utils.SemanticError import SemanticError, SemanticErrorType
from SPPCompiler.SemanticAnalysis.Utils.Scopes import ScopeHandler
from SPPCompiler.SemanticAnalysis.Utils.CommonTypes import CommonTypes

from SPPCompiler.SemanticAnalysis.ASTs.Meta.Ast import Ast
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstPrinter import *

from SPPCompiler.SemanticAnalysis.ASTs.ConventionMovAst import ConventionMovAst
from SPPCompiler.SemanticAnalysis.ASTs.TokenAst import TokenAst

from SPPCompiler.Utils.Sequence import Seq


@dataclass
class InnerScopeAst[T](Ast, SemanticAnalyser, TypeInfer):
    """
    The InnerScopeAst node represents a new scope, enclosed in "{}" braces, with a list of members. The members are type
    `T`.

    Attributes:
        - brace_l_token: The left brace token.
        - members: The list of members in the scope.
        - brace_r_token: The right brace token.
    """

    brace_l_token: TokenAst
    members: List[T]
    brace_r_token: TokenAst

    @ast_printer_method_indent
    def print(self, printer: AstPrinter) -> str:
        # Print the InnerScopeAst.
        s = ""
        s += f"{self.brace_l_token.print(printer)}"
        s += f"\n{Seq(self.members).print(printer, "\n")}\n" if self.members else ""
        s += f"{self.brace_r_token.print(printer)}"
        return s

    def do_semantic_analysis(self, scope_handler, inline: bool = False, **kwargs) -> None:
        from SPPCompiler.SemanticAnalysis.ASTs.ReturnStatementAst import ReturnStatementAst

        # Ensure code doesn't come after a return statement at the current level.
        statement_types = Seq(self.members).map(type)
        if ReturnStatementAst in statement_types:
            index = statement_types.index(ReturnStatementAst)
            if index != -1:
                exception = SemanticError()
                exception.add_info(
                    pos=self.members[index].pos, tag_message="Return statement found.")
                exception.add_error(
                    pos=self.brace_r_token.pos, error_type=SemanticErrorType.ORDER_ERROR,
                    tag_message="Unreachable code detected.",
                    message="Code after a return statement is unreachable.",
                    tip="Ensure that no code comes after a return statement.")
                raise exception

        # Create a new scope and add the members to it.
        if not inline: scope_handler.into_new_scope(f"<inner_scope: {id(self)}>")
        Seq(self.members).for_each(lambda x: x.do_semantic_analysis(scope_handler, **kwargs))
        if not inline: scope_handler.exit_cur_scope()

    def infer_type(self, scope_handler: ScopeHandler, **kwargs) -> InferredType:
        # The returning type of a scope is the final expression in the "self.members" list.
        if self.members:
            return self.members[-1].infer_type(scope_handler, **kwargs)
        return InferredType(convention=ConventionMovAst, type=CommonTypes.void())


__all__ = ["InnerScopeAst"]
