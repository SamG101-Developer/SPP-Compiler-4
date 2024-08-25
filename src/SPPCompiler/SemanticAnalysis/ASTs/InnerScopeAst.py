from dataclasses import dataclass
from typing import List

from SPPCompiler.LexicalAnalysis.Tokens import TokenType
from SPPCompiler.SemanticAnalysis.ASTs.Meta.Ast import Ast, Default
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstMixins import SemanticAnalyser
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstPrinter import *
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstUtils import TypeInfer, InferredType
from SPPCompiler.SemanticAnalysis.Utils.SemanticError import SemanticErrors
from SPPCompiler.SemanticAnalysis.Utils.Scopes import ScopeHandler
from SPPCompiler.SemanticAnalysis.Utils.CommonTypes import CommonTypes

from SPPCompiler.Utils.Sequence import Seq


@dataclass
class InnerScopeAst[T](Ast, Default, SemanticAnalyser, TypeInfer):
    """
    The InnerScopeAst node represents a new scope, enclosed in "{}" braces, with a list of members. The members are type
    `T`.

    Attributes:
        brace_l_token: The left brace token.
        members: The list of members in the scope.
        brace_r_token: The right brace token.
    """

    brace_l_token: "TokenAst"
    members: List[T]
    brace_r_token: "TokenAst"

    @ast_printer_method_indent
    def print(self, printer: AstPrinter) -> str:
        # Print the InnerScopeAst.
        s = ""
        s += f"{self.brace_l_token.print(printer)}"
        s += f"\n{Seq(self.members).print(printer, "\n")}\n" if self.members else ""
        s += f"{self.brace_r_token.print(printer)}"
        return s

    @staticmethod
    def default() -> Default:
        from SPPCompiler.SemanticAnalysis.ASTs.TokenAst import TokenAst
        return InnerScopeAst(-1, TokenAst.dummy(TokenType.TkBraceL), [], TokenAst.dummy(TokenType.TkBraceR))

    def do_semantic_analysis(self, scope_handler, inline: bool = False, **kwargs) -> None:
        from SPPCompiler.SemanticAnalysis.ASTs import ReturnStatementAst

        # Create a new scope and add the members to it.
        if not inline: scope_handler.into_new_scope(f"<inner_scope: {id(self)}>")

        for member in self.members:
            member.do_semantic_analysis(scope_handler, **kwargs)

        # Ensure code doesn't come after a return statement at the current level.
        for i, member in Seq(self.members).enumerate():
            if isinstance(member, ReturnStatementAst) and member is not self.members[-1]:
                raise SemanticErrors.UNREACHABLE_CODE(member, self.members[i + 1])

        if not inline: scope_handler.exit_cur_scope()

    def infer_type(self, scope_handler: ScopeHandler, **kwargs) -> InferredType:
        from SPPCompiler.SemanticAnalysis.ASTs import ConventionMovAst

        # The scope's returning type is the final expression in the "self.members" list.
        if self.members:
            return self.members[-1].infer_type(scope_handler, **kwargs)
        return InferredType(convention=ConventionMovAst, type=CommonTypes.void())


__all__ = ["InnerScopeAst"]
