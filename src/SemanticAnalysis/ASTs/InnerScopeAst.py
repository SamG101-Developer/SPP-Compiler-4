from dataclasses import dataclass
from typing import List, Tuple, Type

from src.SemanticAnalysis.ASTMixins.SemanticAnalyser import SemanticAnalyser
from src.SemanticAnalysis.Utils.SemanticError import SemanticError, SemanticErrorType
from src.SemanticAnalysis.Utils.Scopes import ScopeHandler
from src.SemanticAnalysis.Utils.CommonTypes import CommonTypes
from src.SemanticAnalysis.ASTMixins.TypeInfer import TypeInfer

from src.SemanticAnalysis.ASTs.Meta.Ast import Ast
from src.SemanticAnalysis.ASTs.Meta.AstPrinter import *

from src.SemanticAnalysis.ASTs.ConventionAst import ConventionAst
from src.SemanticAnalysis.ASTs.ConventionMovAst import ConventionMovAst
from src.SemanticAnalysis.ASTs.TokenAst import TokenAst
from src.SemanticAnalysis.ASTs.TypeAst import TypeAst

from src.Utils.Sequence import Seq


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

    def do_semantic_analysis(self, scope_handler, inline_block: bool = False, **kwargs) -> None:
        from src.SemanticAnalysis.ASTs.ReturnStatementAst import ReturnStatementAst

        # Make sure nothing comes after a return statement.
        return_encountered = None
        for member in self.members:
            if return_encountered:
                exception = SemanticError()
                exception.add_info(
                    pos=return_encountered.pos,
                    tag_message=f"Return statement found here.")
                exception.add_error(
                    pos=member.pos,
                    error_type=SemanticErrorType.ORDER_ERROR,
                    message="Unreachable code",
                    tag_message=f"Unreachable code found here.",
                    tip="Remove the unreachable code, or move the return statement to the end of the scope.")
                raise exception
            if isinstance(member, ReturnStatementAst):
                return_encountered = member

        # When a new scope is unwanted, analyse the members in the current scope.
        if inline_block:
            Seq(self.members).for_each(lambda m: m.do_semantic_analysis(scope_handler, **kwargs))

        # When a new scope is wanted (default behaviour), create a new scope and analyse the members in the new scope.
        else:
            scope_handler.into_new_scope(f"<inner-scope: child-of:[{scope_handler.current_scope._scope_name}]>")
            Seq(self.members).for_each(lambda m: m.do_semantic_analysis(scope_handler, **kwargs))
            scope_handler.exit_cur_scope()

    def infer_type(self, scope_handler: ScopeHandler, **kwargs) -> Tuple[Type[ConventionAst], TypeAst]:
        # The returning type of a scope is the final expression in the "self.members" list.
        return self.members[-1].infer_type(scope_handler, **kwargs) if self.members else (ConventionMovAst, CommonTypes.void())


__all__ = ["InnerScopeAst"]
