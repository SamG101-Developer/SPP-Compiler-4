from __future__ import annotations

from dataclasses import dataclass

from SPPCompiler.LexicalAnalysis.Tokens import TokenType
from SPPCompiler.SemanticAnalysis.ASTs.Meta.Ast import Ast, Default
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstMixins import SemanticAnalyser
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstPrinter import *
from SPPCompiler.SemanticAnalysis.Utils.Scopes import ScopeHandler


@dataclass
class WhereBlockAst(Ast, Default, SemanticAnalyser):
    """
    The WhereBlockAst node is used to represent a group of where constraints in a function/class/superimposition, and
    includes the "where" keyword and the constraints.

    Attributes:
        - where_keyword: The "where" keyword token.
        - constraint_group: The group of where constraints.
    """

    where_keyword: "TokenAst"
    constraint_group: "WhereConstraintsGroupAst"

    def __post_init__(self):
        ...
        # raise NotImplementedError("WhereBlockAst is not implemented yet.")

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        # Print the WhereBlockAst.
        s = ""
        if self.constraint_group.constraints:
            s += f"{self.where_keyword.print(printer)} "
            s += f"{self.constraint_group.print(printer)}"
        return s

    @staticmethod
    def default() -> WhereBlockAst:
        from SPPCompiler.SemanticAnalysis.ASTs import WhereConstraintsGroupAst, TokenAst

        # Create an empty constraints group.
        where_constraints_group = WhereConstraintsGroupAst(
            pos=-1,
            brack_l_token=TokenAst.dummy(TokenType.TkBrackL),
            constraints=[],
            brack_r_token=TokenAst.dummy(TokenType.TkBrackR))

        # Create the where block.
        where_block = WhereBlockAst(
            pos=-1,
            where_keyword=TokenAst.dummy(TokenType.KwWhere),
            constraint_group=where_constraints_group)

        return where_block

    def __eq__(self, other):
        # Check both ASTs are the same type and have the same constraint group.
        return isinstance(other, WhereBlockAst) and self.constraint_group == other.constraint_group

    def do_semantic_analysis(self, scope_handler: ScopeHandler, **kwargs) -> None:
        ...


__all__ = ["WhereBlockAst"]
