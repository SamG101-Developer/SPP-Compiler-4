from __future__ import annotations
from dataclasses import dataclass
from typing import List

from src.LexicalAnalysis.Tokens import TokenType

from src.SemanticAnalysis.ASTMixins.SemanticAnalyser import SemanticAnalyser
from src.SemanticAnalysis.Utils.SemanticError import SemanticError, SemanticErrorType
from src.SemanticAnalysis.Utils.Scopes import ScopeHandler
from src.SemanticAnalysis.Utils.Symbols import TypeSymbol

from src.SemanticAnalysis.ASTs.Meta.Ast import Ast, Default
from src.SemanticAnalysis.ASTs.Meta.AstPrinter import *

from src.SemanticAnalysis.ASTs.GenericParameterAst import GenericParameterAst
from src.SemanticAnalysis.ASTs.GenericParameterRequiredAst import GenericParameterRequiredAst
from src.SemanticAnalysis.ASTs.GenericParameterOptionalAst import GenericParameterOptionalAst
from src.SemanticAnalysis.ASTs.GenericParameterVariadicAst import GenericParameterVariadicAst
from src.SemanticAnalysis.ASTs.TokenAst import TokenAst

from src.Utils.Sequence import Seq


@dataclass
class GenericParameterGroupAst(Ast, Default, SemanticAnalyser):
    """
    The GenericParameterGroupAst node is used to represent a group of generic parameters in a
    function/class/superimposition prototype, NOT the arguments to a function call or object instantiation (see
    GenericArgumentGroupAst).

    Attributes:
        - bracket_l_token: The left bracket token.
        - parameters: The generic parameters of the function/class/superimposition prototype.
        - bracket_r_token: The right bracket token.
    """

    bracket_l_token: TokenAst
    parameters: List[GenericParameterAst]
    bracket_r_token: TokenAst

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        # Print the GenericParameterGroupAst.
        s = ""
        if self.parameters:
            s += f"{self.bracket_l_token.print(printer)}"
            s += Seq(self.parameters).print(printer, ", ")
            s += f"{self.bracket_r_token.print(printer)}"
        return s

    @staticmethod
    def default() -> GenericParameterGroupAst:
        # Create a default GenericParameterGroupAst.
        return GenericParameterGroupAst(
            pos=-1,
            bracket_l_token=TokenAst.dummy(TokenType.TkBrackL),
            parameters=[],
            bracket_r_token=TokenAst.dummy(TokenType.TkBrackR))

    def do_semantic_analysis(self, scope_handler: ScopeHandler, **kwargs) -> None:
        # Check no parameters have the same name
        if Seq(self.parameters).map(lambda p: p.identifier).contains_duplicates():
            duplicate_parameters = Seq(self.parameters).map(lambda p: p.identifier).non_unique_items()[0]
            exception = SemanticError()
            exception.add_info(
                pos=duplicate_parameters[0].pos,
                tag_message=f"Generic parameter '{duplicate_parameters[0]}' declared here")
            exception.add_error(
                pos=duplicate_parameters[1].pos,
                error_type=SemanticErrorType.NAME_ERROR,
                message=f"Cannot have duplicate generic parameters in function prototype",
                tag_message=f"Generic parameter '{duplicate_parameters[1]}' re-declared here",
                tip="Change the name of one of the generic parameters to be unique")
            raise exception

        # Add each parameter to the scope. TODO: remove from here + test (its in generate stage) or remove from generate stage
        for generic_parameter in self.parameters:
            scope_handler.current_scope.add_symbol(TypeSymbol(generic_parameter.identifier, None))

        # Check parameter order is Self -> Required -> Optional -> Variadic
        ordering = {GenericParameterRequiredAst: "Required", GenericParameterOptionalAst: "Optional", GenericParameterVariadicAst: "Variadic"}
        current_classifications = Seq(self.parameters).map(lambda p: (type(p), p))
        sorted_classifications = current_classifications.sort(key=lambda t: list(ordering.keys()).index(t[0]))
        if current_classifications != sorted_classifications:
            difference = sorted_classifications.ordered_difference(current_classifications)
            exception = SemanticError()
            exception.add_info(
                pos=difference[-2][1].identifier.pos,
                tag_message=f"{ordering[difference[-2][0]]} generic parameter '{difference[-2][1].identifier}' declared here")
            exception.add_error(
                pos=difference[-1][1].identifier.pos,
                error_type=SemanticErrorType.ORDER_ERROR,
                message=f"Invalid generic parameter order in function prototype",
                tag_message=f"{ordering[difference[-1][0]]} generic parameter '{difference[-1][1].identifier}' declared here",
                tip="Make sure generic parameter order is Required -> Optional -> Variadic")
            raise exception

        # Do semantic analysis on each parameter
        Seq(self.parameters).for_each(lambda p: p.do_semantic_analysis(scope_handler, **kwargs))

    def get_req(self) -> List[GenericParameterRequiredAst]:
        # Get all the required generic parameters.
        return [p for p in self.parameters if isinstance(p, GenericParameterRequiredAst)]

    def get_opt(self) -> List[GenericParameterOptionalAst]:
        # Get all the optional generic parameters.
        return [p for p in self.parameters if isinstance(p, GenericParameterOptionalAst)]

    def get_var(self) -> List[GenericParameterVariadicAst]:
        # Get all the variadic generic parameters.
        return [p for p in self.parameters if isinstance(p, GenericParameterVariadicAst)]


__all__ = ["GenericParameterGroupAst"]
