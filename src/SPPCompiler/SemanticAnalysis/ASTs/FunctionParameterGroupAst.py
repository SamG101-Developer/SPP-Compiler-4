from dataclasses import dataclass
from typing import List, Optional

from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstMixins import SemanticAnalyser
from SPPCompiler.SemanticAnalysis.Utils.SemanticError import SemanticErrors
from SPPCompiler.SemanticAnalysis.Utils.Scopes import ScopeHandler

from SPPCompiler.SemanticAnalysis.ASTs.Meta.Ast import Ast
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstPrinter import *

from SPPCompiler.Utils.Sequence import Seq


@dataclass
class FunctionParameterGroupAst(Ast, SemanticAnalyser):
    """
    The FunctionParameterGroupAst node is used to represent a group of function parameters in a function prototype, NOT
    the arguments to a function call (see FunctionArgumentGroupAst).

    Attributes:
        paren_l_token: The left parenthesis token.
        parameters: The parameters of the function prototype.
        paren_r_token: The right parenthesis token.
    """

    paren_l_token: "TokenAst"
    parameters: List["FunctionParameterAst"]
    paren_r_token: "TokenAst"

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        # Print the FunctionParameterGroupAst.
        s = ""
        s += f"{self.paren_l_token.print(printer)}"
        s += f"{Seq(self.parameters).print(printer, ", ")}"
        s += f"{self.paren_r_token.print(printer)}"
        return s

    def do_semantic_analysis(self, scope_handler: ScopeHandler, **kwargs) -> None:
        from SPPCompiler.SemanticAnalysis.ASTs import (
            FunctionParameterSelfAst, FunctionParameterRequiredAst, FunctionParameterOptionalAst,
            FunctionParameterVariadicAst)

        # Check there are no duplicate parameter names for this function, and raise an exception if there are.
        if Seq(self.parameters).map(lambda p: p.identifier_for_param()).contains_duplicates():
            duplicate_parameters = Seq(self.parameters).map(lambda p: p.identifier).non_unique_items()[0]
            raise SemanticErrors.DUPLICATE_ITEM(duplicate_parameters, "attribute")

        # Ensure the ordering of parameters in this group is correct (Self => Required => Optional => Variadic).
        classification_ordering = {
            FunctionParameterSelfAst: "Self", FunctionParameterRequiredAst: "Required",
            FunctionParameterOptionalAst: "Optional", FunctionParameterVariadicAst: "Variadic"}

        current_classifications = Seq(self.parameters).map(type).zip(Seq(self.parameters))
        sorted_classifications  = current_classifications.sort(key=lambda c: list(classification_ordering.keys()).index(c[0]))
        if current_classifications != sorted_classifications:
            difference = sorted_classifications.ordered_difference(current_classifications)
            raise SemanticErrors.INVALID_ORDER(difference.list(), classification_ordering, "parameter")

        # Ensure there is a maximum of 1 variadic parameter.
        variadic_parameters = Seq(self.parameters).filter_to_type(FunctionParameterVariadicAst)
        if variadic_parameters.length > 1:
            raise SemanticErrors.MULTIPLE_VARIADIC_PARAMETERS(variadic_parameters[0].variable, variadic_parameters[1].variable)

        # Analyse each parameter.
        Seq(self.parameters).for_each(lambda p: p.do_semantic_analysis(scope_handler, **kwargs))

    def get_self(self) -> Optional["FunctionParameterSelfAst"]:
        # Get the "self" function parameter (if it exists).
        from SPPCompiler.SemanticAnalysis.ASTs import FunctionParameterSelfAst
        return Seq(self.parameters).filter_to_type(FunctionParameterSelfAst).first(None)

    def get_req(self) -> Seq["FunctionParameterRequiredAst"]:
        # Get all the required function parameters.
        from SPPCompiler.SemanticAnalysis.ASTs import FunctionParameterRequiredAst
        return Seq(self.parameters).filter_to_type(FunctionParameterRequiredAst)

    def get_opt(self) -> Seq["FunctionParameterOptionalAst"]:
        # Get all the optional function parameters.
        from SPPCompiler.SemanticAnalysis.ASTs import FunctionParameterOptionalAst
        return Seq(self.parameters).filter_to_type(FunctionParameterOptionalAst)

    def get_var(self) -> Optional["FunctionParameterVariadicAst"]:
        # Get the variadic function parameter (if it exists).
        from SPPCompiler.SemanticAnalysis.ASTs import FunctionParameterVariadicAst
        return Seq(self.parameters).filter_to_type(FunctionParameterVariadicAst).first(None)

    def get_non_self(self) -> Seq["FunctionParameterAst"]:
        # Get all the function parameters that are not "self".
        from SPPCompiler.SemanticAnalysis.ASTs import FunctionParameterSelfAst
        return Seq(self.parameters).filter_not_type(FunctionParameterSelfAst)

    def __copy__(self):
        return FunctionParameterGroupAst(self.pos, self.paren_l_token, self.parameters.copy(), self.paren_r_token)


__all__ = ["FunctionParameterGroupAst"]
