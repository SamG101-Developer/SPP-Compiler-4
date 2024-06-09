from dataclasses import dataclass
from typing import List, Optional

from SPPCompiler.SemanticAnalysis.ASTMixins.SemanticAnalyser import SemanticAnalyser
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
        - paren_l_token: The left parenthesis token.
        - parameters: The parameters of the function prototype.
        - paren_r_token: The right parenthesis token.
    """

    paren_l_token: "TokenAst"
    parameters: List["FunctionParameterAst"]
    paren_r_token: "TokenAst"

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
        # if Seq(self.parameters).map(lambda p: p.identifier).contains_duplicates():
        #     duplicate_parameters = Seq(self.parameters).map(lambda p: p.identifier).non_unique_items()[0]
        #     raise SemanticErrors.DUPLICATE_ITEM(duplicate_parameters, "attribute")

        # Ensure the ordering of parameters in this group is correct (Self => Required => Optional => Variadic).
        classification_ordering = {
            FunctionParameterSelfAst: "Self", FunctionParameterRequiredAst: "Required",
            FunctionParameterOptionalAst: "Optional", FunctionParameterVariadicAst: "Variadic"}

        current_classifications = Seq(self.parameters).map(type).zip(Seq(self.parameters))
        sorted_classifications  = current_classifications.sort(key=lambda c: list(classification_ordering.keys()).index(c[0]))
        if current_classifications != sorted_classifications:
            difference = sorted_classifications.ordered_difference(current_classifications)
            raise SemanticErrors.INVALID_ORDER(difference.value, classification_ordering, "parameter")

        # Ensure that this function is a class method if a "self" parameter is present.
        if self.get_self() and scope_handler.at_global_scope(parent_level=2):
            raise SemanticErrors.SELF_PARAMETER_OUTSIDE_CLASS(self.get_self().identifier)

        # Ensure there is a maximum of 1 variadic parameter.
        variadic_parameters = Seq(self.parameters).filter_to_type(FunctionParameterVariadicAst)
        if variadic_parameters.length > 1:
            raise SemanticErrors.MULTIPLE_VARIADIC_PARAMETERS(variadic_parameters[0].variable, variadic_parameters[1].variable)

        # Analyse each parameter.
        Seq(self.parameters).for_each(lambda p: p.do_semantic_analysis(scope_handler, **kwargs))

    def get_self(self) -> Optional["FunctionParameterSelfAst"]:
        # Get the "self" function parameter (if it exists).
        from SPPCompiler.SemanticAnalysis.ASTs import FunctionParameterSelfAst
        return Seq(self.parameters).filter(lambda p: isinstance(p, FunctionParameterSelfAst)).first(None)

    def get_req(self) -> List["FunctionParameterRequiredAst"]:
        # Get all the required function parameters.
        from SPPCompiler.SemanticAnalysis.ASTs import FunctionParameterRequiredAst
        return Seq(self.parameters).filter(lambda p: isinstance(p, FunctionParameterRequiredAst)).value

    def get_opt(self) -> List["FunctionParameterOptionalAst"]:
        # Get all the optional function parameters.
        from SPPCompiler.SemanticAnalysis.ASTs import FunctionParameterOptionalAst
        return Seq(self.parameters).filter(lambda p: isinstance(p, FunctionParameterOptionalAst)).value

    def get_var(self) -> Optional["FunctionParameterVariadicAst"]:
        # Get the variadic function parameter (if it exists).
        from SPPCompiler.SemanticAnalysis.ASTs import FunctionParameterVariadicAst
        return Seq(self.parameters).filter(lambda p: isinstance(p, FunctionParameterVariadicAst)).first(None)


__all__ = ["FunctionParameterGroupAst"]
