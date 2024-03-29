from dataclasses import dataclass
from typing import List, Optional

from src.SemanticAnalysis.ASTMixins.SemanticAnalyser import SemanticAnalyser
from src.SemanticAnalysis.Utils.SemanticError import SemanticError
from src.SemanticAnalysis.Utils.Scopes import ScopeHandler

from src.SemanticAnalysis.ASTs.Meta.Ast import Ast
from src.SemanticAnalysis.ASTs.Meta.AstPrinter import *

from src.SemanticAnalysis.ASTs.FunctionParameterAst import FunctionParameterAst
from src.SemanticAnalysis.ASTs.FunctionParameterSelfAst import FunctionParameterSelfAst
from src.SemanticAnalysis.ASTs.FunctionParameterRequiredAst import FunctionParameterRequiredAst
from src.SemanticAnalysis.ASTs.FunctionParameterOptionalAst import FunctionParameterOptionalAst
from src.SemanticAnalysis.ASTs.FunctionParameterVariadicAst import FunctionParameterVariadicAst
from src.SemanticAnalysis.ASTs.TokenAst import TokenAst

from src.Utils.Sequence import Seq


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

    paren_l_token: TokenAst
    parameters: List[FunctionParameterAst]
    paren_r_token: TokenAst

    def print(self, printer: AstPrinter) -> str:
        # Print the FunctionParameterGroupAst.
        s = ""
        s += f"{self.paren_l_token.print(printer)}"
        s += Seq(self.parameters).print(printer, ", ")
        s += f"{self.paren_r_token.print(printer)}"
        return s

    def do_semantic_analysis(self, scope_handler: ScopeHandler, **kwargs) -> None:
        # Check no parameters have the same name.
        if Seq(self.parameters).map(lambda p: p.identifier).contains_duplicates():
            duplicate_parameters = Seq(self.parameters).map(lambda p: p.identifier).non_unique_items()[0]
            exception = SemanticError(f"Duplicate parameters '{duplicate_parameters[0]}' found in function prototype:")
            exception.add_traceback(duplicate_parameters[0].pos, f"Parameter '{duplicate_parameters[0]}' declared here.")
            exception.add_traceback(duplicate_parameters[1].pos, f"Parameter '{duplicate_parameters[1]}' re-declared here.")
            raise exception

        # Check parameter order is Self -> Required -> Optional -> Variadic.
        ordering = {FunctionParameterSelfAst: "Self", FunctionParameterRequiredAst: "Required", FunctionParameterOptionalAst: "Optional", FunctionParameterVariadicAst: "Variadic"}
        current_classifications = Seq(self.parameters).map(lambda p: (type(p), p))
        sorted_classifications = current_classifications.sort(key=lambda t: list(ordering.keys()).index(t[0]))
        if current_classifications != sorted_classifications:
            difference = sorted_classifications.ordered_difference(current_classifications)
            exception = SemanticError(f"Invalid parameter order in function prototype:")
            exception.add_traceback(difference[-2][1].pos, f"{ordering[difference[-2][0]]} parameter '{difference[-2][1]}' declared here.")
            exception.add_traceback(difference[-1][1].pos, f"{ordering[difference[-1][0]]} parameter '{difference[-1][1]}' declared here.")
            raise exception

        # Check that the function is class method, not in the module global space, if there is a "self" parameter.
        if self.parameters and isinstance(self.parameters[0], FunctionParameterSelfAst) and scope_handler.at_global_scope(parent_level=2):
            exception = SemanticError(f"Can only use the 'self' parameter within a class:")
            exception.add_traceback(self.parameters[0].pos, f"Parameter '{self.parameters[0]}' declared here.")
            raise exception

        # Check that there is a maximum of 1 variadic parameter.
        variadic_parameters = Seq(self.parameters).filter(lambda p: isinstance(p, FunctionParameterVariadicAst))
        if variadic_parameters.length > 1:
            exception = SemanticError(f"Invalid parameter order in function prototype:")
            exception.add_traceback(variadic_parameters[0].pos, f"1st variadic parameter '{variadic_parameters[0]}' declared here.")
            exception.add_traceback(variadic_parameters[1].pos, f"2nd variadic parameter '{variadic_parameters[1]}' declared here.")
            raise exception

        Seq(self.parameters).for_each(lambda p: p.do_semantic_analysis(scope_handler, **kwargs))

    def get_self(self) -> Optional[FunctionParameterSelfAst]:
        # Get the "self" function parameter (if it exists).
        return Seq(self.parameters).filter(lambda p: isinstance(p, FunctionParameterSelfAst)).first(None)

    def get_req(self) -> List[FunctionParameterRequiredAst]:
        # Get all the required function parameters.
        return Seq(self.parameters).filter(lambda p: isinstance(p, FunctionParameterRequiredAst)).value

    def get_opt(self) -> List[FunctionParameterOptionalAst]:
        # Get all the optional function parameters.
        return Seq(self.parameters).filter(lambda p: isinstance(p, FunctionParameterOptionalAst)).value

    def get_var(self) -> Optional[FunctionParameterVariadicAst]:
        # Get the variadic function parameter (if it exists).
        return Seq(self.parameters).filter(lambda p: isinstance(p, FunctionParameterVariadicAst)).first(None)


__all__ = ['FunctionParameterGroupAst']
