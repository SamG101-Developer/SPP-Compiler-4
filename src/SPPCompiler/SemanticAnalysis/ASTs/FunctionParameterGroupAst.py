from dataclasses import dataclass
from typing import List, Optional

from SPPCompiler.SemanticAnalysis.ASTMixins.SemanticAnalyser import SemanticAnalyser
from SPPCompiler.SemanticAnalysis.Utils.SemanticError import SemanticError, SemanticErrorType
from SPPCompiler.SemanticAnalysis.Utils.Scopes import ScopeHandler

from SPPCompiler.SemanticAnalysis.ASTs.Meta.Ast import Ast
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstPrinter import *

from SPPCompiler.SemanticAnalysis.ASTs.FunctionParameterAst import FunctionParameterAst
from SPPCompiler.SemanticAnalysis.ASTs.FunctionParameterSelfAst import FunctionParameterSelfAst
from SPPCompiler.SemanticAnalysis.ASTs.FunctionParameterRequiredAst import FunctionParameterRequiredAst
from SPPCompiler.SemanticAnalysis.ASTs.FunctionParameterOptionalAst import FunctionParameterOptionalAst
from SPPCompiler.SemanticAnalysis.ASTs.FunctionParameterVariadicAst import FunctionParameterVariadicAst
from SPPCompiler.SemanticAnalysis.ASTs.TokenAst import TokenAst

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
        ...

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


__all__ = ["FunctionParameterGroupAst"]
