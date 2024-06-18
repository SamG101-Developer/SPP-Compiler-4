from dataclasses import dataclass
from typing import List, Optional

from SPPCompiler.SemanticAnalysis.ASTs.Meta.Ast import Ast
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstPrinter import *

from SPPCompiler.Utils.Sequence import Seq


@dataclass
class LambdaPrototypeAst(Ast):
    """
    The LambdaPrototypeAst node represents a lambda function. It is the same as a regular FunctionPrototype except it
    doesn't have an identifier, and it has a lambda capture block.

    Attributes:
        - fun_token: The 'fun' keyword token.
        - generic_parameters: The generic parameters of the lambda function.
        - parameters: The parameters of the lambda function.
        - arrow_token: The '->' token.
        - return_type: The return type of the lambda function.
        - where_block: The where block of the lambda function.
        - lambda_capture_block: The lambda capture block of the lambda function.
        - body: The body of the lambda function.
    """

    fun_token: "TokenAst"
    generic_parameters: "GenericParameterGroupAst"
    parameters: "FunctionParameterGroupAst"
    arrow_token: "TokenAst"
    return_type: "TypeAst"
    where_block: Optional["WhereBlockAst"]
    lambda_capture_block: Optional["LambdaCaptureBlockAst"]
    body: "InnerScopeAst[StatementAst]"

    def __post_init__(self):
        raise NotImplementedError("LambdaPrototypeAst is not implemented yet.")

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        # Print the lambda function.
        s = ""
        s += f"{self.fun_token.print(printer)}"
        s += f"{self.generic_parameters.print(printer)}"
        s += f"{self.parameters.print(printer)} "
        s += f"{self.arrow_token.print(printer)} "
        s += f"{self.return_type.print(printer)}"
        s += f" {self.where_block.print(printer)}" if self.where_block else ""
        s += f"{self.lambda_capture_block.print(printer)}" if self.lambda_capture_block else ""
        s += f"{self.body.print(printer)}"
        return s


__all__ = ["LambdaPrototypeAst"]
