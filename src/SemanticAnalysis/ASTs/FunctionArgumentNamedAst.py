from __future__ import annotations
from dataclasses import dataclass
from typing import Tuple, Type

from src.SemanticAnalysis.Analysis.SemanticAnalysis import SemanticAnalysis
from src.SemanticAnalysis.Symbols.Scopes import ScopeHandler
from src.SemanticAnalysis.Types.TypeInfer import TypeInfer

from src.SemanticAnalysis.ASTs.Meta.Ast import Ast
from src.SemanticAnalysis.ASTs.Meta.AstPrinter import *

from src.SemanticAnalysis.ASTs.ConventionMovAst import ConventionMovAst


@dataclass
class FunctionArgumentNamedAst(Ast, SemanticAnalysis, TypeInfer):
    """
    The GenericArgumentNamedAst node represents a named generic argument being given to a generic parameter of a
    function/class/superimposition block. This looks like `function_call[T=Str](123)`, where `T=Str` is the generic
    argument being given to the function. The argument has an identifier, assignment token, and the type.

    Attributes:
        - identifier: The identifier of the argument.
        - assignment_token: The token that represents the assignment of the argument.
        - convention: The convention of the argument.
        - value: The value of the argument.
    """

    identifier: "IdentifierAst"
    assignment_token: "TokenAst"
    convention: "ConventionAst"
    value: "ExpressionAst"

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        # Print the FunctionArgumentNamedAst.
        s = ""
        s += f"{self.identifier.print(printer)}"
        s += f"{self.assignment_token.print(printer)}"
        s += f"{self.convention.print(printer)}"
        s += f"{self.value.print(printer)}"
        return s

    def do_semantic_analysis(self, scope_handler: ScopeHandler, **kwargs) -> None:
        # Pass analysis onto the value of the argument.
        self.value.do_semantic_analysis(scope_handler, **kwargs)

    def infer_type(self, scope_handler: ScopeHandler, **kwargs) -> Tuple[Type[ConventionAst], TypeAst]:
        # The convention of an argument is either the given convention, or the convention of the value.
        match self.convention, self.value.infer_type(scope_handler, **kwargs)[0]:
            case ConventionMovAst(), that_convention: convention = that_convention
            case self_convention, _: convention = type(self_convention)
        return convention, self.value.infer_type(scope_handler, **kwargs)[1]
