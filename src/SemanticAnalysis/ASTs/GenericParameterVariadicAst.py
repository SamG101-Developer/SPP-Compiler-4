from dataclasses import dataclass, field
from typing import Optional

from src.SemanticAnalysis.Analysis.SemanticAnalysis import SemanticAnalysis
from src.SemanticAnalysis.Symbols.Scopes import ScopeHandler

from src.SemanticAnalysis.ASTs.Meta.Ast import Ast
from src.SemanticAnalysis.ASTs.Meta.AstPrinter import *

from src.SemanticAnalysis.ASTs.IdentifierAst import IdentifierAst
from src.SemanticAnalysis.ASTs.GenericIdentifierAst import GenericIdentifierAst
from src.SemanticAnalysis.ASTs.GenericParameterInlineConstraintAst import GenericParameterInlineConstraintAst
from src.SemanticAnalysis.ASTs.TypeAst import TypeAst
from src.SemanticAnalysis.ASTs.TokenAst import TokenAst
from src.SemanticAnalysis.ASTs.TypeSingleAst import TypeSingleAst


@dataclass
class GenericParameterVariadicAst(Ast, SemanticAnalysis):
    """
    The GenericParameterVariadicAst node represents a variadic generic parameter of a function/class/superimposition
    prototype. This is a generic parameter that can be given any number of generic arguments when calling the function
    or instantiating the object.

    Attributes:
        - variadic_token: The variadic token.
        - raw_identifier: The raw identifier of the generic parameter.
        - inline_constraints: The inline constraints of the generic parameter.
        - identifier: The converted identifier of the generic parameter.
    """

    variadic_token: TokenAst
    raw_identifier: IdentifierAst
    inline_constraints: Optional[GenericParameterInlineConstraintAst]
    identifier: TypeAst = field(default=None, init=False)

    def __post_init__(self):
        # Convert the raw identifier into a TypeSingleAst.
        self.identifier = TypeSingleAst(self.raw_identifier.pos, [GenericIdentifierAst(self.raw_identifier.pos, self.raw_identifier.value, None)])

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        # Print the GenericParameterVariadicAst.
        s = ""
        s += f"{self.variadic_token.print(printer)}"
        s += f"{self.identifier.print(printer)}"
        s += f"{self.inline_constraints.print(printer)}" if self.inline_constraints else ""
        return s

    def do_semantic_analysis(self, scope_handler: ScopeHandler, **kwargs) -> None:
        # Because this is a type-oriented AST, the generating stage of function/class/superimposition blocks will have
        # registered these generic types into the scope.
        ...
