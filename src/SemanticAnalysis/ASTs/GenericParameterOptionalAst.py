from dataclasses import dataclass, field
from typing import Optional

from src.SemanticAnalysis.ASTMixins.SemanticAnalyser import SemanticAnalyser
from src.SemanticAnalysis.Utils.Scopes import ScopeHandler

from src.SemanticAnalysis.ASTs.Meta.Ast import Ast
from src.SemanticAnalysis.ASTs.Meta.AstPrinter import *

from src.SemanticAnalysis.ASTs.IdentifierAst import IdentifierAst
from src.SemanticAnalysis.ASTs.GenericIdentifierAst import GenericIdentifierAst
from src.SemanticAnalysis.ASTs.GenericParameterInlineConstraintAst import GenericParameterInlineConstraintAst
from src.SemanticAnalysis.ASTs.TypeAst import TypeAst
from src.SemanticAnalysis.ASTs.TokenAst import TokenAst
from src.SemanticAnalysis.ASTs.TypeSingleAst import TypeSingleAst


@dataclass
class GenericParameterOptionalAst(Ast, SemanticAnalyser):
    """
    The GenericParameterOptionalAst node represents an optional generic parameter of a function/class/superimposition
    prototype. This is a parameter that can be given a generic argument when calling the function or instantiating the
    object.

    Attributes:
        - raw_identifier: The raw identifier of the generic parameter.
        - inline_constraints: The inline constraints of the generic parameter.
        - assignment_token: The assignment token.
        - default_value: The default value of the generic parameter.
        - identifier: The converted identifier of the generic parameter.
    """

    raw_identifier: IdentifierAst
    inline_constraints: Optional[GenericParameterInlineConstraintAst]
    assignment_token: TokenAst
    default_value: TypeAst
    identifier: TypeAst = field(default=None, init=False)

    def __post_init__(self):
        self.identifier = TypeSingleAst(self.raw_identifier.pos, [GenericIdentifierAst(self.raw_identifier.pos, self.raw_identifier.value, None)])

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        # Print the GenericParameterOptionalAst.
        s = ""
        s += f"{self.identifier.print(printer)}"
        s += f"{self.inline_constraints.print(printer)} " if self.inline_constraints else " "
        s += f"{self.assignment_token.print(printer)} "
        s += f"{self.default_value.print(printer)}"
        return s

    def do_semantic_analysis(self, scope_handler: ScopeHandler, **kwargs) -> None:
        # Because this is a type-oriented AST, the generating stage of function/class/superimposition blocks will have
        # registered these generic types into the scope.
        ...
