from dataclasses import dataclass
from typing import Optional

from src.SemanticAnalysis.Analysis.SemanticAnalysis import SemanticAnalysis
from src.SemanticAnalysis.Analysis.SemanticError import SemanticError
from src.SemanticAnalysis.Symbols.Symbols import VariableSymbol, MemoryStatus

from src.SemanticAnalysis.ASTs.Meta.Ast import Ast
from src.SemanticAnalysis.ASTs.Meta.AstPrinter import *

from src.SemanticAnalysis.ASTs.ConventionRefAst import ConventionRefAst
from src.SemanticAnalysis.ASTs.ConventionMutAst import ConventionMutAst
from src.SemanticAnalysis.ASTs.ConventionMovAst import ConventionMovAst


@dataclass
class FunctionParameterOptionalAst(Ast, SemanticAnalysis):
    """
    The FunctionParameterOptionalAst node represents an optional parameter of a function. This is a parameter that can
    be given an argument when calling the function. The parameter cannot have a convention (ie must be mov/copy). The type
    declaration is still required despite a default value being provided; this is a design decision.

    Attributes:
        - is_mutable: The token that represents the mutability of the parameter.
        - identifier: The identifier of the parameter.
        - colon_token: The colon token.
        - convention: The convention of the parameter.
        - type_declaration: The type declaration of the parameter.
        - assignment_token: The assignment token.
        - default_value: The default value of the parameter.
    """

    is_mutable: Optional["TokenAst"]
    identifier: "IdentifierAst"
    colon_token: "TokenAst"
    convention: "ConventionAst"
    type_declaration: "TypeAst"
    assignment_token: "TokenAst"
    default_value: "ExpressionAst"

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        # Print the FunctionParameterOptionalAst.
        s = ""
        s += f"{self.is_mutable.print(printer)}" if self.is_mutable else ""
        s += f"{self.identifier.print(printer)}"
        s += f"{self.colon_token.print(printer)} "
        s += f"{self.convention.print(printer)}"
        s += f"{self.type_declaration.print(printer)}"
        s += f"{self.assignment_token.print(printer)}"
        s += f"{self.default_value.print(printer)}"
        return s

    def do_semantic_analysis(self, scope_handler, **kwargs) -> None:
        # Analyse the parameter type.
        self.type_declaration.do_semantic_analysis(scope_handler, **kwargs)

        # Add the symbol for the parameter. The memory status depends on the calling-convention used.
        symbol = VariableSymbol(self.identifier, self.type_declaration, is_mutable=self.is_mutable is not None, memory_info=MemoryStatus(
            ast_initialized=self.identifier,
            is_borrow_ref=isinstance(self.convention, ConventionRefAst),
            is_borrow_mut=isinstance(self.convention, ConventionMutAst),
            ast_borrow=self.convention))
        scope_handler.current_scope.add_symbol(symbol)

        # Check the convention of the parameter is by-mov, because default values will always be "owned", so the type
        # must use the "move" convention.
        if not isinstance(self.convention, ConventionMovAst):
            exception = SemanticError(f"Optional parameters must use the by-val convention:")
            exception.add_traceback(self.convention.pos, f"Convention '{self.convention}' used here.")
            raise exception

        # Analyse the default value to ensure it is valid. This happens before the type-check against the parameter#
        # type, as otherwise there would be an error in detecting the type of an invalid expression.
        self.default_value.do_semantic_analysis(scope_handler, **kwargs)

        # Check the default value's type matches the parameter's type.
        if not self.type_declaration.symbolic_eq((default_value_type := self.default_value.infer_type(scope_handler, **kwargs))[1], scope_handler.current_scope):
            exception = SemanticError(f"Optional parameter type does not match default value type:")
            exception.add_traceback(self.type_declaration.pos, f"Parameter type '{self.type_declaration}' declared here.")
            exception.add_traceback(self.default_value.pos, f"Default value type '{default_value_type[1]}' inferred here.")
            raise exception

    def __eq__(self, other):
        # Check both ASTs are the same type and have the same identifier.
        return isinstance(other, FunctionParameterOptionalAst) and self.identifier == other.identifier


__all__ = ["FunctionParameterOptionalAst"]
