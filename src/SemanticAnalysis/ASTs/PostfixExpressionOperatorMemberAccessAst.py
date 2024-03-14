from dataclasses import dataclass
from typing import Tuple, Type

from src.SemanticAnalysis.Analysis.SemanticAnalysis import SemanticAnalysis
from src.SemanticAnalysis.Analysis.SemanticError import SemanticError
from src.SemanticAnalysis.Symbols.Scopes import ScopeHandler
from src.SemanticAnalysis.Types.CommonTypes import CommonTypes

from src.SemanticAnalysis.ASTs.Meta.Ast import Ast
from src.SemanticAnalysis.ASTs.Meta.AstPrinter import *

from src.SemanticAnalysis.ASTs.ConventionMovAst import ConventionMovAst
from src.SemanticAnalysis.ASTs.IdentifierAst import IdentifierAst
from src.SemanticAnalysis.ASTs.TokenAst import TokenAst


@dataclass
class PostfixExpressionOperatorMemberAccessAst(Ast, SemanticAnalysis):
    """
    The PostfixExpressionOperatorMemberAccessAst node represents the member access operator of some expression.

    Attributes:
        - dot_token: The dot token that represents the member access operator.
        - identifier: The identifier or numeric token that represents the member being accessed.
    """

    dot_token: "TokenAst"
    identifier: "PostfixMemberPartAst"

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        # Print the member access operator.
        s = ""
        s += f"{self.dot_token.print(printer)}"
        s += f"{self.identifier.print(printer)}"
        return s

    def do_semantic_analysis(self, scope_handler: ScopeHandler, lhs: "ExpressionAst" = None, **kwargs) -> None:
        # For numeric access, check the LHS is a tuple type with enough elements in it.
        if isinstance(self.identifier, TokenAst):
            if lhs.infer_type(scope_handler, **kwargs)[1].without_generics() != CommonTypes.tuple([]):
                exception = SemanticError(f"Numeric member access requires a tuple type:")
                exception.add_traceback(lhs.pos, f"Type '{lhs.infer_type(scope_handler, **kwargs)}' found here.")
                exception.add_traceback(self.identifier.pos, f"Numeric member access found here.")
                raise exception

            if int(self.identifier.token.token_metadata) >= len(lhs.infer_type(scope_handler, **kwargs)[1].parts[-1].generic_arguments.arguments):
                lhs_type = lhs.infer_type(scope_handler, **kwargs)
                exception = SemanticError(f"Numeric member access out of bounds:")
                exception.add_traceback(lhs.pos, f"Type '{lhs_type[0]}{lhs_type[1]}' found here, with {len(lhs.infer_type(scope_handler, **kwargs)[1].parts[-1].generic_arguments.arguments)} elements.")
                exception.add_traceback(self.identifier.pos, f"Numeric member access found here to element {self.identifier.token.token_metadata}.")
                raise exception

        # For attribute access, check the attribute exists on the type being accessed.
        elif isinstance(self.identifier, IdentifierAst):
            lhs_type_scope = scope_handler.current_scope.get_symbol(lhs.infer_type(scope_handler, **kwargs)[1]).associated_scope
            if not lhs_type_scope.has_symbol(self.identifier):
                lhs_type = lhs.infer_type(scope_handler, **kwargs)
                exception = SemanticError(f"Undefined attribute '{self.identifier.value}' on type '{lhs_type[1]}':")
                exception.add_traceback(lhs.pos, f"Type '{lhs_type[0]}{lhs_type[1]}' inferred here.")
                exception.add_traceback(self.identifier.pos, f"Attribute '{self.identifier.value}' accessed here.")
                raise exception

        else:
            raise NotImplementedError

    def infer_type(self, scope_handler: ScopeHandler, lhs: "ExpressionAst" = None, **kwargs) -> Tuple[Type["ConventionAst"], "TypeAst"]:
        # The identifier access needs to get the type of the left side, then inspect the correct attribute for the
        # correct type
        if isinstance(self.identifier, IdentifierAst):
            lhs_type_scope = scope_handler.current_scope.get_symbol(lhs.infer_type(scope_handler, **kwargs)[1]).associated_scope
            return ConventionMovAst, lhs_type_scope.get_symbol(self.identifier).type

        # The numeric access needs to get the generic arguments of the left side (tuple), then get the type of the
        # correct element.
        elif isinstance(self.identifier, TokenAst):
            lhs_type = lhs.infer_type(scope_handler, **kwargs)
            return ConventionMovAst, lhs_type[1].parts[-1].generic_arguments.arguments[int(self.identifier.token.token_metadata)].type

    def __eq__(self, other):
        # Check both ASTs are the same type and have the same identifier.
        return isinstance(other, PostfixExpressionOperatorMemberAccessAst) and self.identifier == other.identifier
