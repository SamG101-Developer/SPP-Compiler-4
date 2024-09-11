import operator
from dataclasses import dataclass

from SPPCompiler.LexicalAnalysis.Tokens import TokenType
from SPPCompiler.SemanticAnalysis.ASTs.Meta.Ast import Ast
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstMixins import SemanticAnalyser
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstPrinter import *
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstUtils import TypeInfer, InferredType
from SPPCompiler.SemanticAnalysis.Utils.CommonTypes import CommonTypes
from SPPCompiler.SemanticAnalysis.Utils.Scopes import ScopeHandler
from SPPCompiler.SemanticAnalysis.Utils.SemanticError import SemanticErrors
from SPPCompiler.SemanticAnalysis.Utils.Symbols import NamespaceSymbol, VariableSymbol


@dataclass
class PostfixExpressionOperatorMemberAccessAst(Ast, SemanticAnalyser, TypeInfer):
    """
    The PostfixExpressionOperatorMemberAccessAst node represents the member access operator of some expression.

    Attributes:
        dot_token: The dot token that represents the member access operator.
        identifier: The identifier or numeric token that represents the member being accessed.
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
        from SPPCompiler.SemanticAnalysis.ASTs import TokenAst, IdentifierAst, PostfixExpressionAst, TypeAst

        match lhs:
            case TypeAst():
                # Ensure static member access is being used on the type.
                if self.dot_token.token.token_type != TokenType.TkDblColon:
                    raise SemanticErrors.STATIC_MEMBER_TYPE_ACCESS(lhs, self.dot_token, "type")

                # Ensure the symbol exists on the type.
                type_scope = scope_handler.current_scope.get_symbol(lhs).associated_scope
                if not type_scope.has_symbol(self.identifier):
                    raise SemanticErrors.MEMBER_ACCESS_NON_EXISTENT(lhs, self.identifier, lhs.infer_type(scope_handler), "type", "attribute")

                # Ensure the method is static (no "self" parameter). todo
                return

            case _:
                lhs_type = lhs.infer_type(scope_handler, **kwargs).type

                # Numeric member access.
                if isinstance(self.identifier, TokenAst):
                    # Check if the left side is a tuple; this is the only type supported for index access.
                    if not lhs_type.without_generics().symbolic_eq(CommonTypes.tuple([]), scope_handler.current_scope):
                        raise SemanticErrors.NUMERICAL_MEMBER_ACCESS_TYPE(lhs, self.identifier, lhs_type)

                    # Check if the index is within bounds, i.e. it is less than the number of elements in the tuple.
                    if int(self.identifier.token.token_metadata) >= len(lhs_type.types[-1].generic_arguments.arguments):
                        raise SemanticErrors.NUMERICAL_MEMBER_ACCESS_OUT_OF_BOUNDS(lhs, self.identifier, lhs_type)

                # Identifier member access.
                if isinstance(self.identifier, IdentifierAst) and self.dot_token.token.token_type == TokenType.TkDot:
                    lhs_symbol = scope_handler.current_scope.get_symbol(lhs_type)
                    lhs_type_scope = lhs_symbol.associated_scope

                    # Check if the left side is a generic type; cannot access members off these.
                    if not lhs_type_scope:
                        raise SemanticErrors.MEMBER_ACCESS_GENERIC_TYPE(lhs, self.identifier, lhs_type)

                    # Check the identifier is a variable and not a namespace.
                    if isinstance(lhs_symbol, NamespaceSymbol):
                        raise SemanticErrors.STATIC_MEMBER_TYPE_ACCESS(lhs, self.dot_token, "namespace")

                    # Check if the member being accessed exists on the left side type.
                    if not lhs_type_scope.has_symbol(self.identifier, exclusive=True):
                        raise SemanticErrors.MEMBER_ACCESS_NON_EXISTENT(lhs, self.identifier, lhs_type, "type", "attribute")

                    # Check for ambiguous symbol access (unless it is a function being called).
                    # Todo: this is a bit hacky to avoid function calls which are allows (overloads resolved).
                    if not lhs_type_scope.get_symbol(self.identifier, exclusive=True).type.types[-1].value.startswith("MOCK"):
                        all_symbols = lhs_type_scope.get_multiple_symbols(self.identifier)
                        min_depth = all_symbols.map(operator.itemgetter(2)).min()
                        syms_at_min_depth = all_symbols.filter(lambda s: s[2] == min_depth)
                        if syms_at_min_depth.length > 1:
                            raise SemanticErrors.AMBIGUOUS_ATTRIBUTE_ACCESS(lhs, self.identifier, syms_at_min_depth)

                # Namespaced member access.
                if isinstance(self.identifier, IdentifierAst) and self.dot_token.token.token_type == TokenType.TkDblColon:
                    # Collect the namespace parts. (Keep collecting until the last part is found).
                    namespace = [lhs]
                    while isinstance(lhs, PostfixExpressionAst) and isinstance(lhs.op, PostfixExpressionOperatorMemberAccessAst):
                        namespace.append(lhs.lhs)
                        lhs = lhs.lhs

                    lhs_symbol = scope_handler.current_scope.get_symbol(lhs)
                    namespace_scope = scope_handler.get_namespaced_scope(namespace)

                    # Check the identifier is a namespace and not a variable.
                    if isinstance(lhs_symbol, VariableSymbol):
                        raise SemanticErrors.RUNTIME_MEMBER_TYPE_ACCESS(lhs, self.dot_token, "variable")

                    # Check if the member being accessed exists on the namespace.
                    if not namespace_scope.has_symbol(self.identifier):
                        raise SemanticErrors.MEMBER_ACCESS_NON_EXISTENT(lhs, self.identifier, lhs_type, "namespace", "member")

    def infer_type(self, scope_handler: ScopeHandler, lhs: "ExpressionAst" = None, **kwargs) -> InferredType:
        from SPPCompiler.SemanticAnalysis.ASTs import ConventionMovAst, IdentifierAst, TokenAst

        # The identifier access needs to get the type of the left side, then inspect the correct attribute for the
        # correct type
        if isinstance(self.identifier, IdentifierAst):
            lhs_type_scope = scope_handler.current_scope.get_symbol(lhs.infer_type(scope_handler, **kwargs).type).associated_scope
            return InferredType(convention=ConventionMovAst, type=lhs_type_scope.get_symbol(self.identifier).type)

        # The numeric access needs to get the generic arguments of the left side (tuple), then get the type of the
        # correct element.
        elif isinstance(self.identifier, TokenAst):
            lhs_type = lhs.infer_type(scope_handler, **kwargs)
            return InferredType(convention=ConventionMovAst, type=lhs_type.type.types[-1].generic_arguments.arguments[int(self.identifier.token.token_metadata)].type)

    def __eq__(self, other):
        # Check both ASTs are the same type and have the same identifier.
        return isinstance(other, PostfixExpressionOperatorMemberAccessAst) and self.identifier == other.identifier


__all__ = ["PostfixExpressionOperatorMemberAccessAst"]
