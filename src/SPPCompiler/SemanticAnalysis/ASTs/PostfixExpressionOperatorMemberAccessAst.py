from dataclasses import dataclass

from SPPCompiler.LexicalAnalysis.Tokens import TokenType
from SPPCompiler.SemanticAnalysis.ASTMixins.SemanticAnalyser import SemanticAnalyser
from SPPCompiler.SemanticAnalysis.ASTMixins.TypeInfer import TypeInfer, InferredType
from SPPCompiler.SemanticAnalysis.ASTs.Meta.Ast import Ast
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstPrinter import *
from SPPCompiler.SemanticAnalysis.Utils.CommonTypes import CommonTypes
from SPPCompiler.SemanticAnalysis.Utils.Scopes import ScopeHandler
from SPPCompiler.SemanticAnalysis.Utils.SemanticError import SemanticErrors


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
        from SPPCompiler.SemanticAnalysis.ASTs import TokenAst, IdentifierAst, PostfixExpressionAst
        lhs_type = lhs.infer_type(scope_handler, **kwargs).type

        # Numeric member access.
        if isinstance(self.identifier, TokenAst):
            # Check if the left side is a tuple; this is the only type supported for index access.
            if not lhs_type.without_generics().symbolic_eq(CommonTypes.tuple([]), scope_handler.current_scope):
                raise SemanticErrors.NUMERICAL_MEMBER_ACCESS_TYPE(lhs, self.identifier, lhs_type)

            # Check if the index is within bounds, i.e. it is less than the number of elements in the tuple.
            if int(self.identifier.token.token_metadata) >= len(lhs_type.parts[-1].generic_arguments.arguments):
                raise SemanticErrors.NUMERICAL_MEMBER_ACCESS_OUT_OF_BOUNDS(lhs, self.identifier, lhs_type)

        # Identifier member access.
        if isinstance(self.identifier, IdentifierAst) and self.dot_token.token.token_type == TokenType.TkDot:
            lhs_type_scope = scope_handler.current_scope.get_symbol(lhs_type).associated_scope

            # Check if the left side is a generic type.
            if not lhs_type_scope:
                raise SemanticErrors.MEMBER_ACCESS_GENERIC_TYPE(lhs, self.identifier, lhs_type)  # todo: allow with constraints / intersection types

            # Check if the member being accessed exists on the left side type.
            if not lhs_type_scope.has_symbol(self.identifier):
                raise SemanticErrors.MEMBER_ACCESS_NON_EXISTENT(lhs, self.identifier, lhs_type, "type", "attribute")

        # Namespaced member access.
        if isinstance(self.identifier, IdentifierAst) and self.dot_token.token.token_type == TokenType.TkDblColon:
            # Collect the namespace parts. a.b.c.d = ((a.b).c).b => keep collecting until the last part is found.
            namespace = [lhs]
            while isinstance(lhs, PostfixExpressionAst) and isinstance(lhs.op, PostfixExpressionOperatorMemberAccessAst):
                namespace.append(lhs.lhs)
                lhs = lhs.lhs

            # Check the member exists in the namespace.
            namespace_scope = scope_handler.get_namespaced_scope(namespace)
            if not namespace_scope:
                raise SemanticErrors.UNKNOWN_IDENTIFIER(namespace[-1], [], "namespace")

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
            return InferredType(convention=ConventionMovAst, type=lhs_type.type.parts[-1].generic_arguments.arguments[int(self.identifier.token.token_metadata)].type)

    def __eq__(self, other):
        # Check both ASTs are the same type and have the same identifier.
        return isinstance(other, PostfixExpressionOperatorMemberAccessAst) and self.identifier == other.identifier
