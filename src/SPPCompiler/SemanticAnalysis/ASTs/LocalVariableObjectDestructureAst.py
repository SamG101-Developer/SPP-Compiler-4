from dataclasses import dataclass
from typing import List

from SPPCompiler.SemanticAnalysis.ASTs.Meta.Ast import Ast
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstMixins import SemanticAnalyser
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstUtils import InferredType
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstPrinter import *
from SPPCompiler.SemanticAnalysis.Utils.Scopes import ScopeHandler
from SPPCompiler.SemanticAnalysis.Utils.SemanticError import SemanticErrors
from SPPCompiler.Utils.Sequence import Seq


@dataclass
class LocalVariableObjectDestructureAst(Ast, SemanticAnalyser):
    """
    The LocalVariableDestructureAst node represents a type-destructure of local variables. This is an advanced form of a
    local variable, and is seen mostly in the "let" statement. For example, in the statement
    "let Point(mut x, y) = point", "mut x" and "y" are attributes and local variables. Both "mut x" and "y" are separate
    single local variables.

    Attributes:
        class_type: The type being destructured.
        paren_l_token: The left bracket token.
        items: The local variables in the destructure.
        paren_r_token: The right bracket token.
    """

    class_type: "TypeAst"
    paren_l_token: "TokenAst"
    items: List["LocalVariableSingleAst"]
    paren_r_token: "TokenAst"

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        s = ""
        s += f"{self.class_type.print(printer)}"
        s += f"{self.paren_l_token.print(printer)}"
        s += f"{Seq(self.items).print(printer, ", ")}"
        s += f"{self.paren_r_token.print(printer)}"
        return s

    def do_semantic_analysis(self, scope_handler: ScopeHandler, **kwargs) -> None:
        from SPPCompiler.LexicalAnalysis.Tokens import TokenType
        from SPPCompiler.SemanticAnalysis.ASTs import (
            LocalVariableSkipArgumentsAst, LocalVariableSingleIdentifierAst, LocalVariableAttributeBindingAst,
            PostfixExpressionOperatorMemberAccessAst, PostfixExpressionAst, LetStatementInitializedAst, TokenAst)

        # Analyse the value, class type, and get the attributes of the class.
        value = kwargs["value"]
        value.do_semantic_analysis(scope_handler, **kwargs)
        self.class_type.do_semantic_analysis(scope_handler, **kwargs)
        attributes = Seq(scope_handler.current_scope.get_symbol(self.class_type).type.body.members)

        # Check the RHS is the same type as the class type.
        value_type = value.infer_type(scope_handler, **kwargs)
        if not value_type.symbolic_eq(InferredType.from_type_ast(self.class_type), scope_handler.current_scope):
            raise SemanticErrors.TYPE_MISMATCH_2(None, value, InferredType.from_type_ast(self.class_type), value_type, scope_handler)

        # Only allow 1 ".." inside an object destructure.
        skips = Seq(self.items).filter_to_type(LocalVariableSkipArgumentsAst)
        if skips.length > 1:
            raise SemanticErrors.MULTIPLE_ARGUMENT_SKIPS(skips[0], skips[1])

        for current_local_variable in Seq(self.items):

            # Don't allow the binding unpacking token for a type destructure: "let p = Point(..x)" makes no sense.
            if isinstance(current_local_variable, LocalVariableSkipArgumentsAst) and current_local_variable.binding:
                raise SemanticErrors.UNPACKING_TOKEN_IN_DESTRUCTURE(current_local_variable)

            # Don't allow the ".." when there are 0 attributes (nothing to skip).
            elif isinstance(current_local_variable, LocalVariableSkipArgumentsAst) and not attributes:
                raise SemanticErrors.SKIPPING_ARGUMENTS_IN_STATELESS_TYPE(current_local_variable)

            # Skip any other analysis for occurrences of the ".." token.
            elif isinstance(current_local_variable, LocalVariableSkipArgumentsAst):
                continue

            # Check the given variable exists as an attribute on the type.
            if not attributes.map(lambda a: a.identifier).contains(current_local_variable.identifier):
                raise SemanticErrors.UNKNOWN_IDENTIFIER(current_local_variable.identifier, attributes.map(lambda a: a.identifier.value).list(), "attribute")

            # Convert the destructure into a let statement, for example "let Point(x, y, z) = point".
            if isinstance(current_local_variable, LocalVariableSingleIdentifierAst):
                ast_0 = PostfixExpressionOperatorMemberAccessAst(self.pos, TokenAst.dummy(TokenType.TkDot), current_local_variable.identifier)
                ast_1 = PostfixExpressionAst(self.pos, value, ast_0)
                ast_2 = LetStatementInitializedAst(self.pos, TokenAst.dummy(TokenType.KwLet), current_local_variable, TokenAst.dummy(TokenType.TkAssign), ast_1)
                ast_2.do_semantic_analysis(scope_handler, **kwargs)

            # Convert the destructure into a let statement, for example "let Vec(point=Point(x, y, z)) = vec".
            elif isinstance(current_local_variable, LocalVariableAttributeBindingAst):
                ast_0 = PostfixExpressionOperatorMemberAccessAst(self.pos, TokenAst.dummy(TokenType.TkDot), current_local_variable.identifier)
                ast_1 = PostfixExpressionAst(self.pos, value, ast_0)
                ast_2 = LetStatementInitializedAst(self.pos, TokenAst.dummy(TokenType.KwLet), current_local_variable.value, TokenAst.dummy(TokenType.TkAssign), ast_1)
                ast_2.do_semantic_analysis(scope_handler, **kwargs)

        # Make sure all the attributes have been assigned to, unless there is a ".." skip.
        assigned_attributes = Seq(self.items).filter_not_type(LocalVariableSkipArgumentsAst).map(lambda a: a.identifier)
        missing_attributes = attributes.map(lambda a: a.identifier).set_subtract(assigned_attributes)
        if missing_attributes and not skips:
            raise SemanticErrors.MISSING_ARGUMENT(self, missing_attributes[0], "destructure", "attribute")


__all__ = ["LocalVariableObjectDestructureAst"]
