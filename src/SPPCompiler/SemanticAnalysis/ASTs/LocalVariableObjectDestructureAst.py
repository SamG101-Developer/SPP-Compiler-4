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
        bracket_l_token: The left bracket token.
        items: The local variables in the destructure.
        bracket_r_token: The right bracket token.
    """

    class_type: "TypeAst"
    bracket_l_token: "TokenAst"
    items: List["LocalVariableSingleAst"]
    bracket_r_token: "TokenAst"

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        return f"{self.class_type.print(printer)}{self.bracket_l_token.print(printer)}{Seq(self.items).print(printer, ", ")}{self.bracket_r_token.print(printer)}"

    def do_semantic_analysis(self, scope_handler: ScopeHandler, **kwargs) -> None:
        from SPPCompiler.LexicalAnalysis.Tokens import TokenType
        from SPPCompiler.SemanticAnalysis.ASTs import (
            LocalVariableSkipArgumentsAst, LocalVariableSingleIdentifierAst, LocalVariableAttributeBindingAst, ConventionMovAst,
            PostfixExpressionOperatorMemberAccessAst, PostfixExpressionAst, LetStatementInitializedAst, TokenAst)

        value = kwargs["value"]
        value.do_semantic_analysis(scope_handler, **kwargs)

        # Semantically analyse the class type, to make sure it exists.
        self.class_type.do_semantic_analysis(scope_handler, **kwargs)
        attributes = Seq(scope_handler.current_scope.get_symbol(self.class_type).type.body.members)

        # Only allow 1 multi-skip inside a tuple.
        skips = Seq(self.items).filter_to_type(LocalVariableSkipArgumentsAst)
        if skips.length > 1:
            raise SemanticErrors.MULTIPLE_ARGUMENT_SKIPS(skips[0], skips[1])

        # Check the RHS is the same type as the class type.
        value_type = value.infer_type(scope_handler, **kwargs)
        if not InferredType(convention=ConventionMovAst, type=self.class_type).symbolic_eq(value_type, scope_handler.current_scope):
            raise SemanticErrors.TYPE_MISMATCH(self, value_type, self.class_type)

        nested_destructures = []
        for current_local_variable in Seq(self.items):
            # Don't allow the binding unpacking token for a type destructure: "let p = Point(..x)" makes no sense.
            if isinstance(current_local_variable, LocalVariableSkipArgumentsAst) and current_local_variable.binding:
                raise SemanticErrors.UNPACKING_TOKEN_IN_DESTRUCTURE(current_local_variable)
            elif isinstance(current_local_variable, LocalVariableSkipArgumentsAst):
                continue

            # Check the given variable exists as an attribute on the type: "let p = Point(x, ..)" requires "x" to be an
            # attribute of "Point".
            if not attributes.map(lambda a: a.identifier).contains(current_local_variable.identifier):
                raise SemanticErrors.UNKNOWN_IDENTIFIER(current_local_variable.identifier, attributes.map(lambda a: a.identifier.value).value, "attribute")

            # Convert the destructure into a let statement, for "let Point(x, y, z) = point".
            if isinstance(current_local_variable, LocalVariableSingleIdentifierAst):
                ast_0 = PostfixExpressionOperatorMemberAccessAst(
                    pos=self.pos,
                    dot_token=TokenAst.dummy(TokenType.TkDot),
                    identifier=current_local_variable.identifier)

                ast_1 = PostfixExpressionAst(
                    pos=self.pos,
                    lhs=value,
                    op=ast_0)

                ast_2 = LetStatementInitializedAst(
                    pos=self.pos,
                    let_keyword=TokenAst.dummy(TokenType.KwLet),
                    assign_to=current_local_variable,
                    assign_token=TokenAst.dummy(TokenType.TkAssign),
                    value=ast_1)

                nested_destructures.append(ast_2)

            # Convert the destructure into a let statement, for "let Vec(point=Point(x, y, z)) = vec".
            elif isinstance(current_local_variable, LocalVariableAttributeBindingAst):
                ast_0 = PostfixExpressionOperatorMemberAccessAst(
                    pos=self.pos,
                    dot_token=TokenAst.dummy(TokenType.TkDot),
                    identifier=current_local_variable.identifier)

                ast_1 = PostfixExpressionAst(
                    pos=self.pos,
                    lhs=value,
                    op=ast_0)

                ast_2 = LetStatementInitializedAst(
                    pos=self.pos,
                    let_keyword=TokenAst.dummy(TokenType.KwLet),
                    assign_to=current_local_variable.value,
                    assign_token=TokenAst.dummy(TokenType.TkAssign),
                    value=ast_1)

                if not type(current_local_variable.value).__name__.startswith("Local"):
                    value_type = current_local_variable.value.infer_type(scope_handler, **kwargs)
                    target_type = ast_1.infer_type(scope_handler, **kwargs)
                    if not value_type.symbolic_eq(target_type, scope_handler.current_scope):
                        raise SemanticErrors.TYPE_MISMATCH(current_local_variable, target_type, value_type)

                nested_destructures.append(ast_2)

        for nested_destructure in nested_destructures:
            nested_destructure.do_semantic_analysis(scope_handler, **kwargs)

        # Make sure all the attributes have been assigned to, unless there is a ".." skip.
        assigned_attributes = Seq(self.items).filter_not_type(LocalVariableSkipArgumentsAst)
        missing_attributes = attributes.map(lambda a: a.identifier).set_subtract(assigned_attributes.map(lambda a: a.identifier))
        if missing_attributes and not skips:
            raise SemanticErrors.MISSING_ARGUMENT(self, missing_attributes[0], "destructure", "attribute")
