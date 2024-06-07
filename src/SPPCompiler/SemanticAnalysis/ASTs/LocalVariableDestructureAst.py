from dataclasses import dataclass
from typing import List

from SPPCompiler.SemanticAnalysis.ASTMixins.SemanticAnalyser import SemanticAnalyser
from SPPCompiler.SemanticAnalysis.ASTs.Meta.Ast import Ast
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstPrinter import *
from SPPCompiler.SemanticAnalysis.Utils.Scopes import ScopeHandler
from SPPCompiler.SemanticAnalysis.Utils.SemanticError import SemanticError, SemanticErrorType
from SPPCompiler.Utils.Sequence import Seq


@dataclass
class LocalVariableDestructureAst(Ast, SemanticAnalyser, SemanticAnalyser):
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
        from SPPCompiler.SemanticAnalysis.ASTs.LocalVariableSkipArgumentAst import LocalVariableSkipArgumentAst
        from SPPCompiler.SemanticAnalysis.ASTs.LocalVariableSingleAst import LocalVariableSingleAst
        from SPPCompiler.SemanticAnalysis.ASTs.LocalVariableAssignmentAst import LocalVariableAssignmentAst
        from SPPCompiler.SemanticAnalysis.ASTs.PostfixExpressionOperatorMemberAccessAst import PostfixExpressionOperatorMemberAccessAst
        from SPPCompiler.SemanticAnalysis.ASTs.PostfixExpressionAst import PostfixExpressionAst
        from SPPCompiler.SemanticAnalysis.ASTs.LetStatementInitializedAst import LetStatementInitializedAst
        from SPPCompiler.SemanticAnalysis.ASTs.TokenAst import TokenAst
        from SPPCompiler.LexicalAnalysis.Tokens import TokenType

        # Semantically analyse the class type, to make sure it exists.
        self.class_type.do_semantic_analysis(scope_handler, **kwargs)
        attributes = Seq(scope_handler.current_scope.get_symbol(self.class_type).type.body.members)

        # Only allow 1 multi-skip inside a tuple.
        skips = Seq(self.items).filter_to_type(LocalVariableSkipArgumentAst)
        if skips.length > 1:
            exception = SemanticError()
            exception.add_info(
                pos=skips[1].pos, tag_message="Multiple skip arguments found.")
            exception.add_error(
                pos=skips[1].pos, error_type=SemanticErrorType.ORDER_ERROR,
                tag_message="Multiple skip arguments in a tuple.",
                message="Only one skip argument is allowed in a tuple.",
                tip="Remove the additional skip argument.")
            raise exception

        for current_local_variable in self.items:
            # Don't allow the unpacking token for a type destructure: "let p = Point(..x)" makes no sense.
            if isinstance(current_local_variable, LocalVariableSingleAst) and current_local_variable.unpack_token:
                exception = SemanticError()
                exception.add_error(
                    pos=current_local_variable.unpack_token.pos, error_type=SemanticErrorType.ORDER_ERROR,
                    tag_message="Unpacking token in a destructure.",
                    message="Unpacking tokens are not allowed in a destructure.",
                    tip="Remove the unpacking token.")
                raise exception

            # Check the given variable exists as an attribute on the type: "let p = Point(x, ..)" requires "x" to be an
            # attribute of "Point".
            if not attributes.map(lambda a: a.identifier).contains(current_local_variable.identifier):
                exception = SemanticError()
                exception.add_error(
                    pos=current_local_variable.identifier.pos, error_type=SemanticErrorType.NAME_ERROR,
                    tag_message=f"Attribute '{current_local_variable.identifier.value}' not found in type '{self.class_type.value}'.",
                    message="Attribute not found in type.",
                    tip="Ensure the attribute exists in the type.")
                raise exception

            # Convert the destructure into a let statement, for "let Point(x, y, z) = point".
            if isinstance(current_local_variable, LocalVariableSingleAst):
                ast_0 = PostfixExpressionOperatorMemberAccessAst(
                    pos=self.pos,
                    dot_token=TokenAst.dummy(TokenType.TkDot),
                    identifier=current_local_variable.identifier)

                ast_1 = PostfixExpressionAst(
                    pos=self.pos,
                    lhs=kwargs["value"],
                    op=ast_0)

                ast_2 = LetStatementInitializedAst(
                    pos=self.pos,
                    let_keyword=TokenAst.dummy(TokenType.KwLet),
                    assign_to=current_local_variable,
                    assign_token=TokenAst.dummy(TokenType.TkAssign, pos=self.pos),
                    value=ast_1)

            # Convert the destructure into a let statement, for "let Vec(point=Point(x, y, z)) = vec".
            elif isinstance(current_local_variable, LocalVariableAssignmentAst):
                ast_0 = PostfixExpressionOperatorMemberAccessAst(
                    pos=self.pos,
                    dot_token=TokenAst.dummy(TokenType.TkDot),
                    identifier=current_local_variable.identifier)

                ast_1 = PostfixExpressionAst(
                    pos=self.pos,
                    lhs=kwargs["value"],
                    op=ast_0)

                ast_2 = LetStatementInitializedAst(
                    pos=self.pos,
                    let_keyword=TokenAst.dummy(TokenType.KwLet),
                    assign_to=current_local_variable.value,
                    assign_token=TokenAst.dummy(TokenType.TkAssign, pos=self.pos),
                    value=ast_1)

        # Make sure all the attributes have been assigned to, unless there is a ".." skip.
        assigned_attributes = Seq(self.items).filter_not_type(LocalVariableSkipArgumentAst)
        if assigned_attributes.length < attributes.length and not skips:
            exception = SemanticError()
            exception.add_error(
                pos=self.pos, error_type=SemanticErrorType.ORDER_ERROR,
                tag_message=f"Missing attribute(s) for type '{self.class_type.value}': {attributes.join(", ")}.",
                message="Not all attributes have been assigned to.",
                tip="Ensure all attributes are assigned to.")
            raise exception
