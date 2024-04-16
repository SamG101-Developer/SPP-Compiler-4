from __future__ import annotations
from dataclasses import dataclass
from ordered_set import OrderedSet
from typing import List

from src.SemanticAnalysis.ASTMixins.SemanticAnalyser import SemanticAnalyser
from src.SemanticAnalysis.Utils.SemanticError import SemanticError, SemanticErrorType, SemanticErrorStringFormatType
from src.SemanticAnalysis.Utils.Scopes import ScopeHandler

from src.SemanticAnalysis.ASTs.Meta.Ast import Ast
from src.SemanticAnalysis.ASTs.Meta.AstPrinter import *

from src.SemanticAnalysis.ASTs.ConventionRefAst import ConventionRefAst
from src.SemanticAnalysis.ASTs.ConventionMovAst import ConventionMovAst
from src.SemanticAnalysis.ASTs.ConventionMutAst import ConventionMutAst
from src.SemanticAnalysis.ASTs.FunctionArgumentAst import FunctionArgumentAst
from src.SemanticAnalysis.ASTs.FunctionArgumentNamedAst import FunctionArgumentNamedAst
from src.SemanticAnalysis.ASTs.FunctionArgumentNormalAst import FunctionArgumentNormalAst
from src.SemanticAnalysis.ASTs.GenericArgumentNamedAst import GenericArgumentNamedAst
from src.SemanticAnalysis.ASTs.IdentifierAst import IdentifierAst
from src.SemanticAnalysis.ASTs.LetStatementUninitializedAst import LetStatementUninitializedAst
from src.SemanticAnalysis.ASTs.PostfixExpressionAst import PostfixExpressionAst
from src.SemanticAnalysis.ASTs.PostfixExpressionOperatorMemberAccessAst import PostfixExpressionOperatorMemberAccessAst
from src.SemanticAnalysis.ASTs.TokenAst import TokenAst

from src.Utils.Sequence import Seq


@dataclass
class FunctionArgumentGroupAst(Ast, SemanticAnalyser):
    """
    The FunctionArgumentGroupAst node is used to group the arguments to a function call, NOT the parameters of a
    function prototype (see FunctionParameterGroupAst).

    Attributes:
        - paren_l_token: The left parenthesis token.
        - arguments: The arguments of the function call.
        - paren_r_token: The right parenthesis token.
    """

    paren_l_token: TokenAst
    arguments: List[FunctionArgumentAst]
    paren_r_token: TokenAst

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        # Print the FunctionArgumentGroupAst.
        s = ""
        s += f"{self.paren_l_token.print(printer)}"
        s += f"{Seq(self.arguments).print(printer, ", ")}"
        s += f"{self.paren_r_token.print(printer)}"
        return s

    def do_semantic_pre_analysis(self, scope_handler: ScopeHandler, **kwargs) -> None:
        # Check no named arguments have the same name
        named_arguments = Seq(self.arguments).filter(lambda a: isinstance(a, FunctionArgumentNamedAst)).map(lambda a: a.identifier)
        if named_arguments.contains_duplicates():
            duplicate_named_arguments = named_arguments.non_unique_items()[0]
            exception = SemanticError()
            exception.add_info(
                pos=duplicate_named_arguments[0].pos,
                tag_message=f"Argument '{duplicate_named_arguments[0]}' declared here")
            exception.add_error(
                pos=duplicate_named_arguments[1].pos,
                error_type=SemanticErrorType.NAME_ERROR,
                message=f"Cannot have duplicate named arguments in a function call",
                tag_message=f"Argument '{duplicate_named_arguments[0]}' re-declared here",
                tip=f"Correct the name of the argument")
            raise exception

        # Check argument order is Normal -> Named
        ordering = {FunctionArgumentNormalAst: "Unnamed", FunctionArgumentNamedAst: "Named"}
        current_classifications = Seq(self.arguments).map(lambda p: (type(p), p))
        sorted_classifications = current_classifications.sort(key=lambda t: list(ordering.keys()).index(t[0]))
        if current_classifications != sorted_classifications:
            difference = sorted_classifications.ordered_difference(current_classifications)
            exception = SemanticError()
            exception.add_info(
                pos=difference[-2][1].pos,
                tag_message=f"{ordering[difference[-2][0]]} argument '{difference[-2][1]}' declared here")
            exception.add_error(
                pos=difference[-1][1].pos,
                error_type=SemanticErrorType.ORDER_ERROR,
                message=f"Invalid argument order in function call:",
                tag_message=f"{ordering[difference[-1][0]]} argument '{difference[-1][1]}' found here",
                tip=f"Make sure argument order is Unnamed -> Named")
            raise exception

    def do_semantic_analysis(self, scope_handler: ScopeHandler, **kwargs) -> None:
        self.do_semantic_pre_analysis(scope_handler, **kwargs)

        # TODO : Expand tuple into multiple arguments, so that each part is analysed (for moves).
        # TODO : Check memory status of symbols too, not just their convention.
        # TODO : Allow borrows from non-identifiers (will be temp values to the caller's scope)

        # Begin memory checks here to prevent overlaps of borrows.
        borrows_ref = OrderedSet()
        borrows_mut = OrderedSet()

        for argument in self.arguments:
            argument.do_semantic_analysis(scope_handler, **kwargs)

            match argument.value:
                case IdentifierAst(): sym = scope_handler.current_scope.get_symbol(argument.value)
                case PostfixExpressionAst() if isinstance(argument.value.op, PostfixExpressionOperatorMemberAccessAst):
                    temp = argument.value
                    while isinstance(temp, PostfixExpressionAst):
                        temp = temp.lhs
                    sym = scope_handler.current_scope.get_symbol(temp) if isinstance(temp, IdentifierAst) else None
                case _: sym = None

            # Check that an argument is initialized before being used: applies to (postfix) identifier only.
            if sym and sym.memory_info.ast_consumed:
                match sym.memory_info.ast_consumed:
                    case LetStatementUninitializedAst():
                        error_message_1 = "is declared as uninitialized here"
                        error_message_2 = "without being initialized"
                    case _:
                        error_message_1 = "is moved here"
                        error_message_2 = "after being moved"

                exception = SemanticError().add_info(
                    pos=sym.memory_info.ast_consumed.pos,
                    tag_message=f"Variable '{argument.value}' {error_message_1}")
                exception.add_error(
                    pos=argument.value.pos,
                    error_type=SemanticErrorType.MEMORY_ERROR,
                    message=f"An uninitialized variable cannot be used",
                    tag_message=f"Variable '{argument.value}' used {error_message_2}",
                    tip=f"Initialize the variable before using it")
                raise exception

            # Check that an argument is not partially moved before being used: applies to (postfix) identifier only.
            # Non-overlapping partial moves are ok, for example, if "a.b" is moved, "a.c" is fine to use, but "a" isn't.
            if sym and sym.memory_info.ast_partial_moves:
                for existing_partial_move in sym.memory_info.ast_partial_moves:
                    if existing_partial_move == argument.value:
                        exception = SemanticError().add_info(
                            pos=existing_partial_move.pos,
                            tag_message=f"Variable '{argument.value}' partially moved here")
                        exception.add_error(
                            pos=argument.value.pos,
                            error_type=SemanticErrorType.MEMORY_ERROR,
                            message="A partially moved variable cannot be used",
                            tag_message=f"Variable '{argument.value}' partially moved here",
                            tip="Initialize the variable before using it")
                        raise exception

                for existing_partial_move in sym.memory_info.ast_partial_moves:
                    existing_partial_move = str(existing_partial_move)
                    if existing_partial_move.startswith(str(argument.value)):
                        partial_move_string = f"Set values for attributes "
                        for partial_move in sym.memory_info.ast_partial_moves:
                            partial_move_string += f"'{partial_move.op.identifier}', "
                        partial_move_string = partial_move_string[:-2]
                        partial_move_string += " before using it."

                        exception = SemanticError()
                        exception.add_error(
                            pos=argument.value.pos,
                            error_type=SemanticErrorType.MEMORY_ERROR,
                            message="Cannot use a partially moved variable:",
                            tag_message=f"Variable '{argument.value}' used here.",
                            tip=partial_move_string)

                        raise exception

            # Check conventions of arguments to enforce the law of exclusivity. Note that "&mut a", "&mut a.b" is an
            # overlap, but "&mut a.b", "&mut a.c" is not.
            match argument.convention:
                case ConventionMovAst() if sym:
                    # Mark the symbol as consumed, if the argument is a single identifier.
                    # TODO: remove initialization AST?
                    if isinstance(argument.value, IdentifierAst):
                        sym.memory_info.ast_consumed = argument.value

                    # Cannot move an identifier if is borrowed as a previous argument.
                    for i, existing_borrow in borrows_mut | borrows_ref:
                        existing_borrow_check = str(existing_borrow)
                        if existing_borrow_check.startswith(str(argument.value)):
                            exception = SemanticError()
                            exception.add_info(
                                i,
                                tag_message=f"Object '{argument.value}' borrowed here")
                            exception.add_error(
                                pos=argument.pos,
                                error_type=SemanticErrorType.MEMORY_ERROR,
                                message=f"Cannot move an object that's currently being borrowed",
                                tag_message=f"Same object moved here",
                                tip=f"Move a different object, or borrow a copy of the object instead")
                            raise exception

                            # exception = SemanticError(f"Cannot move an object '{argument.value}' that's already borrowed:")
                            # exception.add_traceback(i, f"Object '{argument.value}' already borrowed here.")
                            # exception.add_traceback(argument.convention.pos, f"Object '{argument.value}' attempted to be borrowed here.")
                            # raise exception

                    # Cannot move from a borrowed context, so enforce this here too.
                    if sym.memory_info.is_borrow and isinstance(argument.value, PostfixExpressionAst) and isinstance(argument.value.op, PostfixExpressionOperatorMemberAccessAst):
                        exception = SemanticError()
                        exception.add_info(
                            sym.memory_info.ast_borrow.pos,
                            tag_message=f"Parameter '{sym.name}' declared as a borrow here")
                        exception.add_error(
                            pos=argument.value.op.pos,
                            error_type=SemanticErrorType.MEMORY_ERROR,
                            message=f"Cannot move from a borrowed context",
                            tag_message=f"Partial move '{argument}' attempted here",
                            tip=f"Move a different object, or borrow a copy of the object instead")
                        raise exception

                    # Otherwise, mark the left most identifier as partially moved.
                    if isinstance(argument.value, PostfixExpressionAst) and isinstance(argument.value.op, PostfixExpressionOperatorMemberAccessAst):
                        sym.memory_info.ast_partial_moves.append(argument.value)

                case ConventionMutAst():
                    # Can only take a borrow from a (postfix) identifier. TODO : remove this soon
                    if not sym:
                        exception = SemanticError().add_error(
                            pos=argument.convention.pos,
                            error_type=SemanticErrorType.MEMORY_ERROR,
                            message=f"Cannot take a borrow from a non-identifier",
                            tag_message=f"Borrow '{argument.convention}' taken here",
                            tip="Store the value in a variable before borrowing it")
                        raise exception

                    # Cannot take a mutable borrow from an immutable borrow
                    if sym.memory_info.is_borrow_ref:
                        exception = SemanticError()
                        exception.add_info(
                            pos=sym.memory_info.ast_borrow.pos,
                            tag_message=f"Variable '{argument.value}' borrowed immutably here")
                        exception.add_error(
                            pos=argument.convention.pos,
                            error_type=SemanticErrorType.MEMORY_ERROR,
                            message=f"Cannot take a mutable borrow from an immutable borrow",
                            tag_message=f"Mutable borrow '{argument.value}' taken here",
                            tip=f"Declare the variable as mutable")
                        raise exception

                    # Can only take a mutable borrow from a mutable symbol
                    if not (sym.is_mutable or sym.memory_info.is_borrow_mut):
                        exception = SemanticError()
                        exception.add_info(
                            pos=sym.memory_info.ast_initialized.pos,
                            tag_message=f"Variable '{argument.value}' declared immutably here")
                        exception.add_error(
                            pos=argument.convention.pos,
                            error_type=SemanticErrorType.MEMORY_ERROR,
                            message=f"Cannot take a mutable borrow from an immutable variable",
                            tag_message=f"Mutable borrow '{argument.value}' taken here",
                            tip=f"Declare the variable as mutable")
                        raise exception

                    # For a mutable borrow to take place, ensure that no other overlapping part of the variable is
                    # already borrowed immutably or mutably.
                    for i, existing_borrow in borrows_ref | borrows_mut:
                        if existing_borrow.startswith(str(argument.value)) or str(argument.value).startswith(existing_borrow):
                            exception = SemanticError()
                            exception.add_info(
                                i,
                                tag_message=f"Object '{existing_borrow_check}' borrowed here",)
                            exception.add_error(
                                argument.convention.pos,
                                error_type=SemanticErrorType.MEMORY_ERROR,
                                message=f"Mutable borrows cannot overlap with existing borrows",
                                tag_message=f"Object '{argument.value}' borrowed here",
                                tip=f"Ensure current borrows do not overlap with each other")
                            raise exception

                    borrows_mut.add((argument.value.pos, str(argument.value)))

                case ConventionRefAst():
                    # Can only take a borrow from a (postfix) identifier. TODO : remove this soon
                    if not sym:
                        exception = SemanticError().add_error(
                            pos=argument.convention.pos,
                            error_type=SemanticErrorType.MEMORY_ERROR,
                            message=f"Cannot take a borrow from a non-identifier",
                            tag_message=f"Borrow '{argument.convention}' taken here",
                            tip="Store the value in a variable before borrowing it")
                        raise exception

                    # For an immutable borrow to take place, ensure that no other overlapping part of the variable is
                    # already borrowed mutably.
                    for i, existing_borrow in borrows_mut:
                        existing_borrow_check = str(existing_borrow)
                        if existing_borrow_check.startswith(str(argument.value)) or str(argument.value).startswith(existing_borrow):
                            exception = SemanticError()
                            exception.add_info(
                                i,
                                tag_message=f"Object '{existing_borrow_check}' mutably borrowed here")
                            exception.add_error(
                                argument.convention.pos,
                                error_type=SemanticErrorType.MEMORY_ERROR,
                                message=f"Immutable borrows cannot overlap with existing mutable borrows",
                                tag_message=f"Object '{argument.value}' immutably borrowed here",
                                tip=f"Ensure current borrows do not overlap with each other")
                            raise exception

                    borrows_ref.add((argument.value.pos, str(argument.value)))


__all__ = ["FunctionArgumentGroupAst"]
