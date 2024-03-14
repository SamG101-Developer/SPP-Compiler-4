from __future__ import annotations
from dataclasses import dataclass
from ordered_set import OrderedSet
from typing import List

from src.SemanticAnalysis.Analysis.SemanticAnalysis import SemanticAnalysis
from src.SemanticAnalysis.Analysis.SemanticError import SemanticError
from src.SemanticAnalysis.Symbols.Scopes import ScopeHandler

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
class FunctionArgumentGroupAst(Ast, SemanticAnalysis):
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

    def do_semantic_analysis(self, scope_handler: ScopeHandler, **kwargs) -> None:
        # Check no named arguments have the same name
        named_arguments = Seq(self.arguments).filter(lambda a: isinstance(a, GenericArgumentNamedAst)).map(lambda a: a.identifier)
        if named_arguments.contains_duplicates():
            duplicate_named_arguments = named_arguments.non_unique_items()[0]
            exception = SemanticError(f"Duplicate argument names '{duplicate_named_arguments[0]}' found in function prototype:")
            exception.add_traceback(duplicate_named_arguments[0].pos, f"Parameter '{duplicate_named_arguments[0]}' declared here.")
            exception.add_traceback(duplicate_named_arguments[1].pos, f"Parameter '{duplicate_named_arguments[1]}' re-declared here.")
            raise exception

        # Check argument order is Normal -> Named
        ordering = {FunctionArgumentNormalAst: "Normal", FunctionArgumentNamedAst: "Named"}
        current_classifications = Seq(self.arguments).map(lambda p: (type(p), p))
        sorted_classifications = current_classifications.sort(key=lambda t: list(ordering.keys()).index(t[0]))
        if current_classifications != sorted_classifications:
            difference = sorted_classifications.ordered_difference(current_classifications)
            exception = SemanticError(f"Invalid parameter order in function prototype:")
            exception.add_traceback(difference[-2][1].pos, f"{ordering[difference[-2][0]]} parameter '{difference[-2][1]}' declared here.")
            exception.add_traceback(difference[-1][1].pos, f"{ordering[difference[-1][0]]} parameter '{difference[-1][1]}' declared here.")
            raise exception

        # TODO : Expand tuple into multiple arguments, so that each part is analysed.
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
                    case LetStatementUninitializedAst(): error_message = "declared as uninitialized here"
                    case _: error_message = "moved here"

                exception = SemanticError(f"Variable '{argument.value}' used before being initialized:")
                exception.add_traceback(sym.memory_info.ast_consumed.pos, f"Variable '{argument.value}' {error_message}.")
                exception.add_traceback(argument.value.pos, f"Variable '{argument.value}' used here.")
                raise exception

            # Check that an argument is not partially moved before being used: applies to (postfix) identifier only.
            # Non-overlapping partial moves are ok, for example, if "a.b" is moved, "a.c" is fine to use, but "a" isn't.
            if sym and sym.memory_info.ast_partial_moves:
                for existing_partial_move in sym.memory_info.ast_partial_moves:
                    if existing_partial_move == argument.value:
                        exception = SemanticError(f"Variable '{argument.value}' used after being moved:")
                        exception.add_traceback(existing_partial_move.pos, f"Variable '{argument.value}' moved here.")
                        exception.add_traceback(argument.value.pos, f"Variable '{argument.value}' used here.")
                        raise exception

                for existing_partial_move in sym.memory_info.ast_partial_moves:
                    existing_partial_move = str(existing_partial_move)
                    if existing_partial_move.startswith(str(argument.value)):
                        exception = SemanticError(f"Variable '{argument.value}' used after being partially moved:")

                        # TODO : if any partial moves are in the same place then consolidate them into one trace
                        for partial_move in sym.memory_info.ast_partial_moves:
                            exception.add_traceback(partial_move.pos, f"Variable '{partial_move}' partially moved here.")
                        exception.add_traceback(argument.value.pos, f"Variable '{argument.value}' used here.")
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
                            exception = SemanticError(f"Cannot move an object '{argument.value}' that's already borrowed:")
                            exception.add_traceback(i, f"Object '{argument.value}' already borrowed here.")
                            exception.add_traceback(argument.convention.pos, f"Object '{argument.value}' attempted to be borrowed here.")
                            raise exception

                    # Cannot move from a borrowed context, so enforce this here too.
                    if sym.memory_info.is_borrow:
                        exception = SemanticError(f"Cannot move from a borrowed context:")
                        exception.add_traceback(sym.memory_info.ast_borrow.pos, f"Variable '{argument.value}' borrowed here.")
                        exception.add_traceback(argument.pos, f"Partial move '{argument}' attempted here.")
                        raise exception

                    # Otherwise, mark the left most identifier as partially moved.
                    if isinstance(argument.value, PostfixExpressionAst) and isinstance(argument.value.op, PostfixExpressionOperatorMemberAccessAst):
                        sym.memory_info.ast_partial_moves.append(argument.value)

                case ConventionMutAst():
                    # Can only take a borrow from a (postfix) identifier. TODO : remove this soon
                    if not sym:
                        exception = SemanticError(f"Cannot take an borrow from a non-identifier:")
                        exception.add_traceback(argument.convention.pos, f"Borrow '{argument.convention}' taken here.")
                        raise exception

                    # Can only take a mutable borrow from a mutable symbol
                    if not (sym.is_mutable or sym.memory_info.is_borrow_mut):
                        exception = SemanticError(f"Cannot take a mutable borrow from an immutable variable:")
                        exception.add_traceback(sym.memory_info.ast_initialized.pos, f"Variable '{argument.value}' declared immutably here.")
                        exception.add_traceback(argument.convention.pos, f"Mutable borrow '{argument.value}' taken here.")
                        raise exception

                    # Cannot take a mutable borrow from an immutable borrow
                    if sym.memory_info.is_borrow_ref:
                        exception = SemanticError(f"Cannot take a mutable borrow from an immutable borrow:")
                        exception.add_traceback(sym.memory_info.ast_borrow.pos, f"Variable '{argument.value}' borrowed immutably here.")
                        exception.add_traceback(argument.convention.pos, f"Mutable borrow '{argument.value}' taken here.")
                        raise exception

                    # For a mutable borrow to take place, ensure that no other overlapping part of the variable is
                    # already borrowed immutably or mutably.
                    for i, existing_borrow in borrows_ref | borrows_mut:
                        if existing_borrow.startswith(str(argument.value)):
                            exception = SemanticError(f"Cannot take a mutable borrow to an object '{argument.value}' that's already borrowed:")
                            exception.add_traceback(i, f"Object '{argument.value}' already borrowed here.")
                            exception.add_traceback(argument.convention.pos, f"Object '{argument.value}' attempted to ebe borrowed mutably here.")
                            raise exception

                    borrows_mut.add((argument.value.pos, str(argument.value)))

                case ConventionRefAst():
                    # Can only take a borrow from a (postfix) identifier. TODO : remove this soon
                    # if not sym:
                    #     exception = SemanticError(f"Cannot take a borrow from a non-identifier:")
                    #     exception.add_traceback(argument.convention.pos, f"Borrow '{argument.convention}' taken here.")
                    #     raise exception

                    # For an immutable borrow to take place, ensure that no other overlapping part of the variable is
                    # already borrowed mutably.
                    for i, existing_borrow in borrows_mut:
                        existing_borrow_check = str(existing_borrow)
                        if existing_borrow_check.startswith(str(argument.value)):
                            exception = SemanticError(f"Cannot take a immutable borrow to an object '{argument.value}' that's already mutably borrowed:")
                            exception.add_traceback(i, f"Object '{argument.value}' already mutably borrowed here.")
                            exception.add_traceback(argument.convention.pos, f"Object '{argument.value}' attempted to be borrowed immutably here.")
                            raise exception

                    borrows_ref.add((argument.value.pos, str(argument.value)))


__all__ = ["FunctionArgumentGroupAst"]
