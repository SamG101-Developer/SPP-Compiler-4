from __future__ import annotations
from dataclasses import dataclass
from ordered_set import OrderedSet
from typing import List

from SPPCompiler.SemanticAnalysis.ASTMixins.SemanticAnalyser import SemanticAnalyser
from SPPCompiler.SemanticAnalysis.ASTs.Meta.Ast import Ast
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstPrinter import *
from SPPCompiler.SemanticAnalysis.Utils.SemanticError import SemanticErrors
from SPPCompiler.SemanticAnalysis.Utils.Scopes import ScopeHandler
from SPPCompiler.Utils.Sequence import Seq


@dataclass
class FunctionArgumentGroupAst(Ast, SemanticAnalyser):
    """
    The FunctionArgumentGroupAst node is used to group the arguments to a function call, NOT the parameters of a
    function prototype (see FunctionParameterGroupAst).

    Attributes:
        paren_l_token: The left parenthesis token.
        arguments: The arguments of the function call.
        paren_r_token: The right parenthesis token.
    """

    paren_l_token: "TokenAst"
    arguments: List["FunctionArgumentAst"]
    paren_r_token: "TokenAst"

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        # Print the FunctionArgumentGroupAst.
        s = ""
        s += f"{self.paren_l_token.print(printer)}"
        s += f"{Seq(self.arguments).print(printer, ", ")}"
        s += f"{self.paren_r_token.print(printer)}"
        return s

    def do_semantic_pre_analysis(self, scope_handler: ScopeHandler, **kwargs) -> None:
        # This method sometimes needs to be called to ensure certain checks, but the rest of the semantic analysis also
        # handles the symbols' memory state, which isn't desired here.
        from SPPCompiler.SemanticAnalysis.ASTs import FunctionArgumentNormalAst, FunctionArgumentNamedAst

        # Check there are no duplicate named-argument identifiers for this group, and raise an exception if there are.
        named_arguments = Seq(self.arguments).filter_to_type(FunctionArgumentNamedAst).map(lambda a: a.identifier)
        if named_arguments.contains_duplicates():
            duplicate_named_argument_identifiers = named_arguments.non_unique_items()[0]
            raise SemanticErrors.DUPLICATE_ITEM(duplicate_named_argument_identifiers, "named function argument")

        # Ensure the ordering of arguments in this group is correct (Normal => Named).
        classification_ordering = {FunctionArgumentNormalAst: "Anonymous", FunctionArgumentNamedAst: "Named"}
        current_classifications = Seq(self.arguments).map(type).zip(Seq(self.arguments))
        sorted_classifications  = current_classifications.sort(key=lambda c: list(classification_ordering.keys()).index(c[0]))
        if current_classifications != sorted_classifications:
            difference = sorted_classifications.ordered_difference(current_classifications)
            raise SemanticErrors.INVALID_ORDER(difference.value, classification_ordering, "function argument")

    def do_semantic_analysis(self, scope_handler: ScopeHandler, **kwargs) -> None:
        from SPPCompiler.SemanticAnalysis.ASTs import (
            ConventionMovAst, ConventionRefAst, ConventionMutAst,
            IdentifierAst, PostfixExpressionAst, LetStatementInitializedAst)

        # Ensure the pre-analysis is done anyway.
        self.do_semantic_pre_analysis(scope_handler, **kwargs)

        # Define the sets of symbols that are borrowed immutable and mutable.
        borrows_ref = OrderedSet()
        borrows_mut = OrderedSet()

        for argument in self.arguments:
            argument.do_semantic_analysis(scope_handler, **kwargs)

            """Ownership Tracking"""

            # Get the symbol for the function call. This is only gettable if the argument is an identifier or an
            # attribute. In the case it is an attribute, use the outermost part, as long as it is an IdentifierAst. For
            # example, "1.foo.bar.baz()" would have a "None" symbol.
            match argument.value:
                case IdentifierAst():
                    symbol = scope_handler.current_scope.get_symbol(argument.value)
                case PostfixExpressionAst():
                    temp = argument.value
                    while isinstance(temp, PostfixExpressionAst): temp = temp.lhs
                    symbol = scope_handler.current_scope.get_symbol(temp) if isinstance(temp, IdentifierAst) else None
                case _:
                    symbol = None

            # Check the argument is fully initialized before being used. This prevents the common "double free" and
            # "use after free" errors.
            if symbol and symbol.memory_info.ast_consumed:
                match symbol.memory_info.ast_consumed:
                    case LetStatementInitializedAst(): msg1, msg2 = "is declared as uninitialized.", "without being initialized."
                    case _: msg1, msg2 = "is moved here.", "after being moved."

                raise SemanticErrors.USING_NON_INITIALIZED_VALUE(argument, symbol, msg1, msg2)

            # Check the argument doesn't contain partial moves. Non over-lapping partial moves are fine, ie "a.c" can be
            # accessed if "a.b" has been moved, but not if "a" has been moved.
            if symbol and symbol.memory_info.ast_partial_moves:
                for partial_move in symbol.memory_info.ast_partial_moves:
                    if str(partial_move).startswith(str(argument.value)):
                        raise SemanticErrors.USING_PARTIAL_MOVED_VALUE(argument, symbol)

            """Law of Exclusivity"""

            # Check the conventions for the borrowed arguments, to ensure that the law of exclusivity is followed. This
            # applies to overlapping borrows.
            match argument.convention:
                case ConventionMovAst() if symbol:
                    # Mark the symbol as moved or partially moved, for IdentifierAst and PostfixExpressionAsts
                    # respectively.
                    match argument.value:
                        case IdentifierAst(): symbol.memory_info.ast_consumed = argument
                        case PostfixExpressionAst(): symbol.memory_info.ast_partial_moves.append(argument.value)

                    # Cannot move an identifier already borrowed as a previous argument. For example, "a" cannot be
                    # moved into a function call if "a" or "a.b" is borrowed as a previous argument.
                    for borrow in borrows_ref | borrows_mut:
                        if str(borrow).startswith(str(argument.value)):
                            raise SemanticErrors.MEMORY_OVERLAP_CONFLICT(borrow, argument, "borrowed", "move")

                    # Cannot move from a borrowed context, so prevent partial moves for postfix identifiers whose
                    # outermost identifier is borrowed.
                    if isinstance(argument.value, PostfixExpressionAst) and symbol.memory_info.is_borrow:
                        raise SemanticErrors.MOVING_FROM_BORROWED_CONTEXT(argument, argument.value.op, symbol)

                case ConventionMutAst() if symbol:
                    # Cannot take a mutable borrow from an object that is already an immutable borrow. For example, if a
                    # parameter is "f(&a)", then "a" cannot be used as "call_func(&mut a)".
                    if symbol.memory_info.is_borrow_ref:
                        raise SemanticErrors.MEMORY_OVERLAP_CONFLICT(symbol.memory_info.ast_borrow, argument, "immutably borrowed", "mutably borrow")

                    # Can only take a mutable borrow from a variable that has been marked as mutable on declaration.
                    # This requires something like "let mut a = 0".
                    if not (symbol.is_mutable or symbol.memory_info.is_borrow_mut):
                        raise SemanticErrors.MUTABLE_BORROW_FROM_IMMUTABLE_SOURCE(argument, symbol)

                    # Ensure that no overlapping part of the variable is already borrowed either immutably or mutably.
                    # This enforces the law of exclusivity.
                    for borrow in borrows_ref | borrows_mut:
                        if str(borrow).startswith(str(argument.value)) or str(argument.value).startswith(str(borrow)):
                            how = "immutably" if borrow in borrows_ref else "mutably"
                            raise SemanticErrors.MEMORY_OVERLAP_CONFLICT(borrow, argument, f"{how} borrowed", "mutably borrow")

                    # Add the mutable borrow to the set of mutable borrows.
                    borrows_mut.add(argument)

                case ConventionRefAst() if symbol:
                    # Ensure that no overlapping part of the variable is already mutably borrowed. This enforces the law
                    # of exclusivity.
                    for borrow in borrows_mut:
                        if str(borrow).startswith(str(argument.value)) or str(argument.value).startswith(str(borrow)):
                            raise SemanticErrors.MEMORY_OVERLAP_CONFLICT(borrow, argument, f"immutably borrowed", "mutably borrow")

                    # Add the immutable borrow to the set of immutable borrows.
                    borrows_ref.add(argument)


__all__ = ["FunctionArgumentGroupAst"]
