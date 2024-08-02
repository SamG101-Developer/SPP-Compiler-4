from dataclasses import dataclass
from ordered_set import OrderedSet
from typing import List

from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstMixins import SemanticAnalyser
from SPPCompiler.SemanticAnalysis.ASTs.Meta.Ast import Ast
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstPrinter import *
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstUtils import ensure_memory_integrity
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

        # Analyse each argument.
        Seq(self.arguments).map(lambda a: a.do_semantic_analysis(scope_handler, **kwargs))

    def do_semantic_analysis(self, scope_handler: ScopeHandler, **kwargs) -> None:
        from SPPCompiler.SemanticAnalysis.ASTs import ConventionMovAst, ConventionRefAst, ConventionMutAst

        # Note that the memory rules here are also implemented in AstUtils.ensure_memory_integrity, but have to be split
        # here because of the multiple conventions that can be used.

        # Ensure the pre-analysis is done anyway.
        self.do_semantic_pre_analysis(scope_handler, **kwargs)

        # Define the sets of symbols that are borrowed immutable and mutable.
        borrows_ref = OrderedSet()
        borrows_mut = OrderedSet()

        for argument in self.arguments:
            argument.do_semantic_analysis(scope_handler, **kwargs)

            # Check for uninitialized / partially-moved values.
            symbol = scope_handler.current_scope.get_outermost_variable_symbol(argument.value)
            ensure_memory_integrity(self, argument.value, argument, scope_handler, check_move_from_borrowed_context=False, mark_symbols=False)

            # Enforce the Law of Exclusivity: check the conventions for the borrowed arguments, to ensure there are no
            # overlapping borrows.
            match argument.convention:
                case ConventionMovAst() if symbol:
                    # mark symbols are moves / partially move, and check for moving from a borrowed context.
                    ensure_memory_integrity(self, argument.value, argument, scope_handler, check_move=False, check_partial_move=False)

                    # Cannot move an identifier already borrowed as a previous argument. For example, "a" cannot be
                    # moved into a function call if "a" or "a.b" is borrowed as a previous argument.
                    for borrow in borrows_ref | borrows_mut:
                        if str(borrow).startswith(str(argument.value)):
                            how = "immutably" if borrow in borrows_ref else "mutably"
                            raise SemanticErrors.MEMORY_OVERLAP_CONFLICT(borrow, argument.value, f"{how} borrow", "move")

                case ConventionMutAst() if symbol:
                    # Cannot take a mutable borrow from an object that is already an immutable borrow. For example, if a
                    # parameter is "f(&a)", then "a" cannot be used as "call_func(&mut a)".
                    if symbol.memory_info.is_borrow_ref:
                        raise SemanticErrors.MEMORY_OVERLAP_CONFLICT(symbol.memory_info.ast_borrow, argument.value, "mutably borrow", "immutably borrow")

                    # Can only take a mutable borrow from a variable that has been marked as mutable on declaration.
                    # This requires something like "let mut a = 0".
                    if not (symbol.is_mutable or symbol.memory_info.is_borrow_mut):
                        raise SemanticErrors.MUTABLE_BORROW_FROM_IMMUTABLE_SOURCE(argument, symbol)

                    # Ensure that no overlapping part of the variable is already borrowed either immutably or mutably.
                    # This enforces the law of exclusivity.
                    for borrow in borrows_ref | borrows_mut:
                        if str(borrow).startswith(str(argument.value)) or str(argument.value).startswith(str(borrow)):
                            how = "immutably" if borrow in borrows_ref else "mutably"
                            raise SemanticErrors.MEMORY_OVERLAP_CONFLICT(borrow, argument.value, f"{how} borrowed", "mutably borrow")

                    # Add the mutable borrow to the set of mutable borrows.
                    borrows_mut.add(argument.value)

                case ConventionRefAst() if symbol:
                    # Ensure that no overlapping part of the variable is already mutably borrowed. This enforces the law
                    # of exclusivity.
                    for borrow in borrows_mut:
                        if str(borrow).startswith(str(argument.value)) or str(argument.value).startswith(str(borrow)):
                            raise SemanticErrors.MEMORY_OVERLAP_CONFLICT(borrow, argument.value, "mutably borrow", f"immutably borrow")

                    # Add the immutable borrow to the set of immutable borrows.
                    borrows_ref.add(argument.value)

    def __copy__(self):
        return FunctionArgumentGroupAst(self.pos, self.paren_l_token, self.arguments.copy(), self.paren_r_token)


__all__ = ["FunctionArgumentGroupAst"]
