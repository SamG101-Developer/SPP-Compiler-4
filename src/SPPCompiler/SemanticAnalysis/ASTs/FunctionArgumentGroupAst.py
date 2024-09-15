from dataclasses import dataclass
from ordered_set import OrderedSet
from typing import List

from SPPCompiler.LexicalAnalysis.Tokens import TokenType
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstMixins import SemanticAnalyser
from SPPCompiler.SemanticAnalysis.ASTs.Meta.Ast import Ast, Default
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstPrinter import *
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstUtils import ensure_memory_integrity
from SPPCompiler.SemanticAnalysis.Utils.CommonTypes import CommonTypes
from SPPCompiler.SemanticAnalysis.Utils.SemanticError import SemanticErrors
from SPPCompiler.SemanticAnalysis.Utils.Scopes import ScopeHandler
from SPPCompiler.Utils.Sequence import Seq


@dataclass
class FunctionArgumentGroupAst(Ast, Default, SemanticAnalyser):
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

    @staticmethod
    def default() -> Default:
        from SPPCompiler.SemanticAnalysis.ASTs import TokenAst
        return FunctionArgumentGroupAst(-1, TokenAst.dummy(TokenType.TkParenL), [], TokenAst.dummy(TokenType.TkParenR))

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
        from SPPCompiler.LexicalAnalysis.Lexer import Lexer
        from SPPCompiler.SyntacticAnalysis.Parser import Parser
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

        # Edit tuple-expansion argument into multiple arguments. For example, "..tuple" -> "tuple.0, tuple.1".
        for argument in Seq(self.arguments).filter_to_type(FunctionArgumentNormalAst):
            if argument.unpack_token:

                # Make sure any arguments being unpacked are tuples.
                tuple_argument_type = argument.value.infer_type(scope_handler, **kwargs).type
                if not tuple_argument_type.without_generics().symbolic_eq(CommonTypes.tuple([]), scope_handler.current_scope):
                    raise SemanticErrors.UNPACKING_NON_TUPLE_ARGUMENT(argument.value, tuple_argument_type)

                # Remove the "..argument", and replace it with each part of the tuple.
                tuple_argument_index = self.arguments.index(argument)
                self.arguments.remove(argument)
                for i in range(len(tuple_argument_type.types[-1].generic_arguments.arguments)):
                    tuple_subscript_code = Parser(Lexer(f"{argument.value}.{i}").lex(), "").parse_expression().parse_once()
                    self.arguments.insert(tuple_argument_index + i, FunctionArgumentNormalAst(argument.pos, argument.convention, None, tuple_subscript_code))

        # Analyse each argument.
        Seq(self.arguments).map(lambda a: a.do_semantic_analysis(scope_handler, **kwargs))

    def do_semantic_analysis(self, scope_handler: ScopeHandler, function_prototype_ast=None, is_async=None, **kwargs) -> None:
        from SPPCompiler.SemanticAnalysis.ASTs import ConventionMovAst, ConventionRefAst, ConventionMutAst
        from SPPCompiler.SemanticAnalysis.ASTs import CoroutinePrototypeAst

        # Log the caller for pin errors.
        is_coroutine = isinstance(function_prototype_ast, CoroutinePrototypeAst)
        is_coroutine_or_async = is_coroutine or is_async
        function_pin_error_ast = is_async if is_async else function_prototype_ast

        # Define the sets of symbols that are borrowed immutable and mutable.
        borrows_ref = OrderedSet()
        borrows_mut = OrderedSet()

        for argument in self.arguments:
            argument.do_semantic_analysis(scope_handler, **kwargs)

            # Check for uninitialized / partially-moved values.
            symbol = scope_handler.current_scope.get_outermost_variable_symbol(argument.value)
            ensure_memory_integrity(self, argument.value, argument, scope_handler, check_move_from_borrowed_context=False, check_pinned_move=False, mark_symbols=False)

            # Enforce the Law of Exclusivity: check the borrow conventions, to ensure there are no overlaps.
            match argument.convention:
                case ConventionMovAst() if symbol:
                    # Mark symbols as moved / partially moved, and check for moving from a borrowed context.
                    ensure_memory_integrity(self, argument.value, argument, scope_handler, check_move=False, check_partial_move=False, check_pinned_move=True)

                    # Cannot move an identifier that overlaps with an existing borrow.
                    for borrow in borrows_ref | borrows_mut:
                        if str(borrow).startswith(str(argument.value)) or str(argument.value).startswith(str(borrow)):
                            how = "immutably" if borrow in borrows_ref else "mutably"
                            raise SemanticErrors.MEMORY_OVERLAP_CONFLICT(borrow, argument.value, f"{how} borrow", "move")

                case ConventionMutAst() if symbol:
                    # Cannot take a mutable borrow from an immutable borrow.
                    if symbol.memory_info.is_borrow_ref:
                        raise SemanticErrors.MEMORY_OVERLAP_CONFLICT(symbol.memory_info.ast_borrow, argument.value, "mutably borrow", "immutably borrow")

                    # Cannot take a mutable borrow from an immutable object.
                    if not (symbol.is_mutable or symbol.memory_info.is_borrow_mut):
                        raise SemanticErrors.MUTABLE_BORROW_FROM_IMMUTABLE_SOURCE(argument, symbol)

                    # Cannot take a mutable borrow that overlaps with other borrows in the function call.
                    for borrow in borrows_ref | borrows_mut:
                        if str(borrow).startswith(str(argument.value)) or str(argument.value).startswith(str(borrow)):
                            how = "immutably" if borrow in borrows_ref else "mutably"
                            raise SemanticErrors.MEMORY_OVERLAP_CONFLICT(borrow, argument.value, f"{how} borrowed", "mutably borrow")

                    # Ensure the argument is pinned if it is borrowed for a coroutine or async function.
                    if is_coroutine_or_async and not any(str(argument.value).startswith(str(pin)) for pin in symbol.memory_info.ast_pins):
                        raise SemanticErrors.UNPINNED_BORROW(argument.value, function_pin_error_ast, is_async)
                    elif is_coroutine_or_async and "assignment" in kwargs:
                        symbol.memory_info.sym_pin_target = scope_handler.current_scope.get_symbol(kwargs["assignment"])

                    # Add the mutable borrow to the set of mutable borrows.
                    borrows_mut.add(argument.value)

                case ConventionRefAst() if symbol:
                    # Cannot take an immutable borrow that overlaps with other mutable borrows in the function call.
                    for borrow in borrows_mut:
                        if str(borrow).startswith(str(argument.value)) or str(argument.value).startswith(str(borrow)):
                            raise SemanticErrors.MEMORY_OVERLAP_CONFLICT(borrow, argument.value, "mutably borrow", f"immutably borrow")

                    # Ensure the argument is pinned if it is borrowed for a coroutine or async function.
                    if is_coroutine_or_async and not any(str(argument.value).startswith(str(pin)) for pin in symbol.memory_info.ast_pins):
                        raise SemanticErrors.UNPINNED_BORROW(argument.value, function_pin_error_ast, is_async)
                    elif is_coroutine_or_async and "assignment" in kwargs:
                        symbol.memory_info.sym_pin_target = scope_handler.current_scope.get_symbol(kwargs["assignment"])

                    # Add the immutable borrow to the set of immutable borrows.
                    borrows_ref.add(argument.value)

    def __copy__(self):
        return FunctionArgumentGroupAst(self.pos, self.paren_l_token, self.arguments.copy(), self.paren_r_token)


__all__ = ["FunctionArgumentGroupAst"]
