import copy
from dataclasses import dataclass
from typing import List

from SPPCompiler.LexicalAnalysis.Tokens import TokenType
from SPPCompiler.SemanticAnalysis.ASTs.Meta.Ast import Ast
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstMixins import SemanticAnalyser
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstPrinter import *
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstUtils import TypeInfer, InferredType, ensure_memory_integrity
from SPPCompiler.SemanticAnalysis.Utils.CommonTypes import CommonTypes
from SPPCompiler.SemanticAnalysis.Utils.Scopes import ScopeHandler
from SPPCompiler.SemanticAnalysis.Utils.SemanticError import SemanticErrors
from SPPCompiler.SemanticAnalysis.Utils.Symbols import VariableSymbol
from SPPCompiler.Utils.Sequence import Seq


@dataclass
class CaseExpressionAst(Ast, SemanticAnalyser, TypeInfer):
    """
    The IfExpressionAst node represents the unified conditional branching block, combining a standard "if", "match" and
    "?:" into one.

    Attributes:
        case_keyword: The "case" keyword.
        condition: The condition to be evaluated.
        then_keyword: The "then" keyword.
        branches: The pattern blocks to be evaluated.
    """

    case_keyword: "TokenAst"
    condition: "ExpressionAst"
    then_keyword: "TokenAst"
    branches: List["PatternBlockAst"]

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        # Print the IfExpressionAst.
        s = ""
        s += f"{self.case_keyword.print(printer)} "
        s += f"{self.condition.print(printer)}"
        s += f" {self.then_keyword.print(printer)}\n"
        s += f"{Seq(self.branches).print(printer, "\n")}"
        return s

    def do_semantic_analysis(self, scope_handler: ScopeHandler, **kwargs) -> None:
        from SPPCompiler.SemanticAnalysis.ASTs import PatternVariantElseAst, TypeAst, BinaryExpressionAst

        # Move into a new scope.
        scope_handler.into_new_scope("<case-block>")

        # Analyse the condition, and ensure the symbols, if present are memory-integral.
        self.condition.do_semantic_analysis(scope_handler, **kwargs)
        ensure_memory_integrity(self, self.condition, self.condition, scope_handler, mark_symbols=False)

        # Add the condition to the kwargs for pattern-block analysis.
        kwargs |= {"condition": self.condition}

        symbol_memory_status_changes = {}
        for branch in self.branches:
            # Can only use 1 pattern for "is" destructures.
            if branch.comp_operator.token.token_type == TokenType.KwIs and len(branch.patterns) > 1:
                raise SemanticErrors.MULTIPLE_IS_PATTERN_DESTRUCTURE(branch.patterns[0], branch.patterns[1])

            # Make a record of the symbols in the current scope (outside the pattern block).
            this_scope_symbols = Seq(scope_handler.current_scope.all_symbols()).filter_to_type(VariableSymbol)

            # Log the symbols' memory status before and after the branch is analysed.
            this_scope_old_symbol_info = {s: (s.memory_info.ast_consumed, copy.copy(s.memory_info.ast_partial_moves), s.memory_info.ast_initialized, copy.copy(s.memory_info.ast_pins)) for s in this_scope_symbols}
            branch.do_semantic_analysis(scope_handler, **kwargs)
            this_scope_new_symbol_info = {s: (s.memory_info.ast_consumed, copy.copy(s.memory_info.ast_partial_moves), s.memory_info.ast_initialized, copy.copy(s.memory_info.ast_pins)) for s in this_scope_symbols}

            # Log any changes to the memory status of the symbols.
            for symbol, (old_memory_consumed, old_memory_partial_moves, old_memory_initialized, old_pins) in this_scope_old_symbol_info.items():

                # Get the new memory status of the symbol.
                new_memory_consumed, new_memory_partial_moves, new_memory_initialized, new_pins = this_scope_new_symbol_info[symbol]

                # Reset the symbol memory status for the next branch to use as if the previous branch hadn't executed.
                symbol.memory_info.ast_consumed = old_memory_consumed
                symbol.memory_info.ast_partial_moves = old_memory_partial_moves
                symbol.memory_info.ast_initialized = old_memory_initialized
                symbol.memory_info.ast_pins = old_pins

                # Log the symbol states in the dictionary: consistent changes are required for post-case symbol use.
                if symbol in symbol_memory_status_changes:
                    symbol_memory_status_changes[symbol].append((new_memory_initialized, new_pins, new_memory_consumed, new_memory_partial_moves))
                else:
                    symbol_memory_status_changes[symbol] = [(new_memory_initialized, new_pins, new_memory_consumed, new_memory_partial_moves)]

            # Check the else branch is the final branch (this also ensures there is only 1 present)
            if isinstance(branch, PatternVariantElseAst) and branch != self.branches[-1]:
                raise SemanticErrors.ELSE_BRANCH_WRONG_POSITION(branch)

            # Combine the condition with the non-"is" branch patterns to ensure functional compatibility.
            if branch.comp_operator.token.token_type != TokenType.KwIs:
                for pattern in branch.patterns:
                    binary_ast = BinaryExpressionAst(self.condition.pos, self.condition, branch.comp_operator, pattern.expression)
                    binary_ast.do_semantic_analysis(scope_handler, **kwargs)

                    target_type = InferredType.from_type_ast(CommonTypes.bool())
                    return_type = binary_ast.infer_type(scope_handler, **kwargs)
                    if not return_type.symbolic_eq(target_type, scope_handler.current_scope):
                        raise SemanticErrors.CONDITION_NON_BOOLEAN(self, pattern, return_type, "case")

        # Update the initialisation, pins, and consumed state of the symbol: either a new ast or "inconsistent".
        for symbol, changes in symbol_memory_status_changes.items():
            symbol.memory_info.ast_initialized   = "Inconsistent" if any(changes[0][0] != c[0] for c in changes) else changes[0][0]
            symbol.memory_info.ast_pins          = "Inconsistent" if any(changes[0][1] != c[1] for c in changes) else changes[0][1]
            symbol.memory_info.ast_consumed      = "Inconsistent" if any(changes[0][2] != c[2] for c in changes) else changes[0][2]
            symbol.memory_info.ast_partial_moves = "Inconsistent" if any(changes[0][3] != c[3] for c in changes) else changes[0][3]

        # Exit the if-scope.
        scope_handler.exit_cur_scope()

    def infer_type(self, scope_handler, **kwargs) -> InferredType:
        from SPPCompiler.SemanticAnalysis.ASTs import ConventionMovAst, PatternVariantElseAst

        # All the branches must return the same type. Not in standard analysis because it is not always necessary.
        branch_return_types = Seq(self.branches).map(lambda b: b.body.infer_type(scope_handler, **kwargs)).unique_items()
        if branch_return_types.length > 1:
            raise SemanticErrors.CONFLICTING_IF_BRANCH_TYPES(branch_return_types[0], branch_return_types[1])

        # Ensure the final branch is an else branch, in case none of the other patterns match.
        if not isinstance(self.branches[-1].patterns[0], PatternVariantElseAst):
            raise SemanticErrors.NO_ELSE_BRANCH(self.branches[-1])

        # The IfExpressionAst's returning type is any PatternBlockAst's returning type (all blocks will have the same).
        kwargs |= {"condition": self.condition}
        if self.branches and self.branches[0].body.members:
            return self.branches[0].body.members[-1].infer_type(scope_handler, **kwargs)

        # If there are no statements, then every PatternBlockAst is returning the `Void` type.
        return InferredType(convention=ConventionMovAst, type=CommonTypes.void())


__all__ = ["CaseExpressionAst"]
