from __future__ import annotations
import difflib, hashlib
from dataclasses import dataclass

from SPPCompiler.SemanticAnalysis.ASTMixins.SemanticAnalyser import SemanticAnalyser
from SPPCompiler.SemanticAnalysis.ASTMixins.TypeInfer import TypeInfer, InferredType
from SPPCompiler.SemanticAnalysis.ASTs.Meta.Ast import Ast
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstPrinter import *
from SPPCompiler.SemanticAnalysis.Utils.SemanticError import SemanticErrors
from SPPCompiler.SemanticAnalysis.Utils.Scopes import ScopeHandler
from SPPCompiler.SemanticAnalysis.Utils.Symbols import VariableSymbol
from SPPCompiler.Utils.Sequence import Seq


@dataclass
class IdentifierAst(Ast, SemanticAnalyser, TypeInfer):
    """
    The IdentifierAst node represents an identifier. This is an identifier for variable, parameters, etc., and will
    start with a lowercase letter.

    Attributes:
        value: The value of the generic identifier.
    """

    value: str

    def print(self, printer: AstPrinter) -> str:
        # Print the IdentifierAst.
        s = ""
        s += f"{self.value}"
        return s

    def do_semantic_analysis(self, scope_handler: ScopeHandler, **kwargs) -> None:
        # Check that the identifier exists in the current or parent scopes.
        if not scope_handler.current_scope.has_symbol(self):

            # Create a list of "similar" symbols, from the list of variable symbols in the current and parent scopes.
            similar = Seq(scope_handler.current_scope.all_symbols()).filter_to_type(VariableSymbol).map(lambda s: s.name.value)

            # Raise an exception if the identifier does not exist in the current or parent scopes, and include the
            # closest match if one exists.
            raise SemanticErrors.UNKNOWN_IDENTIFIER(self, similar.value, "identifier")

    def infer_type(self, scope_handler: ScopeHandler, **kwargs) -> InferredType:
        from SPPCompiler.SemanticAnalysis.ASTs import ConventionRefAst, ConventionMutAst, ConventionMovAst

        # Get the symbol for the identifier.
        sym = scope_handler.current_scope.get_symbol(self)

        # Determine the convention of the identifier based on the MemoryStatus object tied to the identifier.
        if sym.memory_info.is_borrow_mut: convention = ConventionMutAst
        elif sym.memory_info.is_borrow_ref: convention = ConventionRefAst
        else: convention = ConventionMovAst

        # Return the convention and the type of the identifier.

        print("III", self, sym.type)

        return InferredType(
            convention=convention,
            type_symbol=scope_handler.current_scope.get_symbol(sym.type))

    def __eq__(self, other):
        # Check both ASTs are the same type and have the same value.
        return isinstance(other, IdentifierAst) and self.value == other.value

    def __hash__(self):
        # The hash must be fixed, so that the same identifiers in different IdentifierAst objects have the same hash.
        return int.from_bytes(hashlib.md5(self.value.encode()).digest())

    def __radd__(self, other):
        # Simplify prepending strings to an identifier.
        if isinstance(other, str):
            return IdentifierAst(pos=self.pos, value=other + self.value)
        elif isinstance(other, IdentifierAst):
            return IdentifierAst(pos=self.pos, value=other.value + self.value)

    def __add__(self, other):
        # Simplify appending strings to an identifier.
        if isinstance(other, str):
            return IdentifierAst(pos=self.pos, value=self.value + other)
        elif isinstance(other, IdentifierAst):
            return IdentifierAst(pos=self.pos, value=self.value + other.value)

    def __json__(self) -> str:
        # Return the value of the identifier.
        return self.value


__all__ = ["IdentifierAst"]
