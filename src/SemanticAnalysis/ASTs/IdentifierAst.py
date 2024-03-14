from __future__ import annotations
import difflib, hashlib
from dataclasses import dataclass
from typing import Tuple, Type

from src.SemanticAnalysis.Analysis.SemanticAnalysis import SemanticAnalysis
from src.SemanticAnalysis.Analysis.SemanticError import SemanticError
from src.SemanticAnalysis.Types.TypeInfer import TypeInfer
from src.SemanticAnalysis.Symbols.Scopes import ScopeHandler
from src.SemanticAnalysis.Symbols.Symbols import VariableSymbol

from src.SemanticAnalysis.ASTs.Meta.Ast import Ast
from src.SemanticAnalysis.ASTs.Meta.AstPrinter import *

from src.SemanticAnalysis.ASTs.ConventionRefAst import ConventionRefAst
from src.SemanticAnalysis.ASTs.ConventionMovAst import ConventionMovAst
from src.SemanticAnalysis.ASTs.ConventionMutAst import ConventionMutAst
from src.SemanticAnalysis.ASTs.ConventionPartInitAst import ConventionPartInitAst
from src.SemanticAnalysis.ASTs.ConventionNonInitAst import ConventionNonInitAst

from src.Utils.Sequence import Seq


@dataclass
class IdentifierAst(Ast, SemanticAnalysis, TypeInfer):
    """
    The IdentifierAst node represents an identifier. This is an identifier for variable, parameters, etc., and will
    start with a lowercase letter.

    Attributes:
        - value: The value of the generic identifier.
    """

    value: str

    def print(self, printer: AstPrinter) -> str:
        # Print the IdentifierAst.
        s = ""
        s += f"{self.value}"
        return s

    def do_semantic_analysis(self, scope_handler: ScopeHandler, **kwargs) -> None:
        # Check if this identifier exists in the current scope.
        if not scope_handler.current_scope.has_symbol(self):

            # Get a list of all the variable symbols in the current scope (and parent scopes), and determine the closest
            # match to the identifier.
            all_symbols = Seq(scope_handler.current_scope.all_symbols()).filter(lambda s: isinstance(s, VariableSymbol))
            closest_match = difflib.get_close_matches(self.value, all_symbols.map(lambda s: s.name.value).value, n=1)
            closest_match = f" Did you mean '{closest_match[0]}'?" if closest_match else ""

            # Raise an error if the identifier does not exist in the current scope, including the closest match if one
            # was found.
            exception = SemanticError(f"Undefined identifier '{self.value}':")
            exception.add_traceback(self.pos, f"Identifier '{self.value}' used here.{closest_match}")
            raise exception

        # Do semantic analysis on the identifiers type. TODO: why?
        scope_handler.current_scope.get_symbol(self).type.do_semantic_analysis(scope_handler, **kwargs)

    def infer_type(self, scope_handler: ScopeHandler, **kwargs) -> Tuple[Type[ConventionAst], TypeAst]:
        # Ge the symbol for the identifier.
        sym = scope_handler.current_scope.get_symbol(self)

        # Determine the convention of the identifier based on the MemoryStatus object tied to the identifier.
        if sym.memory_info.ast_consumed:
            convention = ConventionNonInitAst
        elif sym.memory_info.ast_partial_moves:
            convention = ConventionPartInitAst
        elif sym.memory_info.is_borrow_mut:
            convention = ConventionMutAst
        elif sym.memory_info.is_borrow_ref:
            convention = ConventionRefAst
        else:
            convention = ConventionMovAst

        # Return the convention and the type of the identifier.
        return convention, sym.type

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
