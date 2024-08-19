import hashlib
from dataclasses import dataclass

from SPPCompiler.SemanticAnalysis.ASTs.Meta.Ast import Ast
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstMixins import SemanticAnalyser
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstPrinter import *
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstUtils import TypeInfer, InferredType
from SPPCompiler.SemanticAnalysis.Utils.Scopes import ScopeHandler
from SPPCompiler.SemanticAnalysis.Utils.SemanticError import SemanticErrors
from SPPCompiler.SemanticAnalysis.Utils.Symbols import VariableSymbol, NamespaceSymbol
from SPPCompiler.Utils.Sequence import Seq


@dataclass
class IdentifierAst(Ast, SemanticAnalyser, TypeInfer):
    """
    The IdentifierAst node represents an identifier. This is an identifier for variable, parameters, etc., and will
    start with a lowercase letter.

    Attributes:
        value: The value of the identifier.
    """

    value: str

    def print(self, printer: AstPrinter) -> str:
        # Print the IdentifierAst.
        s = ""
        s += f"{self.value}"
        return s

    def do_semantic_analysis(self, scope_handler: ScopeHandler, **kwargs) -> None:
        # Check the identifier exists in the current or parent scopes.
        if not scope_handler.current_scope.has_symbol(self):

            # Create a list of "similar" symbols, from the list of variable symbols in the current and parent scopes.
            similar = Seq(scope_handler.current_scope.all_symbols()).filter_to_type(VariableSymbol).map(lambda s: s.name.value)

            # Raise an exception if the identifier does not exist in the current or parent scopes, and include the
            # closest match if one exists.
            raise SemanticErrors.UNKNOWN_IDENTIFIER(self, similar.value, "identifier")

    def infer_type(self, scope_handler: ScopeHandler, **kwargs) -> InferredType:
        from SPPCompiler.SemanticAnalysis.ASTs import ConventionRefAst, ConventionMutAst, ConventionMovAst

        # Get the symbol for the identifier.
        symbol = scope_handler.current_scope.get_symbol(self)

        match symbol:
            case VariableSymbol():
                # Determine the convention of the identifier based on the MemoryStatus object tied to the identifier.
                if symbol.memory_info.is_borrow_mut: convention = ConventionMutAst
                elif symbol.memory_info.is_borrow_ref: convention = ConventionRefAst
                else: convention = ConventionMovAst

                # Return the convention and the identifier's type.
                return InferredType(convention=convention, type=symbol.type)

            case NamespaceSymbol():
                # Namespaces are not types, so return the namespace symbol itself.
                return InferredType(convention=ConventionMovAst, type=self)

    def to_generic_identifier(self) -> "GenericIdentifierAst":
        # Convert the identifier to a generic identifier.
        from SPPCompiler.SemanticAnalysis.ASTs import GenericIdentifierAst
        return GenericIdentifierAst(self.pos, self.value, None)

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
