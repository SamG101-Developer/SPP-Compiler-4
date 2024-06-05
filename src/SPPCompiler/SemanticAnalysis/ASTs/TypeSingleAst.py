from __future__ import annotations

import copy, difflib, hashlib
from dataclasses import dataclass
from typing import List, Optional

from SPPCompiler.LexicalAnalysis.Tokens import TokenType

from SPPCompiler.SemanticAnalysis.ASTMixins.SemanticAnalyser import SemanticAnalyser
from SPPCompiler.SemanticAnalysis.Utils.SemanticError import SemanticError, SemanticErrorType
from SPPCompiler.SemanticAnalysis.Utils.Scopes import Scope, ScopeHandler
from SPPCompiler.SemanticAnalysis.Utils.Symbols import TypeSymbol, VariableSymbol
from SPPCompiler.SemanticAnalysis.Utils.CommonTypes import CommonTypes

from SPPCompiler.SemanticAnalysis.ASTs.Meta.Ast import Ast
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstPrinter import *
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstUtils import AstUtils

from SPPCompiler.SemanticAnalysis.ASTs.IdentifierAst import IdentifierAst
from SPPCompiler.SemanticAnalysis.ASTs.GenericIdentifierAst import GenericIdentifierAst
from SPPCompiler.SemanticAnalysis.ASTs.GenericArgumentGroupAst import GenericArgumentGroupAst
from SPPCompiler.SemanticAnalysis.ASTs.TokenAst import TokenAst
from SPPCompiler.SemanticAnalysis.ASTs.TypePartAst import TypePartAst

from SPPCompiler.Utils.Sequence import Seq


@dataclass
class TypeSingleAst(Ast, SemanticAnalyser):
    """
    A TypeSingleIdentifier is a single type, ie `std.Vec[T]`. It is made up of a sequence of TypePartAst's, which can be
    either IdentifierAsts (namespace parts), GenericIdentifierAsts (type parts), or TokenAsts (numbers for tuple
    access).

    Attributes:
        - parts: The parts that make up the type identifier.
    """

    parts: List[TypePartAst]

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        # Print the TypeSingleAst.
        s = ""
        s += f"{Seq(self.parts).print(printer, '.')}"
        return s

    def substitute_generics(self, from_ty: TypeSingleAst, to_ty: TypeSingleAst) -> TypeSingleAst:
        # Substitute the generic type "from_ty" with "to_ty" in the type identifier (recursively).
        namespace_parts = Seq(self.parts).filter(lambda p: isinstance(p, IdentifierAst)).value
        self._substitute_generics(from_ty, to_ty)
        return self

    def _substitute_generics(self, from_ty: TypeSingleAst, to_ty: TypeSingleAst) -> TypeSingleAst:
        # Substitute the generic type "from_ty" with "to_ty" in the type identifier (recursively).
        type_parts = [(i, p) for i, p in enumerate(self.parts) if isinstance(p, GenericIdentifierAst)]
        replace_n = len(self.parts) - len(type_parts)

        # Try to substitute the type, i.e. if "from_ty" has been reached.
        if self.without_generics() == from_ty.without_generics():
            self.parts = to_ty.parts

        # Otherwise, iterate the generic arguments and try to substitute the type in each one.
        # TODO: put this "for" in an "else"?
        for i, part in type_parts:
            for g in part.generic_arguments.arguments if part.generic_arguments else []:
                g.type._substitute_generics(from_ty, to_ty)

        # Return the modified type (self).
        return self

    def do_semantic_analysis(self, scope_handler: ScopeHandler, verify_generics: bool = True, **kwargs) -> None:
        ...

    def __iter__(self):
        # Iterate the parts, and recursively the parts of generic parameters
        def iterate(type_single: TypeSingleAst):
            namespace_parts = Seq(type_single.parts).filter(lambda p: isinstance(p, IdentifierAst)).value
            non_namespace_parts = Seq(type_single.parts).filter(lambda p: not isinstance(p, IdentifierAst)).value

            for part in non_namespace_parts:
                yield TypeSingleAst(part.pos, [*namespace_parts, part])
                for g in part.generic_arguments.arguments:
                    yield from iterate(g.type)

        return iterate(self)

    def without_generics(self) -> TypeSingleAst:
        parts = []
        for part in self.parts:
            parts.append(GenericIdentifierAst(part.pos, part.value, GenericArgumentGroupAst(part.pos, TokenAst.dummy(TokenType.TkBrackL), [], TokenAst.dummy(TokenType.TkBrackR))) if isinstance(part, GenericIdentifierAst) else part)
        return TypeSingleAst(self.pos, parts)

    def __eq__(self, that):
        # Check both ASTs are the same type and have the same parts.
        return isinstance(that, TypeSingleAst) and self.parts == that.parts

    def symbolic_eq(self, that, this_scope: Scope, that_scope: Optional[Scope] = None) -> bool:
        # Special cases for union types.
        if that.without_generics() == CommonTypes.var([]):
            for generic_argument in that.parts[-1].generic_arguments.arguments:
                if generic_argument.type.symbolic_eq(self, this_scope, that_scope):
                    return True

        # Allows for generics and aliases to match base types etc.
        that_scope = that_scope or this_scope
        this_type = this_scope.get_symbol(self).type
        that_type = that_scope.get_symbol(that).type
        return this_type == that_type

    def __hash__(self):
        return int.from_bytes(hashlib.md5("".join([str(p) for p in self.parts]).encode()).digest())

    def __json__(self) -> str:
        printer = AstPrinter()
        return self.print(printer)


__all__ = ["TypeSingleAst"]
