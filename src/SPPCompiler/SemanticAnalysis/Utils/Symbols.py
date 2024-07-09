from __future__ import annotations

import copy, dataclasses
from dataclasses import dataclass
from typing import Optional, List

from SPPCompiler.SemanticAnalysis.ASTs.Meta.Ast import Ast


@dataclass(kw_only=True)
class MemoryStatus:
    is_borrow_ref: bool = dataclasses.field(default=False)
    is_borrow_mut: bool = dataclasses.field(default=False)

    ast_initialized: Optional[Ast] = dataclasses.field(default=None)
    ast_consumed: Optional[Ast] = dataclasses.field(default=None)
    ast_borrow: Optional[Ast] = dataclasses.field(default=None)
    ast_partial_moves: List[Ast] = dataclasses.field(default_factory=list)

    @property
    def is_borrow(self) -> bool:
        return self.is_borrow_ref or self.is_borrow_mut

    def as_ast(self):
        from SPPCompiler.SemanticAnalysis.ASTs import ConventionMovAst, ConventionRefAst, ConventionMutAst, TokenAst
        from SPPCompiler.LexicalAnalysis.Tokens import TokenType

        if self.is_borrow_mut:
            return ConventionMutAst(-1, TokenAst.dummy(TokenType.TkLogicalAnd), TokenAst.dummy(TokenType.KwMut))
        elif self.is_borrow_ref:
            return ConventionRefAst(-1, TokenAst.dummy(TokenType.TkLogicalAnd))
        else:
            return ConventionMovAst(-1)


class Symbol:
    ...


@dataclass(kw_only=True)
class NamespaceSymbol(Symbol):
    name: IdentifierAst
    associated_scope: Optional[Scope] = dataclasses.field(default=None)

    def __post_init__(self):
        from SPPCompiler.SemanticAnalysis.ASTs import IdentifierAst
        assert isinstance(self.name, IdentifierAst), f"Got namespace symbol with name: {self.name} ({type(self.name)})"

    def __json__(self) -> dict:
        return {
            "what": "namespace",
            "name": self.name,
        }


@dataclass(kw_only=True)
class VariableSymbol(Symbol):
    name: IdentifierAst
    type: TypeAst
    is_mutable: bool = dataclasses.field(default=False)
    memory_info: MemoryStatus = dataclasses.field(default_factory=MemoryStatus)

    def __post_init__(self):
        from SPPCompiler.SemanticAnalysis.ASTs import IdentifierAst, TypeAst
        assert isinstance(self.name, IdentifierAst), f"Got variable symbol with name: {self.name} ({type(self.name)})"
        assert isinstance(self.type, TypeAst) or self.type is None, f"Got variable symbol with type: {type(self.type)}"

    def __json__(self) -> dict:
        return {
            "what": "variable",
            "name": self.name,
            "type": self.type,
        }


@dataclass(kw_only=True)
class TypeSymbol(Symbol):
    name: TypeAst
    type: Optional[ClassPrototypeAst]  # None for generic types
    associated_scope: Optional[Scope] = dataclasses.field(default=None)

    def __post_init__(self):
        from SPPCompiler.SemanticAnalysis.ASTs import ClassPrototypeAst, TypeAst
        assert isinstance(self.name, TypeAst), f"Got type symbol with name: {self.name} ({type(self.name)}"  # TODO: This is not correct
        assert isinstance(self.type, ClassPrototypeAst) or self.type is None, f"Got type symbol with type: {type(self.type)}"

    def __json__(self) -> dict:
        return {
            "what": "type",
            "name": self.name,
            "type": self.type,
        }

    @property
    def fq_type(self) -> TypeAst:
        if not self.type:
            return self.name

        associated_scope = self.associated_scope._parent_scope
        fq_type = copy.deepcopy(self.name)
        while associated_scope._parent_scope is not None:
            fq_type.parts.insert(0, associated_scope._scope_name)
            associated_scope = associated_scope._parent_scope
        return fq_type


class SymbolTable[SymbolType]:
    _internal_table: dict[str, SymbolType]

    def __init__(self):
        self._internal_table = {}

    def add(self, symbol: SymbolType) -> None:
        self._internal_table[symbol.name] = symbol

    def get(self, name: IdentifierAst | TypeAst, default=None) -> Optional[SymbolType]:
        return self._internal_table[name] if name in self._internal_table else default

    def set(self, name: IdentifierAst | TypeAst, symbol: SymbolType) -> None:
        self._internal_table[name] = symbol

    def all(self) -> List[SymbolType]:
        return [x for x in self._internal_table.values()]

    def __json__(self) -> dict:
        return {
            "symbols": [x for x in self._internal_table.values()]
        }
