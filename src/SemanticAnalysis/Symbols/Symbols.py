from __future__ import annotations

import dataclasses
from dataclasses import dataclass
from typing import Optional, List
import json_fix


@dataclass
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


class Symbol:
    ...


@dataclass
class VariableSymbol(Symbol):
    name: IdentifierAst
    type: TypeAst
    is_mutable: bool = dataclasses.field(default=False)
    memory_info: MemoryStatus = dataclasses.field(default_factory=MemoryStatus)

    def __post_init__(self):
        from src.SemanticAnalysis.ASTs.Ast import IdentifierAst, TypeAst
        assert isinstance(self.name, IdentifierAst), f"Got variable symbol with name: {self.name}"
        assert type(self.type) in TypeAst.__args__ or self.type is None, f"Got variable symbol with type: {self.type}"  # TODO ; shouldn't be None

    def __json__(self) -> dict:
        return {
            "what": "variable",
            "name": self.name,
            "type": self.type,
        }


@dataclass
class TypeSymbol(Symbol):
    name: TypeAst
    type: Optional[ClassPrototypeAst]  # None for generic types
    associated_scope: Optional[Scope] = dataclasses.field(default=None)

    def __post_init__(self):
        from src.SemanticAnalysis.ASTs.Ast import ClassPrototypeAst, TypeAst
        assert type(self.name) in TypeAst.__args__
        assert isinstance(self.type, ClassPrototypeAst) or self.type is None

    def __json__(self) -> dict:
        return {
            "what": "type",
            "name": self.name,
            "type": self.type,
        }


class SymbolTable[SymbolType]:
    _internal_table: dict[str, SymbolType]

    def __init__(self):
        self._internal_table = {}

    def add(self, symbol: SymbolType) -> None:
        self._internal_table[symbol.name] = symbol

    def get(self, name: IdentifierAst | TypeAst, default=None) -> Optional[SymbolType]:
        return self._internal_table.get(name, default)

    def set(self, name: IdentifierAst | TypeAst, symbol: SymbolType) -> None:
        self._internal_table[name] = symbol

    def all(self) -> List[SymbolType]:
        return [x for x in self._internal_table.values()]

    def __json__(self) -> dict:
        return {
            "symbols": [x for x in self._internal_table.values()]
        }
