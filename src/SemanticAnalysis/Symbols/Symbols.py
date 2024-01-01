from __future__ import annotations

import dataclasses
from dataclasses import dataclass
from typing import Optional
import json_fix


class MemoryStatus:
    is_borrow_ref: bool
    is_borrow_mut: bool

    ast_initialized: Optional[Ast]
    ast_consumed: Optional[Ast]
    ast_borrows: list[Ast]
    ast_partial_moves: list[Ast]

    def __init__(self):
        self.is_borrow_ref = False
        self.is_borrow_mut = False

        self.ast_initialized = None
        self.ast_consumed = None
        self.ast_borrows = []
        self.ast_partial_moves = []

    @property
    def is_borrow(self) -> bool:
        return self.is_borrow_ref or self.is_borrow_mut


class Symbol:
    ...


@dataclass
class VariableSymbol(Symbol):
    name: IdentifierAst
    type: TypeAst
    mutable: bool = dataclasses.field(default=False)
    memory_info: MemoryStatus = dataclasses.field(default_factory=MemoryStatus)

    def __json__(self) -> dict:
        return {
            "name": self.name,
            "type": self.type,
        }


@dataclass
class TypeSymbol(Symbol):
    name: IdentifierAst
    type: TypeAst

    def __json__(self) -> dict:
        return {
            "name": self.name,
            "type": self.type,
        }


class SymbolTable[SymbolType]:
    _internal_table: dict[str, SymbolType]

    def __init__(self):
        self._internal_table = {}

    def add(self, symbol: SymbolType) -> None:
        self._internal_table[symbol.name] = symbol

    def get(self, name: str) -> Optional[SymbolType]:
        return self._internal_table.get(name)

    def set(self, name: str, symbol: SymbolType) -> None:
        self._internal_table[name] = symbol

    def __json__(self) -> dict:
        return {
            "symbols": [x for x in self._internal_table.values()]
        }
