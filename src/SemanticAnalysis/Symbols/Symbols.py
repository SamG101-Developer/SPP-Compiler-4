from __future__ import annotations

from dataclasses import dataclass
from typing import Optional

from src.SemanticAnalysis.ASTs.Ast import Ast


class MemoryStatus:
    is_borrow_ref: bool
    is_borrow_mut: bool

    ast_initialized: Optional[Ast]
    ast_consumed: Optional[Ast]
    ast_borrows: list[Ast]
    ast_partial_borrows: list[Ast]

    def __init__(self):
        self.is_borrow_ref = False
        self.is_borrow_mut = False

        self.ast_initialized = None
        self.ast_consumed = None
        self.ast_borrows = []
        self.ast_partial_borrows = []

    @property
    def is_borrow(self) -> bool:
        return self.is_borrow_ref or self.is_borrow_mut


class Symbol:
    ...


@dataclass
class VariableSymbol(Symbol):
    name: str
    type: TypeSymbol
    mutable: bool
    memory_info: MemoryStatus


@dataclass
class TypeSymbol(Symbol):
    name: str
    sup_type: TypeSymbol


class SymbolTable[SymbolType]:
    _internal_table: dict[str, SymbolType]

    def add(self, symbol: SymbolType) -> None:
        self._internal_table[symbol.name] = symbol

    def get(self, name: str) -> Optional[SymbolType]:
        return self._internal_table.get(name)

    def set(self, name: str, symbol: SymbolType) -> None:
        self._internal_table[name] = symbol