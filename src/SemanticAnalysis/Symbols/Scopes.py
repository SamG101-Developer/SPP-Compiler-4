from __future__ import annotations

import json_fix
from abc import abstractmethod
from typing import Any, Final, Optional

from src.SemanticAnalysis.Symbols.Symbols import SymbolTable, Symbol, TypeSymbol, VariableSymbol


class Scope:
    _scope_name: Any
    _parent_scope: Optional[Scope]
    _children_scopes: list[Scope]
    _symbol_table: SymbolTable[Symbol]

    def __init__(self, name: str, parent_scope: Optional[Scope] = None):
        self._scope_name = name
        self._parent_scope = parent_scope
        self._children_scopes = []
        self._symbol_table = SymbolTable()

    def add_symbol(self, symbol: Symbol) -> None:
        self._symbol_table.add(symbol)

    def get_symbol(self, name: str) -> Optional[Symbol]:
        return self._symbol_table.get(name)

    def set_symbol(self, name: str, symbol: Symbol) -> None:
        self._symbol_table.set(name, symbol)

    def __json__(self) -> dict:
        return {
            "name": self._scope_name,
            "parent_scope": self._parent_scope._scope_name if self._parent_scope else None,
            "children_scopes": [child for child in self._children_scopes],
            "symbol_table": self._symbol_table
        }


class TypeScope(Scope):
    _sup_scopes: list[Scope]
    _symbol_table: SymbolTable[TypeSymbol]

    def __init__(self, name: str, parent_scope: Optional[Scope] = None):
        super().__init__(name, parent_scope)
        self._sup_scopes = []


class StructureScope(Scope):
    _symbol_table: SymbolTable[VariableSymbol]


class ScopeHandler:
    _global_scope: Final[Scope]
    _current_scope: Scope

    def __init__(self):
        self._global_scope = Scope("Global")
        self._current_scope = self._global_scope

    def next_scope(self, name: Any):
        new_scope = Scope(name, self._current_scope)
        self._current_scope._children_scopes.append(new_scope)
        self._current_scope = new_scope

    def prev_scope(self):
        self._current_scope = self._current_scope._parent_scope

    def __iter__(self):
        def iterate(scope: Scope):
            self._current_scope = scope
            yield scope

            for child_scope in scope._children_scopes:
                yield from iterate(child_scope)
            self._current_scope = self.global_scope
        return iterate(self._global_scope)

    @property
    def current_scope(self) -> Scope:
        return self._current_scope

    @property
    def global_scope(self) -> Scope:
        return self._global_scope
