from __future__ import annotations

import json_fix
from typing import Any, Final, Optional, Iterator, List

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

    def get_symbol(self, name: str | TypeAst) -> Optional[Symbol]:
        return self._symbol_table.get(name, self._parent_scope.get_symbol(name) if self._parent_scope else None)

    def set_symbol(self, name: str | TypeAst, symbol: Symbol) -> None:
        self._symbol_table.set(name, symbol)

    def all_symbols(self) -> List[Symbol]:
        return self._symbol_table.all()

    def __json__(self) -> dict:
        return {
            "what": "scope",
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


class ScopeIterator:
    _iterator: Iterator[Scope]
    _current: Optional[Scope]

    def __init__(self, iterator: Iterator[Scope]):
        self._iterator = iterator
        self._current = None

    def __next__(self):
        self._current = next(self._iterator)
        return self._current

    @property
    def current(self) -> Optional[Scope]:
        return self._current


class ScopeHandler:
    _global_scope: Final[Scope]
    _current_scope: Scope
    _iterator: ScopeIterator

    def __init__(self):
        self._global_scope = Scope("Global")
        self._current_scope = self._global_scope
        self._iterator = iter(self)

    def into_new_scope(self, name: Any):
        new_scope = Scope(name, self._current_scope)
        self._current_scope._children_scopes.append(new_scope)
        self._current_scope = new_scope

    def exit_cur_scope(self):
        self._current_scope = self._current_scope._parent_scope

    def reset(self):
        self._current_scope = self._global_scope
        self._iterator = iter(self)

    def __iter__(self) -> ScopeIterator:
        def iterate(scope: Scope) -> Iterator[Scope]:
            # print("HERE!", self._current_scope._scope_name, [str(c._scope_name) for c in scope._children_scopes])
            for child_scope in scope._children_scopes:
                yield child_scope
                yield from iterate(child_scope)

        return ScopeIterator(iterate(self._global_scope))

    def move_to_next_scope(self) -> Scope:
        current = self.current_scope._scope_name if self._current_scope else "?"
        self._current_scope = next(self._iterator)
        now = self.current_scope._scope_name if self._current_scope else "?"

        print(f"moved from {current} to {now}")

        return self._current_scope

    @property
    def current_scope(self) -> Scope:
        return self._current_scope

    @property
    def global_scope(self) -> Scope:
        return self._global_scope
