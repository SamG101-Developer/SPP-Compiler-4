from __future__ import annotations

from typing import Final, Optional

from src.SemanticAnalysis.Symbols.Symbols import SymbolTable, Symbol, TypeSymbol, VariableSymbol


class Scope:
    _parent_scope: Optional[Scope]
    _children_scopes: list[Scope]
    _symbol_table: SymbolTable[Symbol]

    def __init__(self, parent_scope: Optional[Scope] = None):
        self._parent_scope = parent_scope
        self._children_scopes = []
        self._symbol_table = SymbolTable()


class TypeScope(Scope):
    _sup_scopes: list[Scope]
    _symbol_table: SymbolTable[TypeSymbol]


class StructureScope(Scope):
    _symbol_table: SymbolTable[VariableSymbol]


class ScopeHandler:
    _global_scope: Final[Scope]
    _current_scope: Scope

    def __init__(self):
        self._global_scope = Scope()
        self._current_scope = self._global_scope

    def next_scope(self):
        new_scope = Scope(self._current_scope)
        self._current_scope._children_scopes.append(new_scope)
        self._current_scope = new_scope

    def prev_scope(self):
        self._current_scope = self._current_scope._parent_scope

    def __iter__(self):
        def iterate(scope: Scope):
            yield scope
            for child_scope in scope._children_scopes:
                yield from iterate(child_scope)

    @property
    def current_scope(self) -> Scope:
        return self._current_scope

    @property
    def global_scope(self) -> Scope:
        return self._global_scope
