from __future__ import annotations


class Symbol:
    ...


class VariableSymbol(Symbol):
    _name: str
    _type: TypeSymbol


class TypeSymbol(Symbol):
    _name: str
    _sup_type: TypeSymbol


class Scope:
    _parent_scope: Scope
    _children_scopes: list[Scope]
    _symbol_table: dict[str, Symbol]


class TypeScope(Scope):
    _sup_scopes: list[Scope]


class StructureScope(Scope):
    ...


class ScopeHandler:
    _global_scope: Scope
    _current_scope: Scope
