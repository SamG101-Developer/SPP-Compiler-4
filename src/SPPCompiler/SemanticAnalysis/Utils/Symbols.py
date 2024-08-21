from __future__ import annotations

import copy, dataclasses
import json
from dataclasses import dataclass
from typing import Optional, List

from SPPCompiler.SemanticAnalysis.ASTs.Meta.Ast import Ast
from SPPCompiler.SemanticAnalysis.Utils.CommonTypes import CommonTypes


@dataclass(kw_only=True)
class MemoryStatus:
    is_borrow_ref: bool = dataclasses.field(default=False)
    is_borrow_mut: bool = dataclasses.field(default=False)

    ast_initialized: Optional[Ast] = dataclasses.field(default=None)
    ast_consumed: Optional[Ast] = dataclasses.field(default=None)
    ast_borrow: Optional[Ast] = dataclasses.field(default=None)
    ast_partial_moves: List[Ast] = dataclasses.field(default_factory=list)
    ast_pins: List[Ast] = dataclasses.field(default_factory=list)

    @property
    def is_borrow(self) -> bool:
        return self.is_borrow_ref or self.is_borrow_mut


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

    def __str__(self) -> str:
        return json.dumps(self)


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

    def __str__(self) -> str:
        return json.dumps(self)


@dataclass(kw_only=True)
class TypeSymbol(Symbol):
    name: GenericIdentifierAst
    type: Optional[ClassPrototypeAst]  # None for generic types
    associated_scope: Optional[Scope] = dataclasses.field(default=None)
    is_generic: bool = dataclasses.field(default=False)

    def __post_init__(self):
        from SPPCompiler.SemanticAnalysis.ASTs import ClassPrototypeAst, GenericIdentifierAst, TypeAst
        assert isinstance(self.name, (GenericIdentifierAst, TypeAst)), f"Got type symbol with name: {self.name} ({type(self.name)}"
        assert isinstance(self.type, ClassPrototypeAst) or self.type is None, f"Got type symbol with type: {type(self.type)}"

    def __json__(self) -> dict:
        return {
            "what": "type",
            "name": self.name,
            "type": self.type,
            "is_generic": self.is_generic,
            "associated_scope": self.associated_scope.name if self.associated_scope else None,
        }

    def __str__(self) -> str:
        return json.dumps(self)

    @property
    def fq_type(self) -> TypeAst:
        from SPPCompiler.SemanticAnalysis.ASTs import TypeAst, IdentifierAst

        if not self.type:
            return self.name

        associated_scope = self.associated_scope.parent
        fq_type = TypeAst(self.name.pos, [], [self.name])
        while associated_scope.parent is not None:
            if isinstance(associated_scope.name, IdentifierAst):
                fq_type.namespace.insert(0, associated_scope.name)
            associated_scope = associated_scope.parent
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

    def __deepcopy__(self, memodict=None):
        # Temporarily remove the "Self" symbol from this table.
        temp_self_symbol = self.get(CommonTypes.self().types[-1])
        if temp_self_symbol:
            temp_self_scope = temp_self_symbol.associated_scope
            temp_self_symbol.associated_scope = None

        # Copy the symbol table, and re-add "Self" to this table.
        new_internal_table = copy.deepcopy(self._internal_table, memodict or {})
        if temp_self_symbol:
            temp_self_symbol.associated_scope = temp_self_scope

        # Wrap the new symbol table and return it.
        new_symbol_table = SymbolTable()
        new_symbol_table._internal_table = new_internal_table
        return new_symbol_table

    def __copy__(self):
        c = self._internal_table.copy()
        s = SymbolTable()
        s._internal_table = c
        return s
