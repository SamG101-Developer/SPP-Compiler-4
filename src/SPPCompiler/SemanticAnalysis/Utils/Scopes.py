from __future__ import annotations

import copy
from typing import Any, Final, Optional, Iterator, List, Tuple

from SPPCompiler.SemanticAnalysis.Utils.Symbols import SymbolTable, TypeSymbol, VariableSymbol, NamespaceSymbol
from SPPCompiler.Utils.Sequence import Seq


class Scope:
    _scope_name: Any
    _parent_scope: Optional[Scope]
    _children_scopes: List[Scope]
    _symbol_table: SymbolTable[TypeSymbol | VariableSymbol]
    _sup_scopes: List[Tuple[Scope, SupPrototypeInheritanceAst]]

    def __init__(self, name: str, parent_scope: Optional[Scope] = None):
        # Set the attributes to the parameters or default values.
        self._scope_name = name
        self._parent_scope = parent_scope
        self._children_scopes = []
        self._symbol_table = SymbolTable()
        self._sup_scopes = []

    def add_symbol(self, symbol: TypeSymbol | VariableSymbol | NamespaceSymbol) -> TypeSymbol | VariableSymbol:
        # For TypeAst, shift the scope if a namespaced type is being added.
        from SPPCompiler.SemanticAnalysis.ASTs import TypeAst
        scope = self
        if isinstance(symbol.name, TypeAst):
            name = copy.deepcopy(symbol.name)
            namespace = name.parts[:-1].copy()

            for part in namespace.copy():
                if Seq(scope._children_scopes).map(lambda s: s._scope_name).contains(part):
                    scope = Seq(scope._children_scopes).filter(lambda s: s._scope_name == part).first()
                    namespace.pop(0)
                else:
                    break
            name.parts[:-1] = namespace
            symbol.name = name

        # Add a symbol to the symbol table.
        scope._symbol_table.add(symbol)
        return symbol

    def get_outermost_variable_symbol(self, name: IdentifierAst | PostfixExpressionAst) -> Optional[VariableSymbol]:
        from SPPCompiler.SemanticAnalysis.ASTs import IdentifierAst, PostfixExpressionAst, PostfixExpressionOperatorMemberAccessAst

        match name:
            case IdentifierAst():
                identifier = name
            case PostfixExpressionAst():
                while isinstance(name, PostfixExpressionAst) and isinstance(name.op, PostfixExpressionOperatorMemberAccessAst): name = name.lhs
                identifier = name
            case _:
                return None

        return self.get_symbol(identifier) if isinstance(identifier, IdentifierAst) else None

    def get_symbol(self, name: IdentifierAst | TypeAst, exclusive: bool = False) -> Optional[TypeSymbol | VariableSymbol]:
        # Ensure that the name is an IdentifierAst or TypeAst, to get a VariableSymbol or a TypeSymbol respectively.
        from SPPCompiler.SemanticAnalysis.ASTs import IdentifierAst, TypeAst
        assert type(name) in [IdentifierAst, TypeAst], f"Expected IdentifierAst or TypeAst, got {type(name)}"
        scope = self

        # For TypeAsts, shift the scope if a namespaced type is being accessed.
        if isinstance(name, TypeAst):
            name = copy.deepcopy(name)
            namespace = name.parts[:-1].copy()

            # For each part in the namespace, enter the child scope if it exists.
            for part in namespace.copy():
                if Seq(scope._children_scopes).map(lambda s: s._scope_name).contains(part):
                    scope = Seq(scope._children_scopes).filter(lambda s: s._scope_name == part).first()
                    namespace.pop(0)
                else:
                    break

            name.parts[:-1] = namespace

        # Get the symbol from the symbol table. If it doesn't exist, then get the symbol from the parent scope. This is
        # done recursively until the symbol is found, or the global scope is reached.
        sym = scope._symbol_table.get(name)

        if not sym and self._parent_scope and not exclusive:
            sym = self._parent_scope.get_symbol(name)
        if sym:
            return sym

        # If the parent scopes don't contain the symbol, then check the sup-scopes. Sup scopes only exist for
        # TypeSymbols, as they contain the scopes of other types that are superimposed over this type, and therefore
        # contain inherited symbols.
        if isinstance(name, IdentifierAst) or isinstance(name, TypeAst) and name.parts[-1].value.startswith("MOCK_"):
            for sup_scope, _ in self._sup_scopes:
                sym = sup_scope.get_symbol(name)
                if sym:
                    return sym

    def get_all_symbols(self, name: TypeAst) -> List[VariableSymbol]:
        from SPPCompiler.SemanticAnalysis.ASTs import TypeAst
        assert isinstance(name, TypeAst)
        scope = self

        syms = [scope._symbol_table.get(name)]
        for sup_scope, _ in scope._sup_scopes:
            syms.extend(sup_scope.get_all_symbols(name))

        while None in syms:
            syms.remove(None)

        return syms

    def has_symbol(self, name: IdentifierAst | TypeAst, exclusive: bool = False) -> bool:
        return self.get_symbol(name, exclusive) is not None

    def set_symbol(self, name: IdentifierAst | TypeAst, symbol: TypeSymbol | VariableSymbol) -> None:
        from SPPCompiler.SemanticAnalysis.ASTs import IdentifierAst, TypeAst
        assert isinstance(name, IdentifierAst) or type(symbol) in TypeAst.__value__.__args__
        self._symbol_table.set(name, symbol)

    def all_symbols(self, exclusive: bool = False) -> List[TypeSymbol | VariableSymbol]:
        return self._symbol_table.all() + (self._parent_scope.all_symbols() if self._parent_scope and not exclusive else [])

    def __json__(self) -> dict:
        return {
            "what": "scope",
            "name": self._scope_name,
            "parent_scope": self._parent_scope._scope_name if self._parent_scope else None,
            "children_scopes": [child for child in self._children_scopes],
            "sup_scopes": [sup._scope_name for sup, _ in self._sup_scopes],
            "symbol_table": self._symbol_table
        }

    @property
    def sup_scopes(self) -> List[Tuple[Scope, SupPrototypeInheritanceAst]]:
        # The sup scopes are a tree of scopes however, due to inheritance, say C inherits A and B, and B inherits A,
        # then the sup scopes must be B and B'a A, so use a set to make sure that the 2nd A isn't added too.
        all_sup_scopes = []
        scopes_read = []
        for sup_scope, ast in self._sup_scopes:
            if sup_scope in scopes_read: continue
            all_sup_scopes.append((sup_scope, ast))
            all_sup_scopes.extend(sup_scope.sup_scopes)
            scopes_read.append(sup_scope)

        return all_sup_scopes

    @property
    def name(self) -> Any:
        return self._scope_name

    @property
    def parent(self) -> Optional[Scope]:
        return self._parent_scope

    @property
    def children(self) -> List[Scope]:
        return self._children_scopes

    @property
    def exclusive_sup_scopes(self) -> List[Tuple[Scope, SupPrototypeInheritanceAst]]:
        return self._sup_scopes

    @property
    def scopes_as_namespace(self) -> List[IdentifierAst]:
        scope = self._parent_scope
        namespace = []
        while scope._parent_scope:
            namespace.insert(0, scope._scope_name)
            scope = scope._parent_scope
        return namespace


class ScopeIterator:
    _iterator: Iterator[Scope]
    _current: Optional[Scope]
    _consumed: List[Scope]

    def __init__(self, iterator: Iterator[Scope]):
        self._iterator = iterator
        self._current = None
        self._consumed = []

    def __next__(self):
        self._consumed.append(self._current)
        self._current = next(self._iterator)
        return self._current

    def __reversed__(self):
        return reversed(self._consumed)

    @property
    def current(self) -> Optional[Scope]:
        return self._current


class ScopeHandler:
    _global_scope: Final[Scope]
    _current_scope: Scope
    _iterator: ScopeIterator

    def __init__(self, global_scope: Optional[Scope] = None):
        from SPPCompiler.SemanticAnalysis.ASTs import GenericIdentifierAst, TypeAst

        self._global_scope = global_scope or Scope("Global")
        self._current_scope = self._global_scope
        self._iterator = iter(self)

        global_type = TypeAst(pos=-1, parts=[GenericIdentifierAst(pos=-1, value="GLOBAL", generic_arguments=None)])
        global_symbol = TypeSymbol(name=global_type, type=None, associated_scope=self._global_scope)
        self._global_scope.add_symbol(global_symbol)

    def into_new_scope(self, name: Any) -> Scope:
        new_scope = Scope(name, self._current_scope)
        self._current_scope._children_scopes.append(new_scope)
        self._current_scope = new_scope
        next(self._iterator)
        return new_scope

    def exit_cur_scope(self):
        self._current_scope = self._current_scope._parent_scope

    def reset(self, scope: Optional[Scope] = None) -> None:
        self._current_scope = scope or self._global_scope
        self._iterator = iter(self)

    def __iter__(self) -> ScopeIterator:
        def iterate(scope: Scope) -> Iterator[Scope]:
            for child_scope in scope._children_scopes:
                yield child_scope
                yield from iterate(child_scope)

        return ScopeIterator(iterate(self._current_scope))

    def move_to_next_scope(self) -> Scope:
        self._current_scope = next(self._iterator)
        return self._current_scope

    def at_global_scope(self, parent_level: int = 0) -> bool:
        scope_to_check = self.current_scope
        for _ in range(parent_level):
            scope_to_check = scope_to_check._parent_scope
        return scope_to_check == self._global_scope

    def get_namespaced_scope(self, namespace: list["IdentifierAst"]) -> Optional[Scope]:
        scope = self._global_scope
        for part in namespace:
            if Seq(scope._children_scopes).map(lambda s: s._scope_name).contains(part):
                scope = Seq(scope._children_scopes).filter(lambda s: s._scope_name == part).first()
            else:
                return None
        return scope

    @property
    def current_scope(self) -> Scope:
        return self._current_scope

    @current_scope.setter
    def current_scope(self, scope: Scope) -> None:
        self._current_scope = scope

    @property
    def global_scope(self) -> Scope:
        return self._global_scope
