from __future__ import annotations

from typing import Any, Final, Optional, Iterator, List, Tuple

from SPPCompiler.LexicalAnalysis.Tokens import TokenType
from SPPCompiler.SemanticAnalysis.Utils.Symbols import SymbolTable, TypeSymbol, VariableSymbol, NamespaceSymbol, \
    TypeAliasSymbol, Symbol
from SPPCompiler.Utils.Sequence import Seq


class Scope:
    _scope_name: Any
    _parent_scope: Optional[Scope]
    _children_scopes: List[Scope]
    _symbol_table: SymbolTable[TypeSymbol | VariableSymbol]
    _sup_scopes: List[Tuple[Scope, SupPrototypeNormalAst | SupPrototypeInheritanceAst]]
    _associated_type_symbol: Optional[TypeSymbol]

    def __init__(self, name: Any, parent_scope: Optional[Scope] = None):

        # Set the attributes to the parameters or default values.
        self._scope_name = name
        self._parent_scope = parent_scope
        self._children_scopes = []
        self._symbol_table = SymbolTable()
        self._sup_scopes = []
        self._associated_type_symbol = None

    def __confirm_symbol(self, symbol: Symbol, ignore_alias: bool) -> Symbol:
        # return symbol
        match symbol:
            case TypeAliasSymbol() if symbol.old_type and not ignore_alias: return self.get_symbol(symbol.old_type)
            case _: return symbol

    def add_symbol(self, symbol: TypeSymbol | VariableSymbol | NamespaceSymbol) -> TypeSymbol | VariableSymbol:
        from SPPCompiler.SemanticAnalysis.ASTs import TypeAst
        from SPPCompiler.SemanticAnalysis.Utils.SemanticError import SemanticErrors

        scope = self

        # Ensure types don't get redefined (except for generics).
        if isinstance(symbol, TypeSymbol) and self.has_symbol(symbol.name) and symbol.name.value != "Self":
            old_symbol = self.get_symbol(symbol.name)
            if not (old_symbol.is_generic and not old_symbol.associated_scope):
                raise SemanticErrors.REDEFINED_TYPE(self.get_symbol(symbol.name, ignore_alias=True).name, symbol.name)

        # For TypeAst, shift the scope if a namespaced type is being added.
        if isinstance(symbol.name, TypeAst):
            for part in symbol.name.namespace + symbol.name.types[:-1]:
                inner_symbol = scope.get_symbol(part)
                match inner_symbol:
                    case None: break
                    case _: scope = inner_symbol.associated_scope
            symbol.name = symbol.name.types[-1]

        # Add a symbol to the symbol table.
        scope._symbol_table.add(symbol)
        return symbol

    def get_symbol(self, name: IdentifierAst | GenericIdentifierAst | TypeAst, exclusive: bool = False, ignore_alias: bool = False) -> Optional[TypeSymbol | VariableSymbol]:
        # Ensure that the name is an IdentifierAst or TypeAst, to get a VariableSymbol or a TypeSymbol respectively.
        from SPPCompiler.SemanticAnalysis.ASTs import IdentifierAst, GenericIdentifierAst, TypeAst
        if not isinstance(name, (IdentifierAst, GenericIdentifierAst, TypeAst)): return None
        scope = self

        # For TypeAsts, shift the scope if a namespaced type is being accessed.
        if isinstance(name, TypeAst):
            for part in name.namespace + name.types[:-1]:
                inner_symbol = scope.get_symbol(part)
                match inner_symbol:
                    case None: break
                    case _: scope = inner_symbol.associated_scope
            name = name.types[-1]

        # Get the symbol from the symbol table. If it doesn't exist, then get the symbol from the parent scope. This is
        # done recursively until the symbol is found, or the global scope is reached.
        symbol = scope._symbol_table.get(name)

        if symbol:
            return self.__confirm_symbol(symbol, ignore_alias)
        if self._parent_scope and not exclusive:
            symbol = self._parent_scope.get_symbol(name, exclusive, ignore_alias)
            if symbol:
                return self.__confirm_symbol(symbol, ignore_alias)

        # If the parent scopes don't contain the symbol, then check the sup-scopes. Sup scopes only exist for
        # TypeSymbols, as they contain the scopes of other types that are superimposed over this type, and therefore
        # contain inherited symbols.
        if isinstance(name, IdentifierAst) or isinstance(name, GenericIdentifierAst) and name.value.startswith("MOCK_"):
            for sup_scope, _ in self._sup_scopes:
                symbol = sup_scope.get_symbol(name, exclusive, ignore_alias)
                if symbol:
                    return self.__confirm_symbol(symbol, ignore_alias)

    def _get_multiple_symbols(self, name: IdentifierAst, original_scope: Scope) -> Seq[Tuple[VariableSymbol, Scope, int]]:
        symbols = Seq([(self._symbol_table.get(name), self, original_scope.depth_to(self))])
        for sup_scope, _ in self._sup_scopes:
            symbols.extend(sup_scope._get_multiple_symbols(name, original_scope))
        for s, c, d in symbols.copy():
            if not s: symbols.remove((s, c, d))
        return symbols

    def get_multiple_symbols(self, name: IdentifierAst) -> Seq[Tuple[VariableSymbol, Scope, int]]:
        return self._get_multiple_symbols(name, self)

    def get_outermost_variable_symbol(self, name: IdentifierAst | PostfixExpressionAst) -> Optional[VariableSymbol]:
        from SPPCompiler.SemanticAnalysis.ASTs import IdentifierAst, PostfixExpressionAst, PostfixExpressionOperatorMemberAccessAst

        # Match the name against a set of ASTs to get the identifier.
        match name:

            # For an IdentifierAst, the name is already the outermost identifier.
            case IdentifierAst():
                identifier = name

            # For a PostfixExpressionAst, the outermost symbol is reached by recursively getting the lhs.
            case PostfixExpressionAst():
                while isinstance(name, PostfixExpressionAst) and isinstance(name.op, PostfixExpressionOperatorMemberAccessAst) and name.op.dot_token.token.token_type == TokenType.TkDot:
                    name = name.lhs
                identifier = name

            # Otherwise, there is no identifier.
            case _:
                return None

        # For namespaced constants, like "std::none", shift the scope to the namespace.
        if isinstance(identifier, PostfixExpressionAst) and isinstance(identifier.op, PostfixExpressionOperatorMemberAccessAst):
            scope, rhs = self, identifier.op.identifier
            while isinstance(identifier, PostfixExpressionAst) and isinstance(identifier.op, PostfixExpressionOperatorMemberAccessAst):
                scope = scope.get_symbol(identifier.lhs).associated_scope
                identifier = identifier.lhs
            symbol = scope.get_symbol(rhs)

        # For identifiers, get the symbol from the symbol table.
        elif isinstance(identifier, IdentifierAst):
            symbol = self.get_symbol(identifier)

        # Otherwise, this is a symbol-less expression.
        else:
            symbol = None

        return symbol

    def has_symbol(self, name: IdentifierAst | TypeAst, exclusive: bool = False, ignore_alias: bool = False) -> bool:
        return self.get_symbol(name, exclusive, ignore_alias) is not None

    def all_symbols(self, exclusive: bool = False) -> List[TypeSymbol | VariableSymbol]:
        return self._symbol_table.all() + (self._parent_scope.all_symbols() if self._parent_scope and not exclusive else [])

    def rem_symbol(self, symbol: TypeSymbol | VariableSymbol, exclusive: bool = False) -> None:
        scope = self
        symbol_found = symbol in scope._symbol_table._internal_table.values()

        if symbol_found:
            return scope._symbol_table.rem(symbol)
        if self._parent_scope and not exclusive:
            self._parent_scope.rem_symbol(symbol, exclusive)

    def __json__(self) -> dict:
        return {
            "what": "scope",
            "name": self._scope_name,
            "parent_scope": self._parent_scope._scope_name if self._parent_scope else None,
            "children_scopes": [child for child in self._children_scopes],
            "sup_scopes": [sup.name for sup, _ in self._sup_scopes],
            "symbol_table": self._symbol_table
        }

    def depth_to(self, that_scope: Scope) -> int:
        def _depth_to(scope: Scope, target: Scope, depth: int) -> int:
            if scope is target:
                return depth
            for child in scope._sup_scopes:
                result = _depth_to(child[0], target, depth + 1)
                if result:
                    return result
            return 0
        return _depth_to(self, that_scope, 0)

    @property
    def sup_scopes(self) -> List[Tuple[Scope, SupPrototypeInheritanceAst]]:
        all_sup_scopes = []
        for sup_scope, ast in self._sup_scopes:
            all_sup_scopes.append((sup_scope, ast))
            all_sup_scopes.extend(sup_scope.sup_scopes)
        return all_sup_scopes

    @property
    def name(self) -> Any:
        return self._scope_name

    @property
    def parent(self) -> Optional[Scope]:
        return self._parent_scope

    @property
    def parent_module(self) -> Scope:
        from SPPCompiler.SemanticAnalysis.ASTs import IdentifierAst
        scope = self
        while not isinstance(scope.name, IdentifierAst):
            scope = scope.parent
        return scope

    @property
    def children(self) -> List[Scope]:
        return self._children_scopes

    @property
    def scopes_as_namespace(self) -> List["IdentifierAst"]:
        from SPPCompiler.SemanticAnalysis.ASTs import IdentifierAst

        scope = self._parent_scope
        namespace = []
        while scope.parent:
            if isinstance(scope.name, IdentifierAst):
                namespace.insert(0, scope.name)
            scope = scope.parent
        return namespace

    @property
    def associated_type_symbol(self) -> Optional[TypeSymbol]:
        return self._associated_type_symbol

    def __str__(self):
        return str(self._scope_name)


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

    def __init__(self, global_scope: Optional[Scope] = None, current_scope: Optional[Scope] = None):
        from SPPCompiler.SemanticAnalysis.ASTs import IdentifierAst

        # Create the global scope, set the current scope to the global scope, and initialize the scope iterator.
        self._global_scope = global_scope or Scope(name=IdentifierAst(-1, "_global"))
        self._current_scope = current_scope or self._global_scope
        self._iterator = iter(self)

        # The Analyzer adds namespace symbols to each namespace it creates (ie "std"). The Global scope is overlooked
        # there, so create it here when a ScopeHandler is created.
        global_namespace_symbol = NamespaceSymbol(name=self._global_scope.name, associated_scope=self._global_scope)
        self._global_scope.add_symbol(global_namespace_symbol)

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
            for child_scope in scope.children:
                yield child_scope
                yield from iterate(child_scope)

        return ScopeIterator(iterate(self._current_scope))

    def move_to_next_scope(self) -> Scope:
        self._current_scope = next(self._iterator)
        return self._current_scope

    def at_global_scope(self, parent_level: int = 0) -> bool:
        scope_to_check = self._current_scope
        for _ in range(parent_level):
            scope_to_check = scope_to_check.parent
        return scope_to_check == self._global_scope

    def get_namespaced_scope(self, namespace: list["IdentifierAst"]) -> Optional[Scope]:
        scope = self._global_scope
        for part in namespace:
            if Seq(scope.children).map(lambda s: s.name).contains(part):
                scope = Seq(scope.children).filter(lambda s: s.name == part).first()
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
