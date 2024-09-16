from __future__ import annotations
from typing import Any, List, Optional, Tuple

from SPPCompiler.LexicalAnalysis.Tokens import TokenType
from SPPCompiler.SemanticAnalysis.ASTs.Meta.Ast import Ast
from SPPCompiler.SemanticAnalysis.Utils.Symbols import TypeSymbol, TypeAliasSymbol, VariableSymbol, SymbolTable
from SPPCompiler.Utils.Sequence import Seq


class Scope:
    _scope_name: Any
    _parent_scope: Optional[Scope]
    _children_scopes: List[Scope]
    _symbol_table: SymbolTable
    _sup_scopes: List[Tuple[Scope, SupPrototypeNormalAst | SupPrototypeInheritanceAst]]
    _associated_type_symbol: Optional[TypeSymbol]

    def __init__(self, name: Any, parent_scope: Optional[Scope] = None):

        # Set the name and parent scope from the parameters.
        self._scope_name = name
        self._parent_scope = parent_scope
        self._children_scopes = []
        self._symbol_table = SymbolTable()
        self._sup_scopes = []
        self._associated_type_symbol = None

    def add_type_symbol(self, symbol: TypeSymbol | TypeAliasSymbol) -> None:
        scope, symbol = shift_scope_for_namespaced_type(self, symbol)
        scope._symbol_table.add(symbol)

    def add_variable_symbol(self, symbol: VariableSymbol) -> None:
        self._symbol_table.add(symbol)

    def get_type_symbol(self, type: GenericIdentifier | TypeAst, exclusive: bool = False, ignore_alias: bool = False) -> Optional[TypeSymbol | TypeAliasSymbol]:
        symbol = self._symbol_table.get(type)
        if not symbol and self._parent_scope and not exclusive:
            symbol = self._parent_scope.get_variable_symbol(type)
        if not symbol and symbol.name.value.startswith("MOCK"):
            symbol = search_super_scopes(self, type)
        return confirm_type_with_alias(self, symbol, ignore_alias)

    def get_variable_symbol(self, name: IdentifierAst, exclusive: bool = False) -> Optional[VariableSymbol]:
        symbol = self._symbol_table.get(name)
        if not symbol and self._parent_scope and not exclusive:
            symbol = self._parent_scope.get_variable_symbol(name)
        if not symbol:
            symbol = search_super_scopes(self, name)
        return symbol

    def get_variable_symbols_multiple(self, name: IdentifierAst) -> Seq[Tuple[VariableSymbol, Scope, int]]:
        return self._get_variable_symbols_multiple(name, self)

    def get_variable_symbol_outermost(self, name: IdentifierAst | PostfixExpressionAst) -> Optional[VariableSymbol]:
        from SPPCompiler.SemanticAnalysis.ASTs import IdentifierAst, PostfixExpressionAst, PostfixExpressionOperatorMemberAccessAst
        name = extract_outermost_name(name)
        if not name: return None

        # For namespaced constants, shift the scope to the namespace.
        if isinstance(name, PostfixExpressionAst) and isinstance(name.op, PostfixExpressionOperatorMemberAccessAst):
            scope, rhs = self, name.op.identifier
            while isinstance(name, PostfixExpressionAst) and isinstance(name.op, PostfixExpressionOperatorMemberAccessAst):
                scope = scope.get_symbol(name.lhs).associated_scope
                name = name.lhs
            symbol = scope.get_symbol(rhs)

        # For identifiers, get the symbol from the symbol table.
        elif isinstance(name, IdentifierAst):
            symbol = self.get_variable_symbol(name)

        # Otherwise, this is a symbol-less expression.
        else:
            symbol = None

        return symbol

    def _get_variable_symbols_multiple(self, name: IdentifierAst, original_scope: Scope) -> Seq[Tuple[VariableSymbol, Scope, int]]:
        symbols = Seq([(self._symbol_table.get(name), self, original_scope.depth_to(self))])
        for sup_scope, _ in self._sup_scopes:
            symbols.extend(sup_scope._get_variable_symbols_multiple(name, original_scope))
        for s, c, d in symbols.copy():
            if not s: symbols.remove((s, c, d))
        return symbols

    def _depth_to(self, that_scope: Scope) -> int:
        def _depth_to(scope: Scope, target: Scope, depth: int) -> int:
            if scope is target:
                return depth
            for child in scope._sup_scopes:
                result = _depth_to(child[0], target, depth + 1)
                if result:
                    return result
            return 0
        return _depth_to(self, that_scope, 0)

    def __json__(self) -> dict:
        return {
            "what": "scope",
            "name": self._scope_name,
            "parent_scope": self._parent_scope._scope_name if self._parent_scope else None,
            "children_scopes": [child for child in self._children_scopes],
            "sup_scopes": [sup.name for sup, _ in self._sup_scopes],
            "symbol_table": self._symbol_table
        }

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


def shift_scope_for_namespaced_type(scope: Scope, symbol: TypeSymbol) -> Tuple[Scope, TypeSymbol]:
    from SPPCompiler.SemanticAnalysis.ASTs import TypeAst

    if isinstance(symbol.name, TypeAst):
        for part in symbol.name.namespace + symbol.name.types[:-1]:
            inner_symbol = scope.get_symbol(part)
            match inner_symbol:
                case None: break
                case _: scope = inner_symbol.associated_scope
        symbol.name = symbol.name.types[-1]

    return scope, symbol


def search_super_scopes(scope: Scope, name: IdentifierAst | GenericIdentifierAst) -> Optional[VariableSymbol]:
    symbol = scope.get_variable_symbol(name, exclusive=True)
    if not symbol:
        for super_scope, _ in scope._sup_scopes:
            symbol = search_super_scopes(super_scope, name)
            if symbol: break
    return symbol


def confirm_type_with_alias(scope: Scope, symbol: TypeSymbol | TypeAliasSymbol, ignore_alias: bool) -> Optional[TypeSymbol]:
    match symbol:
        case TypeAliasSymbol() if symbol.old_type and not ignore_alias: return scope.get_type_symbol(symbol.old_type)
        case _: return symbol


def extract_outermost_name(name: Ast) -> Optional[IdentifierAst]:
    from SPPCompiler.SemanticAnalysis.ASTs import IdentifierAst, PostfixExpressionAst, PostfixExpressionOperatorMemberAccessAst

    match name:
        case IdentifierAst():
            return name

        case PostfixExpressionAst():
            while isinstance(name, PostfixExpressionAst) and isinstance(name.op, PostfixExpressionOperatorMemberAccessAst) and name.op.dot_token.token.token_type == TokenType.TkDot:
                name = name.lhs
            return name

        case _:
            return None
