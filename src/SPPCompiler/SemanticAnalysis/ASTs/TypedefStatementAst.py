from dataclasses import dataclass
from typing import Optional

from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstMixins import PreProcessor, SemanticAnalyser, SymbolGenerator, SupScopeLoader
from SPPCompiler.SemanticAnalysis.ASTs.Meta.Ast import Ast
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstPrinter import *
from SPPCompiler.SemanticAnalysis.Utils.CommonTypes import CommonTypes
from SPPCompiler.SemanticAnalysis.Utils.Scopes import ScopeHandler
from SPPCompiler.SemanticAnalysis.Utils.SemanticError import SemanticErrors
from SPPCompiler.SemanticAnalysis.Utils.Symbols import TypeSymbol, NamespaceSymbol
from SPPCompiler.Utils.Sequence import Seq


@dataclass
class TypedefStatementAst(Ast, PreProcessor, SemanticAnalyser, SymbolGenerator, SupScopeLoader):
    """
    The TypedefStatementAst node is used to represent a typedef statement. This can be used to create a new type from an
    existing type, or to reduce the namespace of a type.
    
    Attributes:
        use_keyword: The "use" keyword token.
        generic_parameters: The generic parameters of the typedef statement.
        old_type_namespace: The old type namespace of the typedef statement.
        items: The items of the typedef statement.
    """
    
    use_keyword: "TokenAst"
    generic_parameters: "GenericParameterGroupAst"
    old_type_namespace: Optional["TypedefStatementOldNamespaceAst"]
    items: "TypedefStatementItemAst"

    def __post_init__(self):
        from SPPCompiler.SemanticAnalysis.ASTs import GenericParameterGroupAst
        self.generic_parameters = self.generic_parameters or GenericParameterGroupAst.default()

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        # Print the TypedefStatementAst.
        s = ""
        s += f"{self.use_keyword.print(printer)}{self.generic_parameters.print(printer)}"
        s += f"{self.old_type_namespace.print(printer)}" if self.old_type_namespace else ""
        s += f"{self.items.print(printer)}"
        return s

    def pre_process(self, context: "ModulePrototypeAst") -> None:
        # Substitute the "Self" type in generic parameters.
        Seq(self.generic_parameters.get_opt()).for_each(lambda p: p.default_value.substitute_generics(CommonTypes.self(), context.identifier))

    def generate(self, scope_handler: ScopeHandler) -> None:
        # todo: this has issues with module-level typedefs (not analysable at this point)
        # todo: (for module-level declarations): add the symbol with no associated type until analysis?
        ...

    def load_sup_scopes(self, scope_handler: ScopeHandler) -> None:
        ...

    def do_semantic_analysis(self, scope_handler: ScopeHandler, **kwargs) -> None:
        from SPPCompiler.SemanticAnalysis.ASTs import TypedefStatementSpecificItemAst, TypedefStatementSpecificItemsAst, TypedefStatementAllItemsAst, TypeAst

        # todo: only allow generic parameters in certain cases (single aliasing)

        # Check the namespace exists.
        namespace = self.old_type_namespace.items
        if not scope_handler.get_namespaced_scope(namespace):
            raise SemanticErrors.UNKNOWN_IDENTIFIER(namespace[-1], [], "namespace")

        # Check the entire type(s) exist(s).
        match self.items:
            case TypedefStatementSpecificItemAst(_, pull_type, push_type):
                # Convert the expanded type (ns + type) into a TypeAst.
                expanded_type = self.old_type_namespace.items.copy() + pull_type.parts.copy()
                expanded_type = TypeAst(expanded_type[0].pos, expanded_type)
                expanded_type.do_semantic_analysis(scope_handler, **kwargs)
                old_type_symbol = scope_handler.current_scope.get_symbol(expanded_type)

                # Create a new type symbol with the linked type.
                new_type = push_type.new_type if push_type else pull_type
                new_type_symbol = TypeSymbol(
                    name=TypeAst(new_type.parts[-1].pos, [new_type.parts[-1]]),
                    type=old_type_symbol.type,
                    associated_scope=old_type_symbol.associated_scope)

                # Inject the new type symbol into the current scope.
                scope_handler.current_scope.add_symbol(new_type_symbol)

            case TypedefStatementSpecificItemsAst(_, _, items, _):
                # Do the semantic analysis for each item.
                for item in items:
                    separated = TypedefStatementAst(self.pos, self.use_keyword, self.generic_parameters, self.old_type_namespace, item)
                    separated.do_semantic_analysis(scope_handler, **kwargs)

            case TypedefStatementAllItemsAst():
                # Get all the symbols from the namespace (TypeSymbol and NamespaceSymbol).
                namespace_scope = scope_handler.get_namespaced_scope(namespace)
                for symbol in Seq(namespace_scope.all_symbols()).filter_to_type(TypeSymbol, NamespaceSymbol):
                    # Inject the symbol into the current scope.
                    scope_handler.current_scope.add_symbol(symbol)


__all__ = ["TypedefStatementAst"]
