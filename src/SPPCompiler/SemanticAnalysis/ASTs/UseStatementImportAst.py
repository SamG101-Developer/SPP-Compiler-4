from dataclasses import dataclass, field
from typing import Dict, List, Optional

from SPPCompiler.LexicalAnalysis.Tokens import TokenType
from SPPCompiler.SemanticAnalysis.ASTs.Meta.Ast import Ast
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstMixins import SemanticAnalyser, SymbolGenerator, SupScopeLoader
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstPrinter import AstPrinter, ast_printer_method
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstUtils import Visibility
from SPPCompiler.SemanticAnalysis.Utils.Scopes import ScopeHandler, Scope
from SPPCompiler.Utils.Sequence import Seq


@dataclass
class UseStatementImportAst(Ast, SymbolGenerator, SupScopeLoader, SemanticAnalyser):
    body: "UseStatementImportBodyAst"

    _generated: bool = field(default=False, init=False, repr=False)
    _new_asts: List[Ast] = field(default_factory=list, init=False, repr=False)

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        s = ""
        s += f"{self.body.print(printer)}"
        return s

    def convert(self, scope_handler: ScopeHandler) -> None:
        from SPPCompiler.SemanticAnalysis.ASTs import (
            UseStatementImportBodyAst, IdentifierAst, TypeAst, UseStatementImportSingleTypeAst,
            UseStatementImportMultipleTypesAst, UseStatementTypeAliasAst, TokenAst, GenericArgumentNamedAst,
            GenericArgumentGroupAst)

        def combine_layers(ast: UseStatementImportBodyAst, current_namespace: List[IdentifierAst], types: Dict[TypeAst, Optional[TypeAst]]):
            match ast.type:
                case UseStatementImportSingleTypeAst():
                    # Save the original length of the namespace, and add the next nested namespace.
                    original_ns_len = len(current_namespace)
                    current_namespace += ast.type.namespace

                    # Create a new type with the current namespace and the types from the import.
                    new_type = TypeAst(-1, namespace=current_namespace.copy(), types=ast.type.types)

                    # Remove the nested namespace from the current namespace.
                    for i in range(len(current_namespace) - original_ns_len):
                        current_namespace.pop()

                    # Add the new type to the list of types.
                    type_alias = TypeAst(-1, [], ast.type.alias.type.types.copy()) if ast.type.alias else None
                    types[new_type] = type_alias

                case UseStatementImportMultipleTypesAst():
                    # Save the original length of the namespace, and add the next nested namespace.
                    original_ns_len = len(current_namespace)
                    current_namespace += ast.type.namespace

                    # For each type in the import, recursively combine the layers.
                    for inner_type in ast.type.types:
                        combine_layers(inner_type, current_namespace, types)

                    # Remove the nested namespace from the current namespace.
                    for i in range(len(current_namespace) - original_ns_len):
                        current_namespace.pop()

        types = {}
        combine_layers(self.body, [], types)

        # Convert the import aliases to type aliases: "use std::Str" => "use Str = std::Str".
        for type, alias in types.items():
            generic_parameters = scope_handler.current_scope.get_symbol(type).type.generic_parameters
            generic_arguments = Seq(generic_parameters.parameters).map(lambda p: GenericArgumentNamedAst(p.pos, p.identifier.types[-1].to_identifier(), TokenAst.dummy(TokenType.TkAssign), p.identifier)).list()
            type.types[-1].generic_arguments = GenericArgumentGroupAst.from_list(generic_arguments)
            new_ast = UseStatementTypeAliasAst(type.pos, (alias or type).types[-1].to_identifier(), generic_parameters, TokenAst.dummy(TokenType.TkAssign), type)
            self._new_asts.append(new_ast)

        self._generated = True

    def generate(self, scope_handler: ScopeHandler, visibility: Visibility = Visibility.Private) -> None:
        self.convert(scope_handler)
        for new_ast in self._new_asts:
            new_ast.generate(scope_handler, visibility)

    def load_sup_scopes(self, scope_handler: ScopeHandler) -> None:
        Seq(self._new_asts).for_each(lambda ast: ast.load_sup_scopes(scope_handler))

    def load_sup_scopes_gen(self, scope_handler: ScopeHandler) -> None:
        Seq(self._new_asts).for_each(lambda ast: ast.load_sup_scopes_gen(scope_handler))

    def do_semantic_analysis(self, scope_handler: ScopeHandler, **kwargs) -> None:
        if not self._generated:
            self.convert(scope_handler)
        Seq(self._new_asts).for_each(lambda ast: ast.do_semantic_analysis(scope_handler, **kwargs))


__all__ = ["UseStatementImportAst"]
