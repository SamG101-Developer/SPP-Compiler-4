import copy
from dataclasses import dataclass, field
from typing import NoReturn

from SPPCompiler.LexicalAnalysis.Tokens import TokenType
from SPPCompiler.SemanticAnalysis.ASTs.Meta.Ast import Ast
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstMixins import SymbolGenerator, SemanticAnalyser, SupScopeLoader
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstPrinter import AstPrinter, ast_printer_method
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstUtils import Visibility
from SPPCompiler.SemanticAnalysis.Utils.Scopes import ScopeHandler, Scope
from SPPCompiler.SemanticAnalysis.Utils.Symbols import TypeSymbol


@dataclass
class UseStatementTypeAliasAst(Ast, SymbolGenerator, SemanticAnalyser, SupScopeLoader):
    new_type: "TypeAst"
    generic_parameters: "GenericParameterGroupAst"
    assignment_token: "TokenAst"
    old_type: "TypeAst"

    _generated: bool = field(default=False, init=False, repr=False)

    def __post_init__(self):
        from SPPCompiler.SemanticAnalysis.ASTs import TypeAst, GenericParameterGroupAst
        self.new_type = TypeAst(self.pos, [], [self.new_type.to_generic_identifier()])
        self.generic_parameters = self.generic_parameters or GenericParameterGroupAst.default()

    @ast_printer_method
    def print(self, printer: AstPrinter) -> NoReturn:
        s = ""
        s += f"{self.new_type.print(printer)}"
        s += f"{self.generic_parameters.print(printer)} "
        s += f"{self.assignment_token.print(printer)} "
        s += f"{self.old_type.print(printer)}"
        return s

    def generate(self, scope_handler: ScopeHandler, visibility: Visibility = Visibility.Private, scope_override: bool = False) -> None:
        from SPPCompiler.SemanticAnalysis.ASTs import ClassPrototypeAst, TokenAst, GenericParameterGroupAst

        generic_parameters = GenericParameterGroupAst.from_list(copy.deepcopy(self.generic_parameters.parameters))
        ast = ClassPrototypeAst(self.pos, [], TokenAst.dummy(TokenType.KwCls), self.new_type.types[-1].to_identifier(), generic_parameters, None, None)
        ast._visibility = visibility
        ast.generate(scope_handler, type_alias=True)

        scope_handler.into_new_scope(f"<type-alias:{self.new_type}>")
        for generic_parameter in self.generic_parameters.parameters:
            type_symbol = TypeSymbol(name=generic_parameter.identifier.types[-1], type=None, is_generic=True)
            scope_handler.current_scope.add_symbol(type_symbol)

        self._generated = True
        if not scope_override:
            scope_handler.exit_cur_scope()

    def load_sup_scopes(self, scope_handler: ScopeHandler, scope_override: bool = False) -> None:
        from SPPCompiler.SemanticAnalysis.ASTs import TypeAst, SupPrototypeInheritanceAst, TokenAst, GenericParameterGroupAst

        if not scope_override:
            scope_handler.move_to_next_scope()

        # Skip the mock-class scopes introduced.
        while isinstance(scope_handler.current_scope.name, TypeAst):
            scope_handler.move_to_next_scope()

        # Create a temporary inner scope to load generics into.
        self.old_type.do_semantic_analysis(scope_handler)
        old_symbol = scope_handler.current_scope.get_symbol(self.old_type)

        # Copy the generic parameter lists
        generic_parameters = GenericParameterGroupAst.from_list(copy.deepcopy(self.generic_parameters.parameters))

        # Apply a superimposition, to allow for the attribute and method access.
        ast = SupPrototypeInheritanceAst(self.pos, TokenAst.dummy(TokenType.KwSup), generic_parameters, self.new_type, None, None, TokenAst.dummy(TokenType.KwExt), self.old_type)
        ast.generate(scope_handler)
        scope_handler.current_scope.parent.get_symbol(self.new_type).associated_scope._sup_scopes.append((scope_handler.current_scope.children[-1], ast))

        # Todo: this needs to be done before the load sup scope stage.
        # Associate the new type symbol with the old type symbol as an alias.
        new_symbol = scope_handler.current_scope.get_symbol(self.new_type)
        new_symbol.old_type = old_symbol.fq_type
        new_symbol.old_associated_scope = old_symbol.associated_scope

        if not scope_override:
            scope_handler.exit_cur_scope()

    def load_sup_scopes_gen(self, scope_handler: ScopeHandler, scope_override: bool = False) -> None:
        from SPPCompiler.SemanticAnalysis.ASTs import TypeAst
        from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstUtils import SupNormalIdentifier, SupInheritanceIdentifier

        # print("[B] MOVED INTO", scope_handler.current_scope, "for", self)
        if not scope_override:
            scope_handler.move_to_next_scope()

        # Skip the mock-class scopes and mock-sup scopes introduced.
        while isinstance(scope_handler.current_scope.name, (TypeAst, SupNormalIdentifier, SupInheritanceIdentifier)):
            scope_handler.move_to_next_scope()

        if not scope_override:
            scope_handler.exit_cur_scope()

    def do_semantic_analysis(self, scope_handler: ScopeHandler, **kwargs) -> None:
        from SPPCompiler.SemanticAnalysis.ASTs import TypeAst
        from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstUtils import SupNormalIdentifier, SupInheritanceIdentifier

        if self._generated:
            scope_handler.move_to_next_scope()
            while isinstance(scope_handler.current_scope.name, (TypeAst, SupNormalIdentifier, SupInheritanceIdentifier)):
                scope_handler.move_to_next_scope()
            scope_handler.exit_cur_scope()
        else:
            self.generate(scope_handler, scope_override=True)
            self.load_sup_scopes(scope_handler, scope_override=True)
            self.load_sup_scopes_gen(scope_handler, scope_override=True)
            scope_handler.exit_cur_scope()


__all__ = ["UseStatementTypeAliasAst"]
