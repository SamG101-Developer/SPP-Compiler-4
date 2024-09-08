import copy
from dataclasses import dataclass
from typing import NoReturn

from SPPCompiler.LexicalAnalysis.Tokens import TokenType
from SPPCompiler.SemanticAnalysis.ASTs.Meta.Ast import Ast
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstMixins import SemanticAnalyser
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstPrinter import AstPrinter, ast_printer_method
from SPPCompiler.SemanticAnalysis.Utils.Scopes import ScopeHandler
from SPPCompiler.SemanticAnalysis.Utils.Symbols import TypeSymbol, TypeAliasSymbol


@dataclass
class UseStatementTypeAliasAst(Ast, SemanticAnalyser):
    new_type: "TypeAst"
    generic_parameters: "GenericParameterGroupAst"
    assignment_token: "TokenAst"
    old_type: "TypeAst"

    def __post_init__(self):
        from SPPCompiler.SemanticAnalysis.ASTs import TypeAst, GenericParameterGroupAst
        self.new_type = TypeAst(self.pos, [], [self.new_type.to_generic_identifier()])
        self.generic_parameters = self.generic_parameters or GenericParameterGroupAst.default()

    @ast_printer_method
    def print(self, printer: AstPrinter) -> NoReturn:
        s = ""
        s += f"{self.new_type.print(printer)} "
        s += f"{self.generic_parameters.print(printer)} "
        s += f"{self.assignment_token.print(printer)} "
        s += f"{self.old_type.print(printer)} "
        return s

    def do_semantic_analysis(self, scope_handler: ScopeHandler, **kwargs) -> None:
        """
        Take the example "use MyVector[T] = std::Vec[T]". This will create a "cls MyVector[T] { }" and
        "sup [T] std::Vec[T] on MyVector[T]".
        """

        from SPPCompiler.SemanticAnalysis.ASTs import ClassPrototypeAst, SupPrototypeInheritanceAst, TokenAst, GenericParameterGroupAst

        # Create a temporary inner scope to load generics into.
        scope_handler.into_new_scope(f"<type-alias:{self.new_type}>")
        for generic_parameter in self.generic_parameters.parameters:
            type_symbol = TypeSymbol(name=generic_parameter.identifier.types[-1], type=None, is_generic=True)
            scope_handler.current_scope.add_symbol(type_symbol)

        self.old_type.do_semantic_analysis(scope_handler, **kwargs)
        old_symbol = scope_handler.current_scope.get_symbol(self.old_type)
        scope_handler.exit_cur_scope()

        # Copy the generic parameter lists
        generic_parameters_1 = GenericParameterGroupAst.from_list(copy.deepcopy(self.generic_parameters.parameters))
        generic_parameters_2 = GenericParameterGroupAst.from_list(copy.deepcopy(self.generic_parameters.parameters))

        # Create a mock class to represent the type alias, and generate it.
        ast_1 = ClassPrototypeAst(self.pos, [], TokenAst.dummy(TokenType.KwCls), self.new_type.types[-1].to_identifier(), generic_parameters_1, None, None)
        ast_1.generate(scope_handler, type_alias=True)

        # Apply a superimposition, to allow for the attribute & method access.
        ast_2 = SupPrototypeInheritanceAst(self.pos, TokenAst.dummy(TokenType.KwSup), generic_parameters_2, self.new_type, None, None, self.old_type, TokenAst.dummy(TokenType.KwOn))
        ast_2.generate(scope_handler)
        old_type_sup_scopes = old_symbol.associated_scope.sup_scopes
        scope_handler.current_scope.children[-2]._sup_scopes.append((scope_handler.current_scope.children[-1], ast_2))

        # Get the symbol of the superclass, and the symbol of the new type.
        new_symbol = scope_handler.current_scope.get_symbol(self.new_type)

        # Associate the new type symbol with the old type symbol as an alias.
        new_symbol.old_type = old_symbol.fq_type
        new_symbol.old_associated_scope = old_symbol.associated_scope


__all__ = ["UseStatementTypeAliasAst"]
