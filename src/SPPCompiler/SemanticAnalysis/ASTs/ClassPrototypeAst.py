from dataclasses import dataclass, field
from typing import List, Optional

from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstMixins import PreProcessor, SemanticAnalyser, SymbolGenerator, SupScopeLoader
from SPPCompiler.SemanticAnalysis.Utils.CommonTypes import CommonTypes
from SPPCompiler.SemanticAnalysis.Utils.Scopes import ScopeHandler
from SPPCompiler.SemanticAnalysis.Utils.SemanticError import SemanticErrors
from SPPCompiler.SemanticAnalysis.Utils.Symbols import TypeSymbol, VariableSymbol

from SPPCompiler.SemanticAnalysis.ASTs.Meta.Ast import Ast
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstPrinter import *

from SPPCompiler.Utils.Sequence import Seq


@dataclass
class ClassPrototypeAst(Ast, PreProcessor, SymbolGenerator, SemanticAnalyser, SupScopeLoader):
    """
    The ClassPrototypeAst node is used to represent the definition of a class. The class can be annotated, has an
    identifier, contains a list of attributes, and can have generic parameters and a where block.

    Attributes:
        annotations: The annotations attached to the class.
        class_token: The "cls" token.
        identifier: The identifier of the class.
        generic_parameters: The generic parameters of the class.
        where_block: The where block of the class.
        body: The body of the class, containing.

        _mod: The module the class is defined in.
    """

    annotations: List["AnnotationAst"]
    class_token: "TokenAst"
    identifier: "TypeAst"
    generic_parameters: Optional["GenericParameterGroupAst"]
    where_block: Optional["WhereBlockAst"]
    body: "InnerScopeAst[ClassAttributeAst]"

    _mod: "ModuleIdentifierAst" = field(default=None, init=False)

    def __post_init__(self):
        from SPPCompiler.SemanticAnalysis.ASTs import GenericParameterGroupAst, TypeAst, WhereBlockAst, InnerScopeAst

        # Fill the generic parameters and where block with empty objects if they are None.
        self.identifier = TypeAst(self.identifier.pos, [], [self.identifier.to_generic_identifier()])
        self.generic_parameters = self.generic_parameters or GenericParameterGroupAst.default()
        self.where_block = self.where_block or WhereBlockAst.default()
        self.body = self.body or InnerScopeAst.default()

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        # Print the ClassPrototypeAst.
        s = ""
        s += f"{Seq(self.annotations).print(printer, "\n")}{self.class_token.print(printer)}{self.identifier.print(printer)}"
        s += f"{self.generic_parameters.print(printer)}" if self.generic_parameters else ""
        s += f" {self.where_block.print(printer)}" if self.where_block else ""
        s += f"{self.body.print(printer)}"
        return s

    def pre_process(self, context: "ModulePrototypeAst") -> None:
        # Replace "Self" in the generic parameters and attribute types so that they refer to the current class.
        self._mod = context.identifier

    def generate(self, scope_handler: ScopeHandler) -> None:
        # Add a new TypeSymbol to the current scope, representing this class being generated. Move into the new scope
        # (representing the new type symbol). Associate the scope with the symbol.
        scope_handler.into_new_scope(self.identifier)

        symbol = TypeSymbol(name=self.identifier.types[-1], type=self, associated_scope=scope_handler.current_scope)
        scope_handler.current_scope.parent.add_symbol(symbol)

        # Add new TypeSymbols for each generic parameter to the scope, representing "None". This is because the
        # attributes may rely on these generic types. Build VariableSymbols for each attribute of the class. Add "Self"
        # as a TypeSymbol pointing to the current class.
        scope_handler.current_scope.add_symbol(TypeSymbol(name=CommonTypes.self().types[-1], type=self, associated_scope=scope_handler.current_scope))
        Seq(self.generic_parameters.parameters).for_each(lambda p: scope_handler.current_scope.add_symbol(TypeSymbol(name=p.identifier.types[-1], type=None, is_generic=True)))
        Seq(self.body.members).for_each(lambda m: scope_handler.current_scope.add_symbol(VariableSymbol(name=m.identifier, type=m.type_declaration)))

        # Move back into the parent scope.
        scope_handler.exit_cur_scope()

    def do_semantic_analysis(self, scope_handler, **kwargs) -> None:
        if "no-scope" not in kwargs:
            scope_handler.move_to_next_scope()

        # Analyse the generic type parameters and where block. This will load the generics into the current scope, and
        # ensure all their constraints are valid.
        self.generic_parameters.do_semantic_analysis(scope_handler, **kwargs)
        self.where_block.do_semantic_analysis(scope_handler, **kwargs)
        self.body.do_semantic_analysis(scope_handler, inline=True, **kwargs)

        # Check there are no duplicate attribute names for this class, and raise an exception if there are.
        if Seq(self.body.members).map(lambda m: m.identifier).contains_duplicates():
            duplicate_attributes = Seq(self.body.members).map(lambda m: m.identifier).non_unique_items()[0]
            raise SemanticErrors.DUPLICATE_ITEM(duplicate_attributes, "attribute")

        if "no-scope" not in kwargs:
            scope_handler.exit_cur_scope()

    def __json__(self):
        return self.identifier


__all__ = ["ClassPrototypeAst"]
