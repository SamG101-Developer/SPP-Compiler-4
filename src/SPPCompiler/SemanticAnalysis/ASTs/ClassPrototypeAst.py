from dataclasses import dataclass, field
from typing import List, Optional

from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstMixins import PreProcessor, SemanticAnalyser, SymbolGenerator, SupScopeLoader
from SPPCompiler.SemanticAnalysis.Utils.CommonTypes import CommonTypes
from SPPCompiler.SemanticAnalysis.Utils.Scopes import ScopeHandler
from SPPCompiler.SemanticAnalysis.Utils.SemanticError import SemanticErrors
from SPPCompiler.SemanticAnalysis.Utils.Symbols import TypeSymbol, VariableSymbol, TypeAliasSymbol
from SPPCompiler.SemanticAnalysis.ASTs.Meta.Ast import Ast
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstPrinter import *
from SPPCompiler.Utils.Sequence import Seq


@dataclass
class ClassPrototypeAst(Ast, PreProcessor, SymbolGenerator, SemanticAnalyser, SupScopeLoader):
    """
    The ClassPrototypeAst node is used to represent the definition of a class. The class's type is created as a type
    symbol in the "generate" stage, or a type-alias symbol if the class is being created as a mock representation of an
    alias's new type name.

    The generic parameters are loaded into the current scope in the "generate" stage, with their types set as None, as
    this is the base class and contains no substitutions. Attributes are subsequently generated, as variable symbols
    over the scope that the type symbol maps to.

    Attributes:
        annotations: The annotations attached to the class.
        cls_keyword: The "cls" token.
        identifier: The identifier of the class.
        generic_parameters: The generic parameters of the class.
        where_block: The where block of the class.
        body: The body of the class, containing.
    """

    annotations: List["AnnotationAst"]
    cls_keyword: "TokenAst"
    identifier: "TypeAst"
    generic_parameters: Optional["GenericParameterGroupAst"]
    where_block: Optional["WhereBlockAst"]
    body: "InnerScopeAst[ClassAttributeAst]"

    _is_alias: bool = field(default=False, init=False, repr=False)

    def __post_init__(self):
        from SPPCompiler.SemanticAnalysis.ASTs import GenericParameterGroupAst, TypeAst, WhereBlockAst, InnerScopeAst

        # Convert the identifier to a TypeAst, and set other attributes to their defaults if they are None.
        self.identifier = TypeAst(self.identifier.pos, [], [self.identifier.to_generic_identifier()])
        self.generic_parameters = self.generic_parameters or GenericParameterGroupAst.default()
        self.where_block = self.where_block or WhereBlockAst.default()
        self.body = self.body or InnerScopeAst.default()

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        # Print the ClassPrototypeAst.
        s = ""
        s += f"{Seq(self.annotations).print(printer, "\n")}{self.cls_keyword.print(printer)} {self.identifier.print(printer)}"
        s += f"{self.generic_parameters.print(printer)}" if self.generic_parameters else ""
        s += f" {self.where_block.print(printer)}" if self.where_block else ""
        s += f"{self.body.print(printer)}"
        return s

    def generate(self, scope_handler: ScopeHandler, *, type_alias: bool = False) -> None:
        """
        The generation stage of a ClassPrototypeAst handles symbol and scope creation. A new scope is created
        immediately for the class, because it is required to link the type symbol to the correct associated scope. The
        symbol is either a TypeSymbol or TypeAliasSymbol. An alias symbol is necessary when creating mock classes to
        analyse aliases uniformly to other types.

        The generic parameters will then be loaded as type-symbols with names and no type/associated scope. They will be
        deep-copied out of scopes and then have their type's set in classes where generic parameters have been
        substituted with generic arguments. Attributes are generated as variable symbols in the class's scope, which is
        linked to the class's type symbol.

        Args:
            scope_handler: The scope handler.
            type_alias: If the class is being created to analyse an alias.

        Returns:
            None
        """

        # Move into a new scope for the class. This will be associated with the class's type symbol.
        scope_handler.into_new_scope(self.identifier.without_generics())

        # Add a new type(-alias) symbol for the class being created.
        match type_alias:
            case False: symbol = TypeSymbol(name=self.identifier.types[-1], type=self, associated_scope=scope_handler.current_scope)
            case _    : symbol = TypeAliasSymbol(name=self.identifier.types[-1], type=self, associated_scope=scope_handler.current_scope)
        scope_handler.current_scope.parent.add_symbol(symbol)
        self._is_alias = type_alias

        # Generate type symbols for the generic parameters, and variable symbols for the attributes.
        scope_handler.current_scope.add_symbol(TypeSymbol(name=CommonTypes.self().types[-1], type=self, associated_scope=scope_handler.current_scope))
        Seq(self.generic_parameters.parameters).for_each(lambda p: scope_handler.current_scope.add_symbol(TypeSymbol(name=p.identifier.types[-1], type=None, is_generic=True)))
        Seq(self.body.members).for_each(lambda m: scope_handler.current_scope.add_symbol(VariableSymbol(name=m.identifier, type=m.type_declaration)))

        # Move back into the parent scope.
        scope_handler.exit_cur_scope()

    def load_sup_scopes(self, scope_handler: ScopeHandler) -> None:
        scope_handler.move_to_next_scope()
        scope_handler.exit_cur_scope()

    def load_sup_scopes_gen(self, scope_handler: ScopeHandler) -> None:
        scope_handler.move_to_next_scope()
        scope_handler.exit_cur_scope()

    def do_semantic_analysis(self, scope_handler, **kwargs) -> None:
        # Move into the class scope created in the "generate" stage.
        scope_handler.move_to_next_scope()

        # Analyse the generic parameters and body of the class with further validation.
        self.generic_parameters.do_semantic_analysis(scope_handler, **kwargs)
        self.where_block.do_semantic_analysis(scope_handler, **kwargs)
        self.body.do_semantic_analysis(scope_handler, inline=True, **kwargs)

        # Check there are no duplicate attribute names.
        if Seq(self.body.members).map(lambda m: m.identifier).contains_duplicates():
            duplicate_attributes = Seq(self.body.members).map(lambda m: m.identifier).non_unique_items()[0]
            raise SemanticErrors.DUPLICATE_ITEM(duplicate_attributes, "attribute")

        # Move back into the parent scope.
        scope_handler.exit_cur_scope()

    def __json__(self):
        return f"{self.identifier}{self.generic_parameters}"


__all__ = ["ClassPrototypeAst"]
