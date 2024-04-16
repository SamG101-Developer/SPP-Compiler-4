from __future__ import annotations
from dataclasses import dataclass, field
from typing import List, Optional

from src.SemanticAnalysis.ASTMixins.SemanticAnalyser import SemanticAnalyser
from src.SemanticAnalysis.Utils.SemanticError import SemanticError, SemanticErrorType
from src.SemanticAnalysis.ASTMixins.PreProcessor import PreProcessor
from src.SemanticAnalysis.Utils.Scopes import ScopeHandler
from src.SemanticAnalysis.ASTMixins.SymbolGeneration import SymbolGenerator
from src.SemanticAnalysis.Utils.Symbols import TypeSymbol, VariableSymbol
from src.SemanticAnalysis.Utils.CommonTypes import CommonTypes

from src.SemanticAnalysis.ASTs.Meta.Ast import Ast
from src.SemanticAnalysis.ASTs.Meta.AstPrinter import *

from src.SemanticAnalysis.ASTs.GenericParameterGroupAst import GenericParameterGroupAst
from src.SemanticAnalysis.ASTs.WhereBlockAst import WhereBlockAst

from src.Utils.Sequence import Seq


@dataclass
class ClassPrototypeAst(Ast, PreProcessor, SymbolGenerator, SemanticAnalyser):
    """
    The ClassPrototypeAst node is used to represent the definition of a class. The class can be annotated, has an
    identifier, contains a list of attributes, and can have generic parameters and a where block.

    Attributes:
        - annotations: The annotations attached to the class.
        - class_token: The "cls" token.
        - identifier: The identifier of the class.
        - generic_parameters: The generic parameters of the class.
        - where_block: The where block of the class.
        - body: The body of the class, containing.

        - _mod: The module the class is defined in.
    """

    annotations: List["AnnotationAst"]
    class_token: "TokenAst"
    identifier: "TypeAst"
    generic_parameters: Optional["GenericParameterGroupAst"]
    where_block: Optional["WhereBlockAst"]
    body: "InnerScopeAst[ClassAttributeAst]"

    _mod: "ModuleIdentifierAst" = field(default=None, init=False)

    def __post_init__(self):
        # Fill the generic parameters and where block with empty objects if they are None.
        self.generic_parameters = self.generic_parameters or GenericParameterGroupAst.default()
        self.where_block = self.where_block or WhereBlockAst.default()

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
        Seq(self.body.members).for_each(lambda m: m.type_declaration.substitute_generics(CommonTypes.self(), self.identifier))
        Seq(self.generic_parameters.get_opt()).for_each(lambda p: p.default_value.substitute_generics(CommonTypes.self(), self.identifier))
        self._mod = context.identifier

    def generate(self, s: ScopeHandler) -> None:
        # Add a new TypeSymbol to the current scope, representing this class being generated. Move into the new scope
        # (representing the new type symbol). Associate the scope with the symbol.
        sym = TypeSymbol(self.identifier, self)
        s.current_scope.add_symbol(sym)
        s.into_new_scope(self.identifier)
        sym.associated_scope = s.current_scope

        # Add new TypeSymbols for each generic parameter to the scope, representing "None". This is because the
        # attributes may rely on these generic types. Build VariableSymbols for each attribute of the class. Add "Self"
        # as a TypeSymbol pointing to the current class.
        s.current_scope.add_symbol(TypeSymbol(CommonTypes.self(), self))
        Seq(self.generic_parameters.parameters).for_each(lambda p: s.current_scope.add_symbol(TypeSymbol(p.identifier, None)))
        Seq(self.body.members).for_each(lambda m: s.current_scope.add_symbol(VariableSymbol(m.identifier, m.type_declaration)))

        # Move back into the parent scope.
        s.exit_cur_scope()

    def do_semantic_analysis(self, scope_handler, **kwargs) -> None:
        # Move into the class scope to have access to types defined on the class (generics parameters)
        scope_handler.move_to_next_scope()

        # Analyse the annotations, generic parameters, where block and the body of the class, to ensure everything being
        # contained is valid.
        Seq(self.annotations).for_each(lambda a: a.do_semantic_analysis(scope_handler, **kwargs))
        self.generic_parameters.do_semantic_analysis(scope_handler, **kwargs)
        # self.where_block.do_semantic_analysis(s, scope_handler, **kwargs, **kwargs)
        self.body.do_semantic_analysis(scope_handler, inline_block=True, **kwargs)

        # Check that no attributes have the same name as each other. Raise an exception if they do.
        if Seq(self.body.members).map(lambda m: m.identifier).contains_duplicates():
            duplicate_attributes = Seq(self.body.members).map(lambda m: m.identifier).non_unique_items()[0]
            raise SemanticError().add_error(
                pos=duplicate_attributes[1].pos,
                error_type=SemanticErrorType.NAME_ERROR,
                message=f"Cannot have duplicate attributes in a class",
                tag_message=f"Attribute '{duplicate_attributes[0]}' re-declared here",
                tip=f"Change the name of the attribute to something unique")

        # Move back into the parent scope.
        scope_handler.exit_cur_scope()

    def __json__(self):
        return self.identifier


__all__ = ["ClassPrototypeAst"]
