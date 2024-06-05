from __future__ import annotations
from dataclasses import dataclass, field
from typing import List, Optional

from SPPCompiler.SemanticAnalysis.ASTMixins.SemanticAnalyser import SemanticAnalyser
from SPPCompiler.SemanticAnalysis.Utils.SemanticError import SemanticError, SemanticErrorType
from SPPCompiler.SemanticAnalysis.ASTMixins.PreProcessor import PreProcessor
from SPPCompiler.SemanticAnalysis.Utils.Scopes import ScopeHandler
from SPPCompiler.SemanticAnalysis.ASTMixins.SymbolGeneration import SymbolGenerator
from SPPCompiler.SemanticAnalysis.Utils.Symbols import TypeSymbol, VariableSymbol
from SPPCompiler.SemanticAnalysis.Utils.CommonTypes import CommonTypes

from SPPCompiler.SemanticAnalysis.ASTs.Meta.Ast import Ast
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstPrinter import *

from SPPCompiler.SemanticAnalysis.ASTs.GenericParameterGroupAst import GenericParameterGroupAst
from SPPCompiler.SemanticAnalysis.ASTs.WhereBlockAst import WhereBlockAst

from SPPCompiler.Utils.Sequence import Seq


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
        ...

    def __json__(self):
        return self.identifier


__all__ = ["ClassPrototypeAst"]
