from dataclasses import dataclass
from typing import List

from SPPCompiler.SemanticAnalysis.ASTs.Meta.Ast import Ast
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstMixins import SemanticAnalyser, SymbolGenerator, PreProcessor, SupScopeLoader
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstPrinter import AstPrinter
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstUtils import InferredType, VisibilityEnabled
from SPPCompiler.SemanticAnalysis.Utils.Scopes import ScopeHandler
from SPPCompiler.SemanticAnalysis.Utils.SemanticError import SemanticErrors
from SPPCompiler.SemanticAnalysis.Utils.Symbols import VariableSymbol
from SPPCompiler.Utils.Sequence import Seq


@dataclass
class GlobalConstantAst(Ast, PreProcessor, SupScopeLoader, SemanticAnalyser, SymbolGenerator, VisibilityEnabled):
    """
    The GlobalConstantAst node is used to represent a global constant (initialized with a value). The constant is a
    single variable, with a literal assigned to it.

    Notes:
        The constant is a global variable, and is not mutable.
        The constant must be initialized with a value.
        The constant is auto pinned and cannot be moved.
        In the future, more advanced expressions will be supported (via constant functions etc).

    Attributes:
        annotations: The annotations attached to the constant.
        let_keyword: The `let` keyword token.
        assign_to: The variable being assigned to.
        colon_token: The colon token.
        type_declaration: The variable's type.
        assign_token: The assignment token.
        value: The value being assigned to the variable.
    """

    annotations: List["AnnotationAst"]
    let_keyword: "TokenAst"
    assign_to: "LocalVariableSingleIdentifierAst"
    colon_token: "Token"
    type_declaration: "TypeAst"
    assign_token: "TokenAst"
    value: "LiteralAst"

    def print(self, printer: AstPrinter) -> str:
        s = ""
        s += f"{Seq(self.annotations).print(printer, '\n')}"
        s += f"{self.let_keyword.print(printer)}"
        s += f"{self.assign_to.print(printer)} "
        s += f"{self.colon_token.print(printer)} "
        s += f"{self.type_declaration.print(printer)} "
        s += f"{self.assign_token.print(printer)} "
        s += f"{self.value.print(printer)}"
        return s

    def generate(self, scope_handler: ScopeHandler) -> None:
        # Global constants cannot be redefined.
        if existing_symbol := scope_handler.current_scope.parent_module.get_symbol(self.assign_to.identifier, exclusive=True):
            raise SemanticErrors.REDEFINED_GLOBAL_CONSTANT(self.assign_to.identifier, self.let_keyword, existing_symbol.memory_info.ast_initialized)

        # Generate the symbol in the global (or namespaced) scope, and mark it as pinned.
        symbol = VariableSymbol(name=self.assign_to.identifier, type=self.type_declaration, visibility=self._visibility)
        symbol.memory_info.ast_pins.append(symbol.name)
        symbol.memory_info.ast_initialized = self
        scope_handler.current_scope.parent_module.add_symbol(symbol)

    def do_semantic_analysis(self, scope_handler: ScopeHandler, **kwargs) -> None:
        from SPPCompiler.SemanticAnalysis.ASTs import ConventionMovAst

        # Ensure the variable is immutable.
        if self.assign_to.is_mutable:
            raise SemanticErrors.MUTABLE_GLOBAL_CONSTANT(self.assign_to.is_mutable)

        # Analyse the type and value.
        self.type_declaration.do_semantic_analysis(scope_handler, **kwargs)
        self.value.do_semantic_analysis(scope_handler, **kwargs)

        # Ensure the value matches the type.
        expected_type = InferredType(convention=ConventionMovAst, type=self.type_declaration)
        given_type = self.value.infer_type(scope_handler)
        if not expected_type.symbolic_eq(given_type, scope_handler.current_scope.parent_module):
            raise SemanticErrors.TYPE_MISMATCH_2(None, self.value, expected_type, given_type, scope_handler)


__all__ = ["GlobalConstantAst"]
