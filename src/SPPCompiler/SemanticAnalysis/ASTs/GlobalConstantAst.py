from dataclasses import dataclass

from SPPCompiler.SemanticAnalysis.ASTs.Meta.Ast import Ast
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstMixins import SemanticAnalyser, SymbolGenerator, PreProcessor, SupScopeLoader
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstPrinter import AstPrinter
from SPPCompiler.SemanticAnalysis.Utils.Scopes import ScopeHandler
from SPPCompiler.SemanticAnalysis.Utils.SemanticError import SemanticErrors
from SPPCompiler.SemanticAnalysis.Utils.Symbols import VariableSymbol


@dataclass
class GlobalConstantAst(Ast, PreProcessor, SupScopeLoader,  SemanticAnalyser, SymbolGenerator):
    """
    The GlobalConstantAst node is used to represent a global constant (initialized with a value). The constant is a
    single variable, with a literal assigned to it.

    Notes:
        - The constant is a global variable, and is not mutable.
        - The constant must be initialized with a value.
        - The constant is auto pinned and cannot be moved.
        - In the future, more advanced expressions will be supported (via constant functions etc).

    Attributes:
        let_keyword: The `let` keyword token.
        assign_to: The variable being assigned to.
        assign_token: The assignment token.
        value: The value being assigned to the variable.
    """

    let_keyword: "TokenAst"
    assign_to: "LocalVariableSingleIdentifierAst"
    assign_token: "TokenAst"
    value: "LiteralAst"

    def print(self, printer: AstPrinter) -> str:
        s = ""
        s += f"{self.let_keyword.print(printer)}"
        s += f"{self.assign_to.print(printer)} "
        s += f"{self.assign_token.print(printer)} "
        s += f"{self.value.print(printer)}"
        return s

    def pre_process(self, context) -> None:
        # Dummy implementation required as all module members must implement this method.
        ...

    def generate(self, scope_handler: ScopeHandler) -> None:
        # Global constants cannot be redifined.
        # Todo: this might get the same symbols out of the current namespace (exclusive=True)?
        if scope_handler.current_scope.has_symbol(self.assign_to.identifier):
            raise SemanticErrors.REDEFINED_GLOBAL_CONSTANT(self.assign_to.identifier, self.let_keyword, scope_handler.current_scope.get_symbol(self.assign_to.identifier).memory_info.ast_initialized)

        # Generate the symbol in the global (or namespaced) scope, and mark it as pinned.
        variable_symbol = VariableSymbol(name=self.assign_to.identifier, type=self.value.infer_type(scope_handler).type)
        variable_symbol.memory_info.ast_pins.append(variable_symbol.name)
        variable_symbol.memory_info.ast_initialized = self
        scope_handler.current_scope.add_symbol(variable_symbol)

    def load_sup_scopes(self, scope_handler: ScopeHandler) -> None:
        # Override default behaviour and do nothing.
        ...

    def do_semantic_analysis(self, scope_handler: ScopeHandler, **kwargs) -> None:
        # Ensure the variable is immutable.
        if self.assign_to.is_mutable:
            raise SemanticErrors.MUTABLE_GLOBAL_CONSTANT(self.assign_to.is_mutable)

        # Analyse the value
        self.value.do_semantic_analysis(scope_handler, **kwargs)


__all__ = ["GlobalConstantAst"]
