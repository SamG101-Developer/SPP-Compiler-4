from dataclasses import dataclass, field

from SPPCompiler.SemanticAnalysis.ASTMixins.PreProcessor import PreProcessor
from SPPCompiler.SemanticAnalysis.ASTMixins.SemanticAnalyser import SemanticAnalyser
from SPPCompiler.SemanticAnalysis.ASTMixins.SupScopeLoader import SupScopeLoader
from SPPCompiler.SemanticAnalysis.ASTMixins.SymbolGeneration import SymbolGenerator
from SPPCompiler.SemanticAnalysis.ASTs.Meta.Ast import Ast
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstPrinter import *
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstUtils import ensure_memory_integrity
from SPPCompiler.SemanticAnalysis.Utils.CommonTypes import CommonTypes
from SPPCompiler.SemanticAnalysis.Utils.Scopes import ScopeHandler
from SPPCompiler.SemanticAnalysis.Utils.SemanticError import SemanticErrors
from SPPCompiler.SemanticAnalysis.Utils.Symbols import VariableSymbol


@dataclass
class LetStatementInitializedAst(Ast, PreProcessor, SymbolGenerator, SemanticAnalyser, SupScopeLoader):
    """
    The LetStatementInitializedAst node is used to represent a variable being initialized with a value. The variable
    could be a single variable, a tuple or a destructure. Recursive destructuring is supported.

    Attributes:
        let_keyword: The `let` keyword token.
        assign_to: The variable being assigned to.
        assign_token: The assignment token.
        value: The value being assigned to the variable.

        _sup_let_type: For function preprocessing.
    """

    let_keyword: "TokenAst"
    assign_to: "LocalVariableAst"
    assign_token: "TokenAst"
    value: "ExpressionAst"
    _sup_let_type: "TypeAst" = field(default=None, kw_only=True)

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        # Print the LetStatementInitializedAst.
        s = ""
        s += f"{self.let_keyword.print(printer)}"
        s += f"{self.assign_to.print(printer)} "
        s += f"{self.assign_token.print(printer)} "
        s += f"{self.value.print(printer)}"
        return s

    def pre_process(self, context) -> None:
        # There is no preprocessing required for a let statement.
        # TODO: why inherit from PreProcessor?
        pass

    def generate(self, scope_handler: ScopeHandler) -> None:
        # Generate the symbol for the variable being assigned to. This is only used for function preprocessing.
        variable_symbol = VariableSymbol(name=self.assign_to.identifier, type=self._sup_let_type)
        scope_handler.current_scope.add_symbol(variable_symbol)

    def load_sup_scopes(self, scope_handler: ScopeHandler) -> None:
        ...

    def do_semantic_analysis(self, scope_handler, **kwargs) -> None:
        from SPPCompiler.SemanticAnalysis.ASTs import TypeAst

        if self._sup_let_type is not None: return
        ensure_memory_integrity(self, self.value, self.assign_token, scope_handler)

        # Check the value being assigned is not a type.
        if isinstance(self.value, TypeAst):
            raise SemanticErrors.INVALID_USE_OF_TYPE_AS_EXPR(self.value)

        # Analyse the value being assigned to the variable.
        kwargs |= {"value": self.value, "let_ast": self}
        self.assign_to.do_semantic_analysis(scope_handler, **kwargs)

        # Check the type being assigned is not std::Void.
        value_type = self.value.infer_type(scope_handler).type
        if value_type.symbolic_eq(CommonTypes.void(), scope_handler.current_scope):
            raise SemanticErrors.VOID_USAGE(self.value)


__all__ = ["LetStatementInitializedAst"]
