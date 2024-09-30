from dataclasses import dataclass
from typing import NoReturn

from SPPCompiler.SemanticAnalysis.ASTs.Meta.Ast import Ast
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstMixins import SemanticAnalyser, PreProcessor, SymbolGenerator, SupScopeLoader
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstPrinter import AstPrinter, ast_printer_method
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstUtils import TypeInfer, InferredType, VisibilityEnabled
from SPPCompiler.SemanticAnalysis.Utils.CommonTypes import CommonTypes
from SPPCompiler.SemanticAnalysis.Utils.Scopes import ScopeHandler


@dataclass
class UseStatementAst(Ast, PreProcessor, SymbolGenerator, SupScopeLoader, SemanticAnalyser, TypeInfer, VisibilityEnabled):
    use_keyword: "TokenAst"
    body: "UseStatementImportAst | UseStatementTypeAliasAst"

    @ast_printer_method
    def print(self, printer: AstPrinter) -> NoReturn:
        s = ""
        s += f"{self.use_keyword.print(printer)} "
        s += f"{self.body.print(printer)}"
        return s

    def generate(self, scope_handler: ScopeHandler) -> None:
        self.body.generate(scope_handler)

    def load_sup_scopes(self, scope_handler: ScopeHandler) -> None:
        self.body.load_sup_scopes(scope_handler)

    def load_sup_scopes_gen(self, scope_handler: ScopeHandler) -> None:
        self.body.load_sup_scopes_gen(scope_handler)

    def do_semantic_analysis(self, scope_handler: ScopeHandler, **kwargs) -> None:
        # Pass the analysis onto the body (can be either a type or import)
        self.body.do_semantic_analysis(scope_handler, **kwargs)

    def infer_type(self, scope_handler: ScopeHandler, **kwargs) -> InferredType:
        from SPPCompiler.SemanticAnalysis.ASTs import ConventionMovAst
        return InferredType(convention=ConventionMovAst, type=CommonTypes.void(self.pos))


__all__ = ["UseStatementAst"]
