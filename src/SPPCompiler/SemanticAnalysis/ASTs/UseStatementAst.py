from dataclasses import dataclass
from typing import NoReturn

from SPPCompiler.SemanticAnalysis.ASTs.Meta.Ast import Ast
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstMixins import SemanticAnalyser
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstPrinter import AstPrinter, ast_printer_method
from SPPCompiler.SemanticAnalysis.Utils.Scopes import ScopeHandler


@dataclass
class UseStatementAst(Ast, SemanticAnalyser):
    use_keyword: "TokenAst"
    body: "UseStatementImportAst | UseStatementTypeAliasAst"

    @ast_printer_method
    def print(self, printer: AstPrinter) -> NoReturn:
        s = ""
        s += f"{self.use_keyword.print(printer)} "
        s += f"{self.body.print(printer)}"

    def do_semantic_analysis(self, scope_handler: ScopeHandler, **kwargs) -> None:
        # Pass the analysis onto the body (can be either a type or import)
        self.body.do_semantic_analysis(scope_handler, **kwargs)


__all__ = ["UseStatementAst"]
