from dataclasses import dataclass

from SPPCompiler.SemanticAnalysis.ASTs.Meta.Ast import Ast
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstPrinter import AstPrinter, ast_printer_method


@dataclass
class UseStatementImportBodyAst(Ast):
    type: "UseStatementImportTypesAst"

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        s = ""
        s += f"{self.type.print(printer)}"
        return s


__all__ = ["UseStatementImportBodyAst"]
