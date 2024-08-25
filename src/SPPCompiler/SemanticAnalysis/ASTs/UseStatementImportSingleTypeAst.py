from dataclasses import dataclass
from typing import List, Optional

from SPPCompiler.SemanticAnalysis.ASTs.Meta.Ast import Ast
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstPrinter import AstPrinter, ast_printer_method
from SPPCompiler.Utils.Sequence import Seq


@dataclass
class UseStatementImportSingleTypeAst(Ast):
    namespace: List["IdentifierAst"]
    types: List["GenericIdentifierAst"]
    alias: Optional["UseStatementImportAliasAst"]

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        s = ""
        s += f"{Seq(self.namespace).print(printer, "::")}::" if self.namespace else ""
        s += f"{self.types[0].print(printer)}"
        s += f" {self.alias.print(printer)}" if self.alias else ""
        return s


__all__ = ["UseStatementImportSingleTypeAst"]
