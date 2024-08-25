from dataclasses import dataclass
from typing import List

from SPPCompiler.SemanticAnalysis.ASTs.Meta.Ast import Ast
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstPrinter import AstPrinter, ast_printer_method
from SPPCompiler.Utils.Sequence import Seq


@dataclass
class UseStatementImportMultipleTypesAst(Ast):
    namespace: List["IdentifierAst"]
    left_brace_token: "TokenAst"
    types: List["UseStatementImportBodyAst"]
    right_brace_token: "TokenAst"

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        s = ""
        s += f"{Seq(self.namespace).print(printer, "::")}::" if self.namespace else ""
        s += f"{self.left_brace_token.print(printer)}"
        s += f"{Seq(self.types).print(printer, ", ")}"
        s += f"{self.right_brace_token.print(printer)}"
        return s


__all__ = ["UseStatementImportMultipleTypesAst"]
