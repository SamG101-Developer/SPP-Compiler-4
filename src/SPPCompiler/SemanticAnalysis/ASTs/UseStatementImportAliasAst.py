from dataclasses import dataclass

from SPPCompiler.SemanticAnalysis.ASTs.Meta.Ast import Ast
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstPrinter import AstPrinter, ast_printer_method


@dataclass
class UseStatementImportAliasAst(Ast):
    as_keyword: "TokenAst"
    type: "TypeAst"

    def __post_init__(self):
        from SPPCompiler.SemanticAnalysis.ASTs.TypeAst import TypeAst
        self.type = TypeAst(self.pos, [], [self.type.to_generic_identifier()])

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        s = ""
        s += f"{self.as_keyword.print(printer)} "
        s += f"{self.type.print(printer)}"
        return s


__all__ = ["UseStatementImportAliasAst"]
