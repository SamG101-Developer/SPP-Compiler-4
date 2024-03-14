from dataclasses import dataclass

from src.SemanticAnalysis.Analysis.SemanticAnalysis import SemanticAnalysis
from src.SemanticAnalysis.Symbols.Scopes import ScopeHandler
from src.SemanticAnalysis.Symbols.SymbolGeneration import SymbolGenerator
from src.SemanticAnalysis.PreProcessor import PreProcessor

from src.SemanticAnalysis.ASTs.Meta.Ast import Ast
from src.SemanticAnalysis.ASTs.Meta.AstPrinter import *

from src.SemanticAnalysis.ASTs.FunctionPrototypeAst import FunctionPrototypeAst
from src.SemanticAnalysis.ASTs.ModulePrototypeAst import ModulePrototypeAst
from src.SemanticAnalysis.ASTs.TokenAst import TokenAst

from src.Utils.Sequence import Seq


@dataclass
class ProgramAst(Ast, PreProcessor, SymbolGenerator, SemanticAnalysis):
    """
    A ProgramAst node represents the root of the AST. It contains a module and an EOF token.

    Attributes:
        - module: The module.
        - eof_token: The EOF token.
    """

    module: ModulePrototypeAst
    eof_token: TokenAst

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        # Print the ProgramAst.
        s = ""
        s += f"{self.module.print(printer)}"
        return s

    def pre_process(self, context: ModulePrototypeAst) -> None:
        # Pre-process the module's members and remove all function prototypes (converted).
        Seq(self.module.body.members).for_each(lambda m: m.pre_process(context))
        self.module.body.members = Seq(self.module.body.members).filter(lambda m: not isinstance(m, FunctionPrototypeAst)).value

    def generate(self, s: ScopeHandler) -> None:
        # Generate the module's members.
        Seq(self.module.body.members).for_each(lambda m: m.generate(s))

    def do_semantic_analysis(self, scope_handler, **kwargs) -> None:
        # Analyse the module's members.
        self.module.do_semantic_analysis(scope_handler, **kwargs)


__all__ = ["ProgramAst"]
