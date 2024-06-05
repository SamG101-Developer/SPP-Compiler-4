from dataclasses import dataclass

from SPPCompiler.SemanticAnalysis.ASTMixins.SemanticAnalyser import SemanticAnalyser
from SPPCompiler.SemanticAnalysis.Utils.Scopes import ScopeHandler
from SPPCompiler.SemanticAnalysis.ASTMixins.SymbolGeneration import SymbolGenerator
from SPPCompiler.SemanticAnalysis.ASTMixins.PreProcessor import PreProcessor

from SPPCompiler.SemanticAnalysis.ASTs.Meta.Ast import Ast
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstPrinter import *

from SPPCompiler.SemanticAnalysis.ASTs.FunctionPrototypeAst import FunctionPrototypeAst
from SPPCompiler.SemanticAnalysis.ASTs.ModulePrototypeAst import ModulePrototypeAst
from SPPCompiler.SemanticAnalysis.ASTs.TokenAst import TokenAst

from SPPCompiler.Utils.Sequence import Seq


@dataclass
class ProgramAst(Ast, PreProcessor, SymbolGenerator, SemanticAnalyser):
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
        # Semantically analyse the module.
        self.module.do_semantic_analysis(scope_handler, **kwargs)


__all__ = ["ProgramAst"]
