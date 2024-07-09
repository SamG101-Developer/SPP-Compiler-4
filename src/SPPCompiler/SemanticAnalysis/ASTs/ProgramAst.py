from dataclasses import dataclass

from SPPCompiler.SemanticAnalysis.ASTs.Meta.Ast import Ast
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstMixins import PreProcessor, SemanticAnalyser, SupScopeLoader, SymbolGenerator
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstPrinter import *
from SPPCompiler.SemanticAnalysis.Utils.Scopes import ScopeHandler
from SPPCompiler.Utils.Sequence import Seq


@dataclass
class ProgramAst(Ast, PreProcessor, SymbolGenerator, SemanticAnalyser, SupScopeLoader):
    """
    A ProgramAst node represents the root of the AST. It contains a module and an EOF token.

    Attributes:
        module: The module.
        eof_token: The EOF token.
    """

    module: "ModulePrototypeAst"
    eof_token: "TokenAst"

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        # Print the ProgramAst.
        s = ""
        s += f"{self.module.print(printer)}"
        return s

    def pre_process(self, context: "ModulePrototypeAst") -> None:
        from SPPCompiler.SemanticAnalysis.ASTs import SubroutinePrototypeAst, CoroutinePrototypeAst

        # Pre-process the module's members and remove all function prototypes (converted).
        Seq(self.module.body.members).for_each(lambda m: m.pre_process(context))
        self.module.body.members = Seq(self.module.body.members).filter_not_type(SubroutinePrototypeAst, CoroutinePrototypeAst).value

    def generate(self, scope_handler: ScopeHandler) -> None:
        # Generate the module's members.
        Seq(self.module.body.members).for_each(lambda m: m.generate(scope_handler))

    def load_sup_scopes(self, scope_handler: ScopeHandler) -> None:
        # Load the module's members.
        Seq(self.module.body.members).for_each(lambda m: m.load_sup_scopes(scope_handler))

    def do_semantic_analysis(self, scope_handler, **kwargs) -> None:
        # Semantically analyse the module.
        self.module.do_semantic_analysis(scope_handler, **kwargs)


__all__ = ["ProgramAst"]
