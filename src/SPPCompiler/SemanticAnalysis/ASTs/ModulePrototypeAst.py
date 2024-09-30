from dataclasses import dataclass
from typing import List, Any

from llvmlite import ir as llvm_ir

from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstMixins import SemanticAnalyser, LLVMGeneration
from SPPCompiler.SemanticAnalysis.ASTs.Meta.Ast import Ast
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstPrinter import *
from SPPCompiler.Utils.Sequence import Seq


@dataclass
class ModulePrototypeAst(Ast, SemanticAnalyser, LLVMGeneration):
    """
    The ModulePrototypeAst node represents a module definition, containing the module identifier and implementation.
    Annotations can also be attached.

    Attributes:
        annotations: The annotations attached to the module.
        body: The module implementation.
    """

    annotations: List["AnnotationAst"]
    body: "ModuleImplementationAst"

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        # Print the ModulePrototypeAst.
        s = ""
        s += f"{Seq(self.annotations).print(printer, '\n')}\n"
        s += f"{self.body.print(printer)}"
        return s

    def do_semantic_analysis(self, scope_handler, **kwargs) -> None:
        # Semantically analyse the module identifier and implementation.
        self.body.do_semantic_analysis(scope_handler, **kwargs)


__all__ = ["ModulePrototypeAst"]
