from dataclasses import dataclass
from typing import List, Any

from llvmlite import ir as llvm_ir

from src.SemanticAnalysis.ASTMixins.SemanticAnalyser import SemanticAnalyser
from src.SemanticAnalysis.ASTMixins.LLVMGeneration import LLVMGeneration
from src.SemanticAnalysis.ASTs.Meta.Ast import Ast
from src.SemanticAnalysis.ASTs.Meta.AstPrinter import *
from src.SemanticAnalysis.ASTs.AnnotationAst import AnnotationAst
from src.SemanticAnalysis.ASTs.TokenAst import TokenAst
from src.SemanticAnalysis.ASTs.ModuleIdentifierAst import ModuleIdentifierAst
from src.SemanticAnalysis.ASTs.ModuleImplementationAst import ModuleImplementationAst
from src.Utils.Sequence import Seq


@dataclass
class ModulePrototypeAst(Ast, SemanticAnalyser, LLVMGeneration):
    """
    The ModulePrototypeAst node represents a module definition, containing the module identifier and implementation.
    Annotations can also be attached.

    Attributes:
        - annotations: The annotations attached to the module.
        - module_keyword: The "mod" keyword token.
        - identifier: The module identifier.
        - body: The module implementation.
    """

    annotations: List[AnnotationAst]
    module_keyword: TokenAst
    identifier: ModuleIdentifierAst
    body: ModuleImplementationAst

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        # Print the ModulePrototypeAst.
        s = ""
        s += f"{Seq(self.annotations).print(printer, '\n')}\n"
        s += f"{self.module_keyword.print(printer)}"
        s += f"{self.identifier.print(printer)}\n"
        s += f"{self.body.print(printer)}"
        return s

    def do_semantic_analysis(self, scope_handler, **kwargs) -> None:
        Seq(self.annotations).for_each(lambda x: x.do_semantic_analysis(scope_handler, **kwargs))
        # self.identifier.do_semantic_analysis(s)
        self.body.do_semantic_analysis(scope_handler, **kwargs)

    def do_llvm_generation(self, module: llvm_ir.Module, **kwargs) -> Any:
        # Determine the module name and create the llvm module.
        llvm_module_name = Seq(self.identifier.parts).map(lambda p: p.value).join('.')
        llvm_module = llvm_ir.Module(name=llvm_module_name)
        self.body.do_llvm_generation(llvm_module, **kwargs)
        return llvm_module


__all__ = ["ModulePrototypeAst"]
