from dataclasses import dataclass
from typing import List, Any

from llvmlite import ir as llvm_ir

from src.SemanticAnalysis.ASTMixins.SemanticAnalyser import SemanticAnalyser
from src.SemanticAnalysis.ASTMixins.LLVMGeneration import LLVMGeneration
from src.SemanticAnalysis.ASTs.Meta.Ast import Ast
from src.SemanticAnalysis.ASTs.Meta.AstPrinter import *
from src.Utils.Sequence import Seq


@dataclass
class ModuleImplementationAst(Ast, SemanticAnalyser, LLVMGeneration):
    """
    The ModuleImplementationAst node represents the contents of a module, contained under a ModulePrototypeAst node.
    This includes "cls", "sup", "fun", and "use" blocks/statements.

    Attributes:
        - members: The members of the module.
    """

    members: List["ModuleMemberAst"]

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        # Print the ModuleImplementationAst.
        s = ""
        s += f"{Seq(self.members).print(printer, "\n\n")}"
        return s

    def do_semantic_analysis(self, scope_handler, **kwargs) -> None:
        # Do semantic analysis on all the members of the module.
        Seq(self.members).for_each(lambda x: x.do_semantic_analysis(scope_handler, **kwargs))

    def do_llvm_generation(self, module: llvm_ir.Module, **kwargs) -> Any:
        # Generate the LLVM IR for all the members of the module.
        Seq(self.members).for_each(lambda x: x.do_llvm_generation(module, **kwargs))


__all__ = ["ModuleImplementationAst"]
