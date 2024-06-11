from dataclasses import dataclass
from typing import List, Any

from llvmlite import ir as llvm_ir

from SPPCompiler.SemanticAnalysis.ASTMixins.SemanticAnalyser import SemanticAnalyser
from SPPCompiler.SemanticAnalysis.ASTMixins.LLVMGeneration import LLVMGeneration
from SPPCompiler.SemanticAnalysis.ASTs.Meta.Ast import Ast
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstPrinter import *
from SPPCompiler.Utils.Sequence import Seq


@dataclass
class ModuleImplementationAst(Ast, SemanticAnalyser, LLVMGeneration):
    """
    The ModuleImplementationAst node represents the contents of a module, contained under a ModulePrototypeAst node.
    This includes "cls", "sup", "fun", and "use" blocks/statements.

    Attributes:
        members: The members of the module.
    """

    members: List["ModuleMemberAst"]

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        # Print the ModuleImplementationAst.
        s = ""
        s += f"{Seq(self.members).print(printer, "\n\n")}"
        return s

    def do_semantic_analysis(self, scope_handler, **kwargs) -> None:
        # Semantically analyse all the members of the module.
        Seq(self.members).for_each(lambda x: x.do_semantic_analysis(scope_handler, **kwargs))

    def do_llvm_generation(self, module: llvm_ir.Module, **kwargs) -> Any:
        # Generate the LLVM IR for all the members of the module.
        Seq(self.members).for_each(lambda x: x.do_llvm_generation(module, **kwargs))


__all__ = ["ModuleImplementationAst"]
