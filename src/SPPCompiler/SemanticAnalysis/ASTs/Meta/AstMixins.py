from typing import Any

from SPPCompiler.SemanticAnalysis.Utils.Scopes import ScopeHandler

import llvmlite.ir as llvm_ir


class PreProcessor:
    def pre_process(self, context) -> None:
        pass


class SymbolGenerator:
    def generate(self, scope_handler: ScopeHandler) -> None:
        pass


class SemanticAnalyser:
    def do_semantic_analysis(self, scope_handler: ScopeHandler, **kwargs) -> None:
        pass


class LLVMGeneration:
    def do_llvm_generation(self, module: llvm_ir.Module, **kwargs) -> Any:
        pass


class SupScopeLoader:
    def load_sup_scopes(self, scope_handler: ScopeHandler) -> None:
        scope_handler.move_to_next_scope()
        scope_handler.exit_cur_scope()


__all__ = ["PreProcessor", "SymbolGenerator", "SemanticAnalyser", "LLVMGeneration", "SupScopeLoader"]
