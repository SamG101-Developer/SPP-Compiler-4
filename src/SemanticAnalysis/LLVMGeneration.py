from abc import ABC, abstractmethod
from typing import Any

import llvmlite.ir as llvm_ir


class LLVMGeneration(ABC):
    @abstractmethod
    def do_llvm_generation(self, module: llvm_ir.Module, **kwargs) -> Any:
        pass
