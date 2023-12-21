from abc import ABC, abstractmethod
from src.SemanticAnalysis.ASTs.Ast import ModulePrototypeAst, SupPrototypeAst


PreProcessorContext = ModulePrototypeAst | SupPrototypeAst


class PreProcessor(ABC):
    @abstractmethod
    def process(self, text: str, context: PreProcessorContext) -> None:
        pass
