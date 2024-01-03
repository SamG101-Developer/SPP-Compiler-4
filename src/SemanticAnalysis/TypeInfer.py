from abc import ABC, abstractmethod


class TypeInfer(ABC):
    @abstractmethod
    def infer_type(self) -> TypeAst:
        pass
