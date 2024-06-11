from abc import ABC, abstractmethod


class PreProcessor(ABC):
    @abstractmethod
    def pre_process(self, context) -> None:
        pass
