from typing import List
from src.LexicalAnalysis.Tokens import TokenType


class ParserError(Exception):
    pos: int
    expected_tokens: List[str]

    def __init__(self, *args) -> None:
        super().__init__(*args)
        self.pos = -1
        self.expected_tokens = []
