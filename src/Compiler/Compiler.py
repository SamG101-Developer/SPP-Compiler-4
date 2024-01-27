from src.LexicalAnalysis.Lexer import Lexer
from src.SyntacticAnalysis.Parser import Parser
from src.SemanticAnalysis.ASTs import Ast
from src.SemanticAnalysis.Analyse import Analyser


class Compiler:
    _code: str
    _file_path: str

    def __init__(self, code: str, file_path: str) -> None:
        self._code = code
        self._file_path = file_path

    def compile(self) -> Ast:
        tokens = Lexer(self._code).lex()
        ast = Parser(tokens, self._file_path).parse()
        Analyser(tokens, ast).analyse()
        return ast
