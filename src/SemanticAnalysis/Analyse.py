from src.SemanticAnalysis.ASTs.Ast import ProgramAst


class Analyser:
    _ast: ProgramAst

    def __init__(self, ast: ProgramAst):
        self._ast = ast

    def analyse(self):
        root_context = self._ast.module
        self._ast.pre_process(root_context)
