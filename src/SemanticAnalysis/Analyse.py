import json

from SemanticAnalysis.Symbols.Scopes import ScopeHandler
from src.SemanticAnalysis.ASTs.Ast import ProgramAst


class Analyser:
    _ast: ProgramAst

    def __init__(self, ast: ProgramAst):
        self._ast = ast

    def analyse(self):
        root_context = self._ast.module
        scope_handler = ScopeHandler()

        self._ast.pre_process(root_context)
        self._ast.generate(scope_handler)
        with open("../bin/symbol_table.json", "w") as f:
            f.write(json.dumps(scope_handler.current_scope, indent=4))
