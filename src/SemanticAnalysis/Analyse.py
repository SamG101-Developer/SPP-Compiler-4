import json

from src.LexicalAnalysis.Tokens import Token
from src.SemanticAnalysis.Analysis.SemanticError import SemanticError
from src.SemanticAnalysis.Symbols.Scopes import ScopeHandler
from src.SemanticAnalysis.ASTs.Ast import ProgramAst
from src.Utils.ErrorFormatter import ErrorFormatter


class Analyser:
    _tokens: list[Token]
    _ast: ProgramAst

    def __init__(self, tokens: list[Token], ast: ProgramAst):
        self._tokens = tokens
        self._ast = ast

    def analyse(self):
        root_context = self._ast.module
        scope_handler = ScopeHandler()
        err_fmt = ErrorFormatter(self._tokens, "<filename>")

        self._ast.pre_process(root_context)
        self._ast.generate(scope_handler)

        with open("../bin/symbol_table.json", "w") as f:
            f.write(json.dumps(scope_handler.current_scope, indent=4))

        try:
            self._ast.do_semantic_analysis(iter(scope_handler))
        except SemanticError as e:
            final_error = str(e)
            for error in e.additional_info:
                final_error += err_fmt.error(error[0], message=error[1])
            raise SystemExit(final_error) from None
