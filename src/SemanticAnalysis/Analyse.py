import dataclasses
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

        with open("../bin/ast.json", "w") as f:
            f.write(json.dumps(dataclasses.asdict(self._ast), indent=4))

        self._ast.pre_process(root_context)
        self._ast.generate(scope_handler)
        scope_handler.reset()

        with open("../bin/symbol_table_pre.json", "w") as f:
            f.write(json.dumps(scope_handler.current_scope, indent=4))

        try:
            self._ast.do_semantic_analysis(scope_handler)
            with open("../bin/symbol_table.json", "w") as f:
                f.write(json.dumps(scope_handler.current_scope, indent=4))

        except SemanticError as e:
            final_error = str(e)
            for error in e.additional_info:
                final_error += err_fmt.error(error[0], message=error[1], minimal=error[2])
            for error in e.next_exceptions:
                final_error += f"\n\n{error}"
                for inner_error in error.additional_info:
                    final_error += err_fmt.error(inner_error[0], message=inner_error[1], minimal=inner_error[2])
            raise SystemExit(final_error) from None
