import os.path
from typing import NoReturn

from src.LexicalAnalysis.Tokens import Token
from src.SemanticAnalysis.Analysis.SemanticError import SemanticError
from src.SemanticAnalysis.Symbols.Scopes import ScopeHandler
from src.SemanticAnalysis.ASTs import ProgramAst
from src.Utils.ErrorFormatter import ErrorFormatter

from src.Utils.Sequence import Seq


class Analyser:
    _file_name: str
    _tokens: list[Token]
    _ast: ProgramAst

    def __init__(self, file_name: str, tokens: list[Token], ast: ProgramAst):
        self._file_name: str = file_name
        self._tokens = tokens
        self._ast = ast

    def stage_1_analysis(self, scope_handler: ScopeHandler) -> None:
        # Create the error formatter to pickup any errors raised during analysis. These will need to be formatted
        # specifically.
        err_fmt = ErrorFormatter(self._tokens, self._file_name)
        self.move_scope_handler_to_namespace(scope_handler)

        # Preprocess and generate the symbols & scopes for the module. If there is an error, then handle it.
        try:
            self._ast.pre_process(self._ast.module)
            self._ast.generate(scope_handler)
        except SemanticError as e:
            handle_semantic_error(err_fmt, e)

        # Reset the scope to the global scope for the next module to be analysed.
        scope_handler.reset()

    def stage_2_analysis(self, scope_handler: ScopeHandler) -> None:
        # Create the error formatter to pickup any errors raised during analysis. These will need to be formatted
        # specifically.
        err_fmt = ErrorFormatter(self._tokens, self._file_name)
        self.move_scope_handler_to_namespace(scope_handler)

        # Semantic analysis is done on the ast. If there is an error, then handle it.
        try:
            self._ast.do_semantic_analysis(scope_handler)

        except SemanticError as e:
            handle_semantic_error(err_fmt, e)

    def move_scope_handler_to_namespace(self, scope_handler: ScopeHandler):
        # Create the module namespace by splitting the module name based on the path separator. The module namespace is
        # the path to the module, relative to the src path, without the file name and without the src folder itself.

        from src.SemanticAnalysis.ASTs import IdentifierAst
        module_namespace = self._file_name.split(os.path.sep)
        module_namespace = module_namespace[module_namespace.index("src") + 1 : -1]

        # For each part in the module namespace, if the current scope has a child scope with the same name, set the
        # current scope to that child scope. Otherwise, create a new scope with the name of the part and set the current
        # scope to that new scope.
        for part in module_namespace:
            part = IdentifierAst(-1, part)
            if Seq(scope_handler.current_scope._children_scopes).map(lambda s: s._scope_name).contains(part):
                scope = Seq(scope_handler.current_scope._children_scopes).filter(lambda s: s._scope_name == part).first()
                scope_handler.reset(scope)
            else:
                scope_handler.into_new_scope(part)


def handle_semantic_error(err_fmt: ErrorFormatter, exception: SemanticError) -> NoReturn:
    final_error = str(exception)
    for error in exception.additional_info:
        final_error += err_fmt.error(error[0], message=error[1], minimal=error[2])
    for error in exception.next_exceptions:
        final_error += f"\n\n{error}"
        for inner_error in error.additional_info:
            final_error += err_fmt.error(inner_error[0], message=inner_error[1], minimal=inner_error[2])
    raise SystemExit(final_error) from None
