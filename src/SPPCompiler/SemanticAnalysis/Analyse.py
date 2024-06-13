import os.path
from typing import NoReturn, List, Callable

from SPPCompiler.LexicalAnalysis.Tokens import Token
from SPPCompiler.SemanticAnalysis.Utils.SemanticError import SemanticError, SemanticErrorStringFormatType
from SPPCompiler.SemanticAnalysis.Utils.Scopes import ScopeHandler
from SPPCompiler.SemanticAnalysis.ASTs import ProgramAst
from SPPCompiler.Utils.ErrorFormatter import ErrorFormatter
from SPPCompiler.Utils.Sequence import Seq


class Analyser:
    _file_name: str
    _tokens: list[Token]
    _ast: ProgramAst
    _err_fmt: ErrorFormatter

    def __init__(self, file_name: str, tokens: list[Token], ast: ProgramAst):
        self._file_name: str = file_name
        self._tokens = tokens
        self._ast = ast
        self._err_fmt = ErrorFormatter(tokens, file_name)

    def _stage_n_analysis(self, scope_handler: ScopeHandler, functions: List[Callable], **kwargs):
        self.move_scope_handler_to_namespace(scope_handler)
        try:
            for func in functions: func()
        except SemanticError as e:
            handle_semantic_error(self._err_fmt, e)

    def stage_0_analysis(self, scope_handler: ScopeHandler) -> None:
        self._stage_n_analysis(scope_handler, [])

    def stage_1_analysis(self, scope_handler: ScopeHandler) -> None:
        self._stage_n_analysis(scope_handler, [lambda: self._ast.pre_process(self._ast.module), lambda: self._ast.generate(scope_handler)])

    def stage_2_analysis(self, scope_handler: ScopeHandler) -> None:
        self._stage_n_analysis(scope_handler, [lambda: self._ast.sup_scope_gen(scope_handler)])

    def stage_3_analysis(self, scope_handler: ScopeHandler) -> None:
        self._stage_n_analysis(scope_handler, [lambda: self._ast.do_semantic_analysis(scope_handler)])

    def move_scope_handler_to_namespace(self, scope_handler: ScopeHandler) -> None:
        # Get the relevant parts of the namespace: from "src" to the deepest folder (not filename).
        from SPPCompiler.SemanticAnalysis.ASTs import IdentifierAst
        module_namespace = self._file_name.split(os.path.sep)
        module_namespace = module_namespace[module_namespace.index("src") + 1:]
        module_namespace[-1] = module_namespace[-1].replace(".spp", "")

        # Either move into the correct scope, or create the new scope, for each part of the namespace.
        for part in Seq(module_namespace).map(lambda p: IdentifierAst(-1, p)):
            if Seq(scope_handler.current_scope._children_scopes).map(lambda s: s._scope_name).contains(part):
                scope = Seq(scope_handler.current_scope._children_scopes).filter(lambda s: s._scope_name == part).first()
                scope_handler.reset(scope)
            else:
                scope_handler.into_new_scope(part)


def handle_semantic_error(err_fmt: ErrorFormatter, exception: SemanticError) -> NoReturn:
    final_error = ""
    for error in exception.additional_info:
        final_error += err_fmt.error(
            error[0], message=error[1],
            tag_message=error[2],
            minimal=error[3] == SemanticErrorStringFormatType.MINIMAL,
            no_format=error[3] == SemanticErrorStringFormatType.NO_FORMAT)

    raise SystemExit(final_error) from None
