import os.path
from typing import NoReturn

from SPPCompiler.LexicalAnalysis.Tokens import Token
from SPPCompiler.SemanticAnalysis.Utils.SemanticError import SemanticError, SemanticErrorStringFormatType
from SPPCompiler.SemanticAnalysis.Utils.Scopes import ScopeHandler
from SPPCompiler.SemanticAnalysis.ASTs import ProgramAst
from SPPCompiler.Utils.ErrorFormatter import ErrorFormatter
from SPPCompiler.SemanticAnalysis.Utils.Symbols import NamespaceSymbol

from SPPCompiler.Utils.Sequence import Seq


class Analyser:
    _file_name: str
    _tokens: list[Token]
    _ast: ProgramAst
    _num_children_introduced: int

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
            cur_num_children = len(scope_handler.current_scope._children_scopes)
            self._ast.pre_process(self._ast.module)
            self._ast.generate(scope_handler)

            scope_handler.reset()

            # Save the number of child scopes that have just been introduced, as this is needed later for the 2nd
            # semantic analysis pass. It is needed to skip over these new child scopes to reach the target scope of
            # a specific module.
            self.move_scope_handler_to_namespace(scope_handler)
            new_num_children = len(scope_handler.current_scope._children_scopes)
            self._num_children_introduced = new_num_children - cur_num_children

        except SemanticError as e:
            handle_semantic_error(err_fmt, e)

    def stage_2_analysis(self, scope_handler: ScopeHandler, shift_count: int) -> None:
        # Create the error formatter to pickup any errors raised during analysis. These will need to be formatted
        # specifically.
        err_fmt = ErrorFormatter(self._tokens, self._file_name)
        self.move_scope_handler_to_namespace(scope_handler)
        self.move_scope_handler_to_end_of_scope(scope_handler, shift_count)

        # Semantic analysis is done on the ast. If there is an error, then handle it.
        try:
            self._ast.do_semantic_analysis(scope_handler)
        except SemanticError as e:
            handle_semantic_error(err_fmt, e)

    def move_scope_handler_to_namespace(self, scope_handler: ScopeHandler):
        # Create the module namespace by splitting the module name based on the path separator. The module namespace is
        # the path to the module, relative to the src path, without the file name and without the src folder itself.

        from SPPCompiler.SemanticAnalysis.ASTs import IdentifierAst
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
                scope_handler.current_scope.add_symbol(namespace_symbol := NamespaceSymbol(name=part))
                namespace_scope = scope_handler.into_new_scope(part)
                namespace_symbol.associated_scope = namespace_scope

    def move_scope_handler_to_end_of_scope(self, scope_handler: ScopeHandler, shift_count: int):
        # This method takes a given scope and shifts the iterator and current scope to whatever "shift_count" is. This
        # means that if "shift_count" is 3, then 3 scopes will be skipped (scopes 0, 1 & 2). The current scope is then
        # set to the parent scope of these scopes, so the next "move_into_next_scope()" function call will set the
        # current scope to the 4th child scope of the parent scope.

        if shift_count > 0:

            # Get the target child scope, and set up a duplicate iterator that acts as a +1 lookahead iterator to the
            # main ScopeAHandler's iterator.
            target_child_scope = scope_handler.current_scope._children_scopes[shift_count]
            iter1 = iter(scope_handler)

            # Increment the lookahead iterator until it reaches the target child scope. The ScopeHandler's iterator is
            # now the scope directly before the target child scope, taking into account nested scopes, etc.
            target = None
            while next(iter1) != target_child_scope:
                scope_handler.move_to_next_scope()

            # Set the current scope to the parent scope of the target child scope.
            scope_handler.exit_cur_scope()

    @property
    def num_children_introduced(self) -> int:
        return self._num_children_introduced


def handle_semantic_error(err_fmt: ErrorFormatter, exception: SemanticError) -> NoReturn:
    final_error = ""
    for error in exception.additional_info:
        final_error += err_fmt.error(
            error[0], message=error[1],
            tag_message=error[2],
            minimal=error[3] == SemanticErrorStringFormatType.MINIMAL,
            no_format=error[3] == SemanticErrorStringFormatType.NO_FORMAT)

    raise SystemExit(final_error) from None
