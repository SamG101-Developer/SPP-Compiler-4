import dataclasses
import json
import os
from typing import List, Tuple

from SPPCompiler.LexicalAnalysis.Lexer import Lexer
from SPPCompiler.SyntacticAnalysis.Parser import Parser
from SPPCompiler.SemanticAnalysis.ASTs.Meta import Ast
from SPPCompiler.SemanticAnalysis.Analyse import Analyser
from SPPCompiler.SemanticAnalysis.Utils.Scopes import ScopeHandler
from SPPCompiler.Compiler.ModuleTree import ModuleTree


class Compiler:
    _src_path: str
    _module_tree: ModuleTree
    _mode: str

    def __init__(self, src_path: str, mode: str = "d") -> None:
        # Save the src path and generate the module tree.
        self._src_path = src_path
        self._module_tree = ModuleTree(src_path)
        self._mode = mode

        # Compile the modules.
        self.compile()

    def compile(self) -> Ast:
        args = self._lex_parse()
        self._analyse(*args)

    def _lex_parse(self) -> Tuple[List[str], List[Analyser], ScopeHandler]:
        # Create a list of the modules.
        modules = []
        for module in self._module_tree:
            modules.append(module)

        # Create a list of the lexed tokens.
        lexed = []
        for module in self._module_tree:
            tokens = Lexer(open(module).read()).lex()
            lexed.append(tokens)

        # Create a list of the parsed asts.
        parsed = []
        for module, tokens in zip(modules, lexed):
            ast = Parser(tokens, module).parse()
            parsed.append(ast)
            self._write_to_file(module, "trees", dataclasses.asdict(ast))

        # Create a list of the analysers. Semantic analysis is done in two stages, so don't execute any analysis yet.
        analysers = []
        for module, tokens, ast in zip(modules, lexed, parsed):
            analysers.append(Analyser(module, tokens, ast))

        scope_handler = ScopeHandler()
        return modules, analysers, scope_handler

    def _analyse(self, modules: List[str], analysers: List[Analyser], scope_handler: ScopeHandler) -> None:
        for module, analyser in zip(modules, analysers):
            analyser.stage_0_analysis(scope_handler)
            scope_handler.reset()
        self._write_to_file(self._src_path + os.sep + "symbols0.json", "symbols", scope_handler.global_scope)

        for module, analyser in zip(modules, analysers):
            analyser.stage_1_analysis(scope_handler)
            scope_handler.reset()
        self._write_to_file(self._src_path + os.sep + "symbols1.json", "symbols", scope_handler.global_scope)

        for module, analyser in zip(modules, analysers):
            analyser.stage_2_analysis(scope_handler)
            scope_handler.reset()
        self._write_to_file(self._src_path + os.sep + "symbols2.json", "symbols", scope_handler.global_scope)

        for module, analyser in zip(modules, analysers):
            analyser.stage_3_analysis(scope_handler)
            scope_handler.reset()
        self._write_to_file(self._src_path + os.sep + "symbols3.json", "symbols", scope_handler.global_scope)

    def _write_to_file(self, file_path: str, section: str, what) -> None:
        if self._mode == "r":
            return

        json_repr = json.dumps(what, indent=4)
        file_path_section = file_path.split(os.sep)
        file_path_sections = file_path_section[:file_path_section.index("src")] + ["bin", section] + file_path_section[file_path_section.index("src") + 1:]
        file_path = os.sep.join(file_path_sections)
        file_path = file_path.replace(".spp", ".json")

        directory_path = os.sep.join(file_path.split(os.sep)[:-1])
        os.path.exists(directory_path) or os.makedirs(directory_path)
        with open(file_path, "w") as file:
            file.write(json_repr)
