import dataclasses
import json
import os

from SPPCompiler.Compiler.ModuleTree import ModuleTree
from SPPCompiler.LexicalAnalysis.Lexer import Lexer
from SPPCompiler.SyntacticAnalysis.Parser import Parser
from SPPCompiler.SemanticAnalysis.ASTs.Meta import Ast
from SPPCompiler.SemanticAnalysis.Analyse import Analyser
from SPPCompiler.SemanticAnalysis.Utils.Scopes import ScopeHandler
from SPPCompiler.Utils.ProgressBar import ProgressBar


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
        # Create a list of the modules. Each stage is in its own loop to allow for errors to come in the right order, ie
        # all lexing errors then all parsing errors then all semantic errors.
        modules = []
        for module in self._module_tree:
            modules.append(module)
        module_count = len(modules)

        # Create a list of the lexed tokens.
        lexed = []
        lexed_progress_bar = ProgressBar("Lexing..............", module_count)
        for module in self._module_tree:
            lexed_progress_bar.next(module)
            code = open(module).read()
            tokens = Lexer(code).lex()
            lexed.append(tokens)
            self._write_to_file(module, "tokens", [dataclasses.asdict(token) for token in tokens])

        # Create a list of the parsed asts.
        parsed = []
        parsed_progress_bar = ProgressBar("Parsing.............", module_count)
        for module, tokens in zip(modules, lexed):
            parsed_progress_bar.next(module)
            ast = Parser(tokens, module).parse()
            parsed.append(ast)
            self._write_to_file(module, "trees", dataclasses.asdict(ast))

        # Create a list of the analysers. Semantic analysis is done in two stages, so don't execute any analysis yet.
        analysers = []
        for module, tokens, ast in zip(modules, lexed, parsed):
            analysers.append(Analyser(module, tokens, ast))

        # Get the root ScopeHandler by analysing the first module: "main.spp". Stage 1 analysis includes preprocessing
        # and symbol generation, creating the ScopeHandler. This is needed to inject the modules into, in their own
        # scoped namespaced. Reset scope to move out of namespace.
        scope_handler = ScopeHandler()
        analyser_1_progress_bar = ProgressBar("Symbol generation...", module_count)
        for module, analyser in zip(modules, analysers):
            analyser_1_progress_bar.next(module)
            analyser.stage_1_analysis(scope_handler)
            scope_handler.reset()
        self._write_to_file(self._src_path + os.sep + "symbols1.json", "symbols", scope_handler.global_scope)

        analyser_2_progress_bar = ProgressBar("Superimpositions....", module_count)
        for module, analyser in zip(modules, analysers):
            analyser_2_progress_bar.next(module)
            # Count how many modules come before this module in the innermost namespace of the module. This is to ensure
            # that the net scope inspected is the scope of this module, not the first class in the same namespace.
            current_module = module
            current_module_directory = os.sep.join(current_module.split(os.sep)[:-1])
            counter = 0

            for check_module, a in zip(modules, analysers):
                check_module_directory = os.sep.join(check_module.split(os.sep)[:-1])
                if check_module == current_module: break
                if check_module_directory == current_module_directory: counter += a.num_children_introduced

            analyser.stage_2_analysis(scope_handler, counter)
            scope_handler.reset()
        self._write_to_file(self._src_path + os.sep + "symbols2.json", "symbols", scope_handler.global_scope)

        # Stage 2 analysis is the semantic analysis, which is done on all modules. Reset scope to move out of namespace.
        analyser_3_progress_bar = ProgressBar("Semantic analysis...", module_count)
        for module, analyser in zip(modules, analysers):
            analyser_3_progress_bar.next(module)

            # Count how many modules come before this module in the innermost namespace of the module. This is to ensure
            # that the net scope inspected is the scope of this module, not the first class in the same namespace.
            current_module = module
            current_module_directory = os.sep.join(current_module.split(os.sep)[:-1])
            counter = 0

            for check_module, a in zip(modules, analysers):
                check_module_directory = os.sep.join(check_module.split(os.sep)[:-1])
                if check_module == current_module: break
                if check_module_directory == current_module_directory: counter += a.num_children_introduced

            # Analyse the module.
            analyser.stage_3_analysis(scope_handler, counter)
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
        # print(directory_path)

        directory_path = os.sep.join(file_path.split(os.sep)[:-1])
        os.path.exists(directory_path) or os.makedirs(directory_path)
        with open(file_path, "w") as file:
            file.write(json_repr)
