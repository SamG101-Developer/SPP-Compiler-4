import dataclasses
import json
import os

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
        # Create a list of the modules. Each stage is in its own loop to allow for errors to come in the right order, ie
        # all lexing errors then all parsing errors then all semantic errors.
        modules = []
        for module in self._module_tree:
            modules.append(module)

        # Create a list of the lexed tokens.
        lexed = []
        for module in self._module_tree:
            code = open(module).read()
            tokens = Lexer(code).lex()
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

        # Get the root ScopeHandler by analysing the first module: "main.spp". Stage 1 analysis includes preprocessing
        # and symbol generation, creating the ScopeHandler. This is needed to inject the modules into, in their own
        # scoped namespaced. Reset scope to move out of namespace.
        scope_handler = ScopeHandler()
        for module, analyser in zip(modules, analysers):
            analyser.stage_1_analysis(scope_handler)
            scope_handler.reset()
        self._write_to_file(self._src_path + os.sep + "symbols1.json", "symbols", scope_handler.global_scope)

        # Stage 2 analysis is the semantic analysis, which is done on all modules. Reset scope to move out of namespace.
        for module, analyser in zip(modules, analysers):
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
            analyser.stage_2_analysis(scope_handler, counter)
            scope_handler.reset()

        self._write_to_file(self._src_path + os.sep + "symbols2.json", "symbols", scope_handler.global_scope)

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
