from src.LexicalAnalysis.Lexer import Lexer
from src.SyntacticAnalysis.Parser import Parser
from src.SemanticAnalysis.ASTs import Ast
from src.SemanticAnalysis.Analyse import Analyser
from src.SemanticAnalysis.Symbols.Scopes import ScopeHandler
from src.Compiler.ModuleTree import ModuleTree


class Compiler:
    _src_path: str
    _module_tree: ModuleTree

    def __init__(self, src_path: str) -> None:
        # Save the src path and generate the module tree.
        self._src_path = src_path
        self._module_tree = ModuleTree(src_path)

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

        # Create a list of the analysers. Semantic analysis is done in two stages, so don't execute any analysis yet.
        analysers = []
        for module, tokens, ast in zip(modules, lexed, parsed):
            analysers.append(Analyser(module, tokens, ast))

        # Get the root ScopeHandler by analysing the first module: "main.spp". Stage 1 analysis includes preprocessing
        # and symbol generation, creating the ScopeHandler. This is needed to inject the modules into, in their own
        # scoped namespaced.
        scope_handler = ScopeHandler()
        for analyser in analysers:
            analyser.stage_1_analysis(scope_handler)

        # Stage 2 analysis is the semantic analysis, which is done on all modules.
        for analyser in analysers:
            analyser.stage_2_analysis(scope_handler)
