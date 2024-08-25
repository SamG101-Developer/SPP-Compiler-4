from dataclasses import dataclass
from typing import NoReturn

from SPPCompiler.LexicalAnalysis.Tokens import TokenType
from SPPCompiler.SemanticAnalysis.ASTs.Meta.Ast import Ast
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstMixins import SemanticAnalyser
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstPrinter import AstPrinter, ast_printer_method
from SPPCompiler.SemanticAnalysis.Utils.Scopes import ScopeHandler


@dataclass
class UseStatementTypeAliasAst(Ast, SemanticAnalyser):
    new_type: "TypeAst"
    generic_parameters: "GenericParameterGroupAst"
    assignment_token: "TokenAst"
    old_type: "TypeAst"

    def __post_init__(self):
        from SPPCompiler.SemanticAnalysis.ASTs import TypeAst
        self.new_type = TypeAst(self.pos, [], [self.new_type.to_generic_identifier()])
        raise NotImplementedError("This is not implemented yet.")

    @ast_printer_method
    def print(self, printer: AstPrinter) -> NoReturn:
        s = ""
        s += f"{self.generic_parameters.print(printer)} "
        s += f"{self.new_type.print(printer)} "
        s += f"{self.assignment_token.print(printer)} "
        s += f"{self.old_type.print(printer)} "
        return s

    def do_semantic_analysis(self, scope_handler: ScopeHandler, **kwargs) -> None:
        """
        Take the example "use MyVector[T] = std::Vec[T]". This will create a "cls MyVector[T] { }" and
        "sup [T] std::Vec[T] on MyVector[T]".
        """

        # from SPPCompiler.SemanticAnalysis.ASTs import ClassPrototypeAst, SupPrototypeInheritanceAst, TokenAst
        # ast_1 = ClassPrototypeAst(self.pos, [], TokenAst.dummy(TokenType.KwCls), self.new_type.types[-1].to_identifier(), self.generic_parameters, None, None)
        # ast_2 = SupPrototypeInheritanceAst(self.pos, TokenAst.dummy(TokenType.KwSup), self.generic_parameters, self.new_type, None, None, self.old_type, TokenAst.dummy(TokenType.KwOn))


__all__ = ["UseStatementTypeAliasAst"]
