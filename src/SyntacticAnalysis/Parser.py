from __future__ import annotations
from typing import Callable

from SyntacticAnalysis.ParserRuleHandler import ParserRuleHandler
from src.LexicalAnalysis.Tokens import Token, TokenType
from src.SyntacticAnalysis.ParserRuleHandler import ParserRuleHandler
from src.SemanticAnalysis.ASTs.Ast import *


# Decorator that wraps the function in a ParserRuleHandler
def parser_rule(rule: ParserRuleHandler.ParserRule) -> Callable[[], ParserRuleHandler]:
    def wrapper(self) -> ParserRuleHandler:
        return ParserRuleHandler(self, rule)
    return wrapper


class Parser:
    _tokens: List[Token]
    _index: int

    def current_pos(self) -> int:
        return self._index

    @parser_rule
    def parse_eof(self) -> TokenAst:
        p1 = self.parse_token(TokenType.TkEOF).parse_once()
        return p1

    @parser_rule
    def parse_program(self) -> ProgramAst:
        c1 = self.current_pos()
        p1 = self.parse_module_prototype().parse_once()
        p2 = self.parse_eof().parse_once()
        return ProgramAst(c1, p1, p2)

    @parser_rule
    def parse_module_prototype(self) -> ModulePrototypeAst:
        """
        [ModulePrototype] => [Annotation]* [Tok("mod")] [ModuleIdentifier] [ModuleMember]*

        - [Annotation]*      => The list of token stream mutations to make to the module before it is parsed.
        - [Tok("mod")]       => The 'mod' keyword.
        - [ModuleIdentifier] => The identifier of the module.
        - [ModuleMember]*    => The list of members that form the implementation of the module.

        The ModulePrototype contains the members that form a single module. It is a "top-level" AST, because the
        ProgramAst stores a ModulePrototypeAst instance directly.
        """
        c1 = self.current_pos()
        p1 = self.parse_annotation().parse_zero_or_more()
        p2 = self.parse_token(TokenType.TkModule).parse_once()
        p3 = self.parse_module_identifier().parse_once()
        p4 = self.parse_module_member().parse_zero_or_more()
        return ModulePrototypeAst(c1, p1, p2, p3, p4)

    @parser_rule
    def parse_module_member(self) -> ModuleMemberAst:
        p1 = self.parse_function_prototype().for_alt()
        p2 = self.parse_class_prototype().for_alt()
        p3 = self.parse_sup_prototype_normal().for_alt()
        p4 = self.parse_sup_prototype_inheritance().for_alt()
        p5 = self.parse_typedef_statement().for_alt()
        p6 = (p1 | p2 | p3 | p4 | p5).parse_once()
        return p6

    @parser_rule
    def parse_module_identifier(self) -> ModuleIdentifierAst:
        c1 = self.current_pos()
        p1 = self.parse_identifier().parse_one_or_more(TokenType.TkDot)
        return ModuleIdentifierAst(c1, p1)

    @parser_rule
    def parse_token(self, token_type: TokenType) -> TokenAst:
        ...
