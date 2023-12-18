from __future__ import annotations
from typing import Callable, List, Optional

from src.LexicalAnalysis.Tokens import TokenType
from src.SyntacticAnalysis.Ast import Ast
from src.SyntacticAnalysis.Parser import Parser
from src.SyntacticAnalysis.ParserAlternateRulesHandler import ParserAlternateRulesHandler
from src.SyntacticAnalysis.ParserError import ParserError


class ParserRuleHandler:
    ParserRule = Callable[[], Ast]

    _rule: ParserRule
    _parser: Parser
    _for_alternate: bool
    _result: Optional[Ast]

    def __init__(self, parser: Parser, rule: ParserRule) -> None:
        self._parser = parser
        self._rule = rule
        self._for_alternate = False
        self._result = None

    def parse_once(self) -> Ast:
        self._result = self._rule()
        return self._result

    def parse_optional(self) -> Optional[Ast]:
        parser_index = self._parser._index
        try:
            self._result = self._rule()
            return self._result
        except ParserError:
            self._parser._index = parser_index
            return None

    def parse_zero_or_more(self, sep: TokenType = None) -> List[Ast]:
        self._result = []
        while ast := self.parse_optional():
            self._result.append(ast)
            sep and self._parser.parse_token(sep).parse_once()
        return self._result

    def parse_one_or_more(self, sep: TokenType = None) -> List[Ast]:
        self.parse_zero_or_more(sep)
        if not self._result:
            raise ParserError("Expected one or more.")
        return self._result

    def for_alt(self) -> ParserRuleHandler:
        self._for_alternate = True
        return self

    def __or__(self, that: ParserRuleHandler) -> ParserAlternateRulesHandler:
        if not (self._for_alternate and that._for_alternate):
            raise ParserError("Cannot use '|' operator on a non-alternate rule.")

        return (ParserAlternateRulesHandler(self._parser).for_alt()
                .add_parser_rule_handler(self)
                .add_parser_rule_handler(that))
