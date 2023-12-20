from __future__ import annotations
from typing import Callable, List, Optional

from src.LexicalAnalysis.Tokens import TokenType
from src.SyntacticAnalysis.Ast import Ast
from src.SyntacticAnalysis.Parser import Parser
from src.SyntacticAnalysis.ParserAlternateRulesHandler import ParserAlternateRulesHandler
from src.SyntacticAnalysis.ParserError import ParserError


class ParserRuleHandler[T]:
    ParserRule = Callable[[], T]

    _rule: ParserRule
    _parser: Parser
    _for_alternate: bool
    _result: Optional[T]

    def __init__(self, parser: Parser, rule: ParserRule) -> None:
        self._parser = parser
        self._rule = rule
        self._for_alternate = False
        self._result = None

    def parse_once(self) -> T:
        self._result = self._rule()
        return self._result

    def parse_optional(self) -> Optional[T]:
        parser_index = self._parser._index
        try:
            self._result = self._rule()
            return self._result
        except ParserError:
            self._parser._index = parser_index
            return None

    def parse_zero_or_more(self, sep: TokenType = None) -> List[T]:
        self._result = []
        while ast := self.parse_optional():
            self._result.append(ast)
            sep and self._parser.parse_token(sep).parse_once()
        return self._result

    def parse_one_or_more(self, sep: TokenType = None) -> List[T]:
        self.parse_zero_or_more(sep)
        if not self._result:
            raise ParserError("Expected one or more.")
        return self._result

    def for_alt(self) -> ParserRuleHandler:
        self._for_alternate = True
        return self

    def and_then(self, wrapper_function) -> ParserRuleHandler:
        new_parser_rule_handler = ParserRuleHandler(self._parser, self._rule)
        new_parser_rule_handler._rule = lambda: wrapper_function(self._rule())
        return new_parser_rule_handler

    def __or__(self, that: ParserRuleHandler) -> ParserAlternateRulesHandler:
        if not (self._for_alternate and that._for_alternate):
            raise ParserError("Cannot use '|' operator on a non-alternate rule.")

        return (ParserAlternateRulesHandler(self._parser).for_alt()
                .add_parser_rule_handler(self)
                .add_parser_rule_handler(that))
