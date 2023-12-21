from __future__ import annotations
from typing import List

from src.SemanticAnalysis.ASTs import Ast
# from src.SyntacticAnalysis.Parser import Parser
from src.SyntacticAnalysis.ParserError import ParserError
from src.SyntacticAnalysis.ParserRuleHandler import ParserRuleHandler


class ParserAlternateRulesHandler(ParserRuleHandler):
    _parser_rule_handlers: List[ParserRuleHandler]

    def __init__(self, parser: Parser) -> None:
        super().__init__(parser, None)
        self._parser_rule_handlers = []

    def add_parser_rule_handler(self, parser_rule_handler: ParserRuleHandler) -> ParserAlternateRulesHandler:
        self._parser_rule_handlers.append(parser_rule_handler)
        return self

    def parse_once(self) -> Ast:
        for parser_rule_handler in self._parser_rule_handlers:
            parser_index = self._parser._index
            try:
                self._result = parser_rule_handler.parse_once()
                return self._result
            except ParserError:
                self._parser._index = parser_index
                continue
        raise ParserError(self._parser._index, "Expected one of the alternatives.")

    def __or__(self, that) -> ParserAlternateRulesHandler:
        if not (self._for_alternate and that._for_alternate):
            raise SystemExit("Cannot use '|' operator on a non-alternate rule.")

        return self.add_parser_rule_handler(that)
