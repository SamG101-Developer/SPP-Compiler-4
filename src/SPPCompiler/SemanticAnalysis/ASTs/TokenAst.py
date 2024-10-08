from __future__ import annotations

import hashlib
from dataclasses import dataclass

from SPPCompiler.LexicalAnalysis.Tokens import Token, TokenType
from SPPCompiler.SemanticAnalysis.ASTs.Meta.Ast import Ast
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstPrinter import *


@dataclass
class TokenAst(Ast):
    """
    The TokenAst node represents a single token, which has been parsed by the parser. It is one of the lowest level ASTs
    alongside the IdentifierAst and some types under the LiteralAst union.

    Attributes:
        token: The Token instance that was parsed.
    """

    token: Token

    @staticmethod
    def dummy(token_type: TokenType, info=None, pos=-1) -> TokenAst:
        # Quick way to create a token ast for a given token type.
        return TokenAst(pos, Token(info or token_type.value, token_type))

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        # Print the token metadata, and add a space after the token if it is a keyword.
        return self.token.token_metadata

    def __eq__(self, other):
        # Check both ASTs are the same type and have the same token types and metadata if they're lexemes.
        return (
                isinstance(other, TokenAst)
                and self.token.token_type == other.token.token_type
                and (self.token.token_metadata == other.token.token_metadata if self.token.token_type.name.startswith("Lx") else True))

    def __hash__(self):
        # The hash must be fixed, so that the same identifiers in different IdentifierAst objects have the same hash.
        return int.from_bytes(hashlib.md5(self.token.token_type.name.encode()).digest())


__all__ = ["TokenAst"]
