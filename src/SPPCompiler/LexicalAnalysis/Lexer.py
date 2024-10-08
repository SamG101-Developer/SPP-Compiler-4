from SPPCompiler.LexicalAnalysis.Tokens import Token, TokenType
import re


_TKS   = [t for t in TokenType.__dict__.keys() if t[:2] == "Tk"]
_KWS = [t for t in TokenType.__dict__.keys() if t[:2] == "Kw"]
_LXS  = [t for t in TokenType.__dict__.keys() if t[:2] == "Lx"]

_TKS.sort(key=lambda t: len(TokenType[t].value), reverse=True)
_KWS.sort(key=lambda t: len(TokenType[t].value), reverse=True)

_AVAILABLE_TOKS = _KWS + _LXS + _TKS


class Lexer:
    _code: str

    def __init__(self, code: str) -> None:
        self._code = code.replace("\t", "    ")

    def lex(self) -> list[Token]:
        current = 0
        output = []

        while current < len(self._code):
            for token in _AVAILABLE_TOKS:
                value = getattr(TokenType, token).value
                upper = current + len(value)
                token_prefix = token[:2]

                # Keywords: Match the keyword, and check that the next character isn't [A-Za-z_] (identifier).
                if token_prefix == "Kw" and self._code[current:upper] == value and not (self._code[upper].isalpha() or self._code[upper] == "_"):
                    output.append(Token(value, TokenType[token]))
                    current += len(value)
                    break

                # Lexemes: Match a lexeme by attempting to get a regex match against the current code. Discard comments.
                elif token_prefix == "Lx" and (matched := re.match(value, self._code[current:])):
                    if TokenType[token] not in [TokenType.LxSingleLineComment, TokenType.LxMultiLineComment]:
                        output.append(Token(matched.group(0), TokenType[token]))
                    if TokenType[token] == TokenType.LxMultiLineComment:
                        output.extend([Token("\n", TokenType.TkNewLine)] * matched.group(0).count("\n"))
                    current += len(matched.group(0))
                    break

                # Tokens: Match the token and increment the counter by the length of the token.
                elif token_prefix == "Tk" and self._code[current:upper] == value:
                    output.append(Token(value, TokenType[token]))
                    current += len(value)
                    break

            else:
                # Use n error token here, so that the error checker can use the same code to format the error when some
                # rule fails to parse, rather than trying to raise an error from here.
                output += [Token(self._code[current], TokenType.ERR)]
                current += 1

        return [Token("\n", TokenType.TkNewLine)] + output + [Token("<EOF>", TokenType.TkEOF)]
