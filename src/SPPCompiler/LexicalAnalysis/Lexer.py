from SPPCompiler.LexicalAnalysis.Tokens import Token, TokenType
import re


class Lexer:
    _code: str

    def __init__(self, code: str) -> None:
        self._code = code.replace("\t", "    ")

    def lex(self) -> list[Token]:
        current = 0
        output = []

        tokens   = [t for t in TokenType.__dict__["_member_names_"] if t[:2] == "Tk"]
        keywords = [t for t in TokenType.__dict__["_member_names_"] if t[:2] == "Kw"]
        lexemes  = [t for t in TokenType.__dict__["_member_names_"] if t[:2] == "Lx"]

        tokens.sort(key=lambda t: len(TokenType[t].value), reverse=True)
        keywords.sort(key=lambda t: len(TokenType[t].value), reverse=True)

        available_tokens = keywords + lexemes + tokens

        while current < len(self._code):
            for token in available_tokens:
                value = getattr(TokenType, token).value
                upper = current + len(value)

                # Keywords: Match the keyword, and check that the next character isn't [A-Za-z_] (identifier).
                if token[:2] == "Kw" and self._code[current:upper] == value and not (self._code[upper].isalpha() or self._code[upper] == "_"):
                    output.append(Token(value, TokenType[token]))
                    current += len(value)
                    break

                # Lexemes: Match a lexeme by attempting to get a regex match against the current code. Discard comments.
                elif token[:2] == "Lx" and (matched := re.match(value, self._code[current:])):
                    if TokenType[token] not in [TokenType.LxSingleLineComment, TokenType.LxMultiLineComment]:
                        output.append(Token(matched.group(0), TokenType[token]))
                    if TokenType[token] == TokenType.LxMultiLineComment:
                        output.extend([Token("\n", TokenType.TkNewLine)] * matched.group(0).count("\n"))
                    current += len(matched.group(0))
                    break

                # Tokens: Match the token and increment the counter by the length of the token.
                elif token[:2] == "Tk" and self._code[current:upper] == value:
                    output.append(Token(value, TokenType[token]))
                    current += len(value)
                    break

            else:
                # Use n error token here, so that the error checker can use the same code to format the error when some
                # rule fails to parse, rather than trying to raise an error from here.
                output += [Token(self._code[current], TokenType.ERR)]
                current += 1

        return [Token("\n", TokenType.TkNewLine)] + output + [Token("<EOF>", TokenType.TkEOF)]
