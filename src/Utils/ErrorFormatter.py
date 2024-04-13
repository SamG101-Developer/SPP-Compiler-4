from typing import List

from colorama import Fore, Style

from src.LexicalAnalysis.Tokens import Token, TokenType


class ErrorFormatter:
    _tokens: List[Token]
    _file_path: str

    def __init__(self, tokens: List[Token], file_path: str) -> None:
        self._tokens = tokens
        self._file_path = file_path

    def error(self, start_pos: int, end_pos: int = -1, message: str = "", minimal: bool = False, no_format: bool = False) -> str:
        if no_format:
            return message
        while self._tokens[start_pos].token_type in [TokenType.TkNewLine, TokenType.TkWhitespace]:
            start_pos += 1

        # Get the tokens at the start and end of the line containing the error. Skip the leading newline.
        error_line_start_pos = [i for i, x in enumerate(self._tokens[:start_pos]) if x.token_type == TokenType.TkNewLine][-1] + 1
        error_line_end_pos = ([i for i, x in enumerate(self._tokens[start_pos:]) if x.token_type == TokenType.TkNewLine] or [len(self._tokens) - 1])[0] + start_pos
        error_line_tokens = self._tokens[error_line_start_pos:error_line_end_pos]
        error_line_as_string = "".join([str(token) for token in error_line_tokens])

        # Get the line number of the error
        error_line_number = len([x for x in self._tokens[:start_pos] if x.token_type == TokenType.TkNewLine])

        # The number of "^" is the length of the token data where the error is.
        carets = "^" * len(self._tokens[start_pos].token_metadata)
        carets_line_as_string = f"{carets} <- "
        carets_line_as_string = " " * sum([len(str(token)) for token in self._tokens[error_line_start_pos : start_pos]]) + carets_line_as_string

        # todo: comment the rest because I FORGOT HOW IT WORKS

        formatted_message = ""
        current_line = ""
        current_line_length = 0
        for word in message.split(" "):
            if current_line_length + len(word) > 120:
                formatted_message += f"{current_line}\n"
                current_line = " " * (len(carets_line_as_string) + len(" <- "))
                current_line_length = 0
            current_line += f"{word} "
            current_line_length += len(word) + 1
        formatted_message += f"{current_line}\n"

        left_padding = " " * len(str(error_line_number))
        final_error_message = "\n".join([
            f"\n\n{Fore.WHITE}{Style.BRIGHT}Error in file {self._file_path} on line {error_line_number}:" * (not minimal),
            f"{left_padding} |",
            f"{Fore.RED}{error_line_number} | {error_line_as_string}",
            f"{left_padding} | {Style.RESET_ALL}{carets_line_as_string}{formatted_message}"
        ])

        return final_error_message
