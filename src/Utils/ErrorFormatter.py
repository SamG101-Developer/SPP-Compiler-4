from typing import List
from src.LexicalAnalysis.Tokens import Token


class ErrorFormatter:
    _tokens: List[Token]
    _file_path: str

    def __init__(self, tokens: List[Token], file_path: str) -> None:
        self._tokens = tokens
        self._file_path = file_path

    def error(self, start_pos: int, end_pos: int = -1, message: str = ""):
        error_line_start_pos = str(self._tokens[:start_pos]).rfind("\n") + 1
        error_line_end_pos = str(self._tokens[start_pos:]).find("\n") + start_pos
        error_line_tokens = self._tokens[error_line_start_pos:error_line_end_pos]

        error_line_number = str(self._tokens[:start_pos]).count("\n") + 1
        error_line_as_string = str(error_line_tokens)

        carets = "^" * len(error_line_tokens)
        carets_line_as_string = f"{carets} <- "
        carets_line_as_string = " " * len(str(error_line_tokens[:start_pos - error_line_start_pos])) + carets_line_as_string

        formatted_message = ""
        current_line = ""
        current_line_length = 0
        for word in message.split(" "):
            if current_line_length + len(word) > 80:
                formatted_message += f"{current_line}\n"
                current_line = " " * len(carets_line_as_string)
                current_line_length = 0
            current_line += f"{word} "
            current_line_length += len(word) + 1
        formatted_message += f"{current_line}\n"

        left_padding = " " * len(str(error_line_number))
        final_error_message = "\n".join([
            f"Error in file {self._file_path} on line {error_line_number}:",
            f" {left_padding} |",
            f"{error_line_number} | {error_line_as_string}",
            f" {left_padding} | {carets_line_as_string}{formatted_message}"
        ])

        return final_error_message
