import os
from typing import NoReturn

from SPPCompiler.Utils.ErrorFormatter import ErrorFormatter
from SPPCompiler.SemanticAnalysis.Utils.SemanticError import SemanticError


def format_semantic_error(err_fmt: ErrorFormatter, exception: SemanticError) -> str:
    from SPPCompiler.SemanticAnalysis.Utils.SemanticError import SemanticErrorStringFormatType
    final_error = "\n"
    for error in exception.additional_info:
        final_error += err_fmt.error(
            error[0], message=error[1],
            tag_message=error[2],
            minimal=error[3] == SemanticErrorStringFormatType.MINIMAL,
            no_format=error[3] == SemanticErrorStringFormatType.NO_FORMAT)
    return final_error


def handle_semantic_error(err_fmt: ErrorFormatter, exception: SemanticError) -> NoReturn:
    final_error = format_semantic_error(err_fmt, exception)
    raise SystemExit(final_error) from None
