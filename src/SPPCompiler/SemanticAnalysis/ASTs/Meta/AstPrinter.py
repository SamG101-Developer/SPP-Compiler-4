import copy
import functools
import re
from typing import Final


class AstPrinter:
    _indent: int

    TAB_SIZE: Final[int] = 4

    def __init__(self):
        self._indent = 0
        self._code = ""

    def format_line(self, line: str):
        # Insert tabs after \n characters in parts of the code.
        return line.replace("\n", "\n" + " " * (AstPrinter.TAB_SIZE // 2), line.count("\n") - 1)

    def increment_indent(self):
        # Increase the indent by the tab size.
        self._indent += AstPrinter.TAB_SIZE

    def decrement_indent(self):
        # Decrease the indent by the tab size.
        self._indent -= AstPrinter.TAB_SIZE

    @property
    def code(self) -> str:
        return self._code


# Decorators for the printer methods
def ast_printer_method(func, next_indent: bool = False):
    @functools.wraps(func)
    def wrapper(self=None, *args):
        printer = args[0]

        next_indent and printer.increment_indent()
        line = func(self, *args)
        line = printer.format_line(line)
        next_indent and printer.decrement_indent()

        return line

    return wrapper


def ast_printer_method_indent(func):
    return ast_printer_method(func, True)


__all__ = [
    "AstPrinter",
    "ast_printer_method",
    "ast_printer_method_indent"]
