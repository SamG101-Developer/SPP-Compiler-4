from __future__ import annotations

from typing import Iterable, Callable


class Seq[T]:
    _value: Iterable[T]

    def __init__(self, value: Iterable[T]) -> None:
        self._value = value

    def for_each(self, func: Callable[[T], None]) -> None:
        for v in self._value: func(v)

    def map[U](self, func: Callable[[T], U]) -> Seq[U]:
        return Seq([func(v) for v in self._value])

    def join(self, separator: str = "") -> str:
        return separator.join(self._value)

    def keys[U](self) -> Seq[U]:
        return Seq([x for x, y in self._value])

    def values[U](self) -> Seq[U]:
        return Seq([y for x, y in self._value])

    def print(self, printer, separator: str = "") -> str:
        mapped = self.map(lambda x: x.print(printer))
        joined = mapped.join(separator)
        return joined
