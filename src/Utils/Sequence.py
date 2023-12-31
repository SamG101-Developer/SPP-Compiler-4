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

    def print(self, separator: str = "") -> str:
        return self.map(lambda x: x.print()).join(separator)
