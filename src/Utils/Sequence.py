from __future__ import annotations

from typing import Iterable, Callable, Sized


class Seq[T]:
    _value: Iterable[T]

    def __init__(self, value: Iterable[T]) -> None:
        self._value = value

    def for_each(self, func: Callable[[T], None]) -> None:
        for v in self._value: func(v)

    def map[U](self, func: Callable[[T], U]) -> Seq[U]:
        return Seq([func(v) for v in self._value])

    def filter(self, func: Callable[[T], bool]) -> Seq[T]:
        return Seq([v for v in self._value if func(v)])

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

    def empty(self) -> bool:
        return len(self._value) == 0

    def not_empty(self) -> bool:
        return len(self._value) != 0

    @property
    def value(self) -> int:
        return self._value

    @property
    def length(self) -> int:
        return len(self._value)
