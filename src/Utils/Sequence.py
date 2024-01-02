from __future__ import annotations

from typing import List, Callable, Iterator


class Seq[T]:
    _value: List[T]

    def __init__(self, value: List[T]) -> None:
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

    def unique_items(self) -> Seq[T]:
        return Seq(list(set(self._value)))  # TODO : use an ordered set to maintain the order of the items every time

    def non_unique_items(self) -> Seq[Seq[T]]:
        items = []
        for x in self.unique_items():
            if self._value.count(x) > 1:
                items.append([y for y in self._value if y == x])
        return Seq(items)

    def contains_duplicates(self) -> bool:
        return self.non_unique_items().not_empty()

    def __iter__(self) -> Iterator[T]:
        return iter(self._value)

    def __getitem__(self, key: int) -> T:
        return self._value[key]

    def __setitem__(self, key: int, value: T) -> None:
        self._value[key] = value

    @property
    def value(self) -> List[T]:
        return self._value

    @property
    def length(self) -> int:
        return len(self._value)
