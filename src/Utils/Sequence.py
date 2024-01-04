from __future__ import annotations

from typing import List, Callable, Iterator


class Seq[T]:
    _value: List[T]

    def __init__(self, value: List[T]) -> None:
        self._value = value

    def for_each[U](self, func: Callable[[T], U]) -> None:
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

    def non_unique_items_flat(self) -> Seq[T]:
        return self.non_unique_items().map(lambda x: x[0])

    def contains_duplicates(self) -> bool:
        return self.non_unique_items().not_empty()

    def sort(self, key: Callable[[T], any] = None, reverse: bool = False) -> Seq[T]:
        return Seq(sorted(self._value, key=key, reverse=reverse))

    def is_sorted(self, key: Callable[[T], any] = None, reverse: bool = False) -> bool:
        return self.sort(key=key, reverse=reverse) == self

    def ordered_difference(self, other: Seq[T]) -> Seq[T]:
        out = []
        for x, y in zip(self, other):
            if x != y:
                out.append(y)
        return Seq(out)

    def zip[U](self, other: Seq[U]) -> Seq[tuple[T, U]]:
        return Seq(list(zip(self._value, other._value)))

    def all(self, func: Callable[[T], bool]) -> bool:
        return all(func(x) for x in self._value)

    def any(self, func: Callable[[T], bool]) -> bool:
        return any(func(x) for x in self._value)

    def contains(self, item: T) -> bool:
        return item in self._value

    def __iter__(self) -> Iterator[T]:
        return iter(self._value)

    def __getitem__(self, key: int) -> T:
        return self._value[key]

    def __setitem__(self, key: int, value: T) -> None:
        self._value[key] = value

    def __sub__(self, other):
        return Seq([x for x in self._value if x not in other])

    def __eq__(self, other):
        return self._value == other._value

    def __bool__(self):
        return self.not_empty()

    @property
    def value(self) -> List[T]:
        return self._value

    @property
    def length(self) -> int:
        return len(self._value)
