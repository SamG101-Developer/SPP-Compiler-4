from __future__ import annotations

from typing import List, Callable, Iterator, Optional


class Seq[T]:
    _value: List[T]

    def __init__(self, value: List[T]) -> None:
        self._value = value

    def append(self, item: T) -> None:
        self._value.append(item)

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

    def non_unique_items(self) -> Seq[List[T]]:
        items = []
        for x in self.unique_items():
            if self._value.count(x) > 1:
                items.append([y for y in self._value if y == x])
        return Seq(items)

    def non_unique_items_flat(self) -> Seq[T]:
        return self.non_unique_items().map(lambda x: x[0])

    def contains_duplicates(self) -> bool:
        return self.non_unique_items().not_empty()

    def contains(self, item: T) -> bool:
        return item in self._value

    def contains_any(self, items: Seq[T]) -> Seq[T]:
        return Seq([x for x in items if x in self._value])

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

    def skip(self, n: int) -> Seq[T]:
        return Seq(self._value[n:])

    def take(self, n: int) -> Seq[T]:
        return Seq(self._value[:n])

    def zip[U](self, other: Seq[U]) -> Seq[tuple[T, U]]:
        return Seq(list(zip(self._value, other._value)))

    def find(self, func: Callable[[T], bool]) -> Optional[T]:
        for x in self._value:
            if func(x):
                return x
        return None

    def all(self, func: Callable[[T], bool]) -> bool:
        return all(func(x) for x in self._value)

    def any(self, func: Callable[[T], bool]) -> bool:
        return any(func(x) for x in self._value)

    def enumerate(self) -> Seq[tuple[int, T]]:
        return Seq(list(enumerate(self._value)))

    def remove(self, item: T) -> Seq[T]:
        self._value.remove(item)
        return self

    def replace(self, item: T, replacement: T) -> Seq[T]:
        self._value = [replacement if x == item else x for x in self._value]
        return self

    def pop(self, index: int) -> T:
        return self._value.pop(index)

    def first(self, default: Optional[T] = None):
        return self._value[0] if self.not_empty() else default

    def last(self, default: Optional[T] = None):
        return self._value[-1] if self.not_empty() else default

    def __iter__(self) -> Iterator[T]:
        return iter(self._value)

    def __getitem__(self, key: int) -> T:
        if isinstance(key, int) and key > len(self._value) - 1:
            raise IndexError(f"Index {key} is out of bounds for sequence of length {len(self._value)}")
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
