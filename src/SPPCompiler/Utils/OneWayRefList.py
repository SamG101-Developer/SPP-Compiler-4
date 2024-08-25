from __future__ import annotations
from typing import List, Iterable, Optional


class OneWayRefList[T]:
    """
    The OneWayRefList is a special list where appending to the list will append to any other linked lists, but appending
    to the linked lists will not append to this list. This allows one "master" list to control other lists, but allows
    the other lists to be individually customised. This is primarily used for sup-scope control: the base type has all
    the sup-scopes for the entire class, with generically superimposed types having special superimpositions too (for
    specialisation).
    """

    _master_list: Optional[OneWayRefList[T]]
    _this_list: List[T]

    def __init__(self, this_list: List[T], *, master_list: Optional[OneWayRefList[T]] = None) -> None:
        self._this_list = this_list
        self._master_list = master_list

    def append(self, item: T) -> None:
        self._this_list.append(item)

    @property
    def _complete_list(self) -> List[T]:
        match self._master_list:
            case None: return self._this_list
            case master_list: return master_list._complete_list + self._this_list

    def __iter__(self) -> Iterable[T]:
        return iter(self._complete_list)

    def __getitem__(self, key: int) -> T:
        return self._complete_list[key]

    def __setitem__(self, key: int, value: T) -> None:
        print("Setting item", key, "in list of length", len(self))
        self._this_list[key] = value

    def __len__(self) -> int:
        return len(self._complete_list)

    def __contains__(self, item: T) -> bool:
        return item in self._complete_list

    def __repr__(self) -> str:
        return repr(self._complete_list)


__all__ = ["OneWayRefList"]
