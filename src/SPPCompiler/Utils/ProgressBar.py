import math
import colorama


class ProgressBar:
    _title: str
    _max_value: int
    _current_value: int
    _current_label: str
    _characters: list[str]

    def __init__(self, title: str, max_value: int) -> None:
        self._title = title
        self._max_value = max_value
        self._current_value = 0
        self._current_label = ""
        self._character = "▔"
        self._init()

    def _init(self) -> None:
        color = colorama.Fore.LIGHTWHITE_EX
        reset = colorama.Fore.RESET
        print(f"{color}{self._title}:{reset}\n")

    def next(self, label: str) -> None:
        self._current_value += 1
        self._current_label = ""
        self._print()

    def _print(self) -> None:
        percentage = self._current_value / self._max_value * 100
        bar = self._character * math.floor(percentage)

        color = colorama.Fore.LIGHTRED_EX if percentage < 25 else colorama.Fore.LIGHTYELLOW_EX if percentage < 75 else colorama.Fore.LIGHTGREEN_EX
        reset = colorama.Fore.RESET

        print(f"\r\t{color}{bar}{reset}", end="")

        if self._current_value == self._max_value:
            print("\n")
