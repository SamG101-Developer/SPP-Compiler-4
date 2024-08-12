import math
import os

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
        self._character = "—"

    def next(self, label: str) -> None:
        self._current_value += 1
        self._current_label = label.split(os.path.sep)[-1]
        self._print()

    def _print(self) -> None:
        # Calculate the percentage done, and the number of characters to print.
        percentage = self._current_value / self._max_value * 100
        bar = self._character * math.floor(percentage)

        # Create the title label, describing the progress bar.
        color = colorama.Fore.LIGHTWHITE_EX
        reset = colorama.Fore.RESET
        title = f"{color}{self._title}{reset} "

        # Create the subtext label, describing the current step, and pad it to the end of the bar.
        pad_to_label = " " * (100 - len(bar))
        if self._current_value == self._max_value:
            label = "[Done]"
        else:
            label = f"{pad_to_label}[{self._current_label}]"
        label = f"{color}{label}{reset}"

        # Create the colour of the progress bar depending on the percentage complete.
        color = colorama.Fore.LIGHTRED_EX if percentage < 25 else colorama.Fore.LIGHTYELLOW_EX if percentage < 75 else colorama.Fore.LIGHTGREEN_EX

        # Print the progress bar
        print(f"\r{title}{color}{bar}{reset} {label}", end="")

        # Print a newline if the progress bar is complete.
        if self._current_value == self._max_value:
            print("")
