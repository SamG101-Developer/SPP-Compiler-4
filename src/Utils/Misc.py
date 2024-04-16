SUFFIXES = {1: "st", 2: "nd", 3: "rd"}


def ordinal(number: int) -> str:
    suffix = "th" if 10 <= number % 100 <= 20 else SUFFIXES.get(number % 10, "th")
    return f"{number}{suffix}"
