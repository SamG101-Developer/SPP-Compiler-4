from ordered_set import OrderedSet


class ParserError(Exception):
    pos: int
    expected_tokens: OrderedSet[str]

    def __init__(self, *args) -> None:
        super().__init__(*args)
        self.pos = -1
        self.expected_tokens = OrderedSet()
