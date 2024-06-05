from abc import ABCMeta


class ConventionPartInitAstMeta(ABCMeta):
    def __repr__(self):
        return "Partially-Initialized: "


class ConventionPartInitAst(metaclass=ConventionPartInitAstMeta):
    """
    The ConventionPartInitAst is not creatable from parsing code, but is generated in semantic analysis code when
    inferring the type of a value, as the type of value involves its convention. It means that the value is partially
    initialized, and can only be used when fully initializing it.
    """

    def __eq__(self, other):
        # Check both ASTs are the same type.
        return isinstance(other, ConventionPartInitAst)


__all__ = ["ConventionPartInitAst"]
