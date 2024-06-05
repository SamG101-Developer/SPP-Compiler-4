class ConventionNonInitAstMeta(type):
    def __repr__(self):
        return "Uninitialized: "


class ConventionNonInitAst(metaclass=ConventionNonInitAstMeta):
    """
    The ConventionNonInitAst is not creatable from parsing code, but is generated in semantic analysis code when
    inferring the type of a value, as the type of value involves its convention. It means that the value is not
    initialized, and can only be used when initializing it.
    """

    def __eq__(self, other):
        # Check both ASTs are the same type.
        return isinstance(other, ConventionNonInitAst)


__all__ = ["ConventionNonInitAst"]
