from __future__ import annotations
from typing import Dict

from SPPCompiler.SemanticAnalysis.ASTs import IdentifierAst, TypeAst


def infer_generics_types(
        generic_parameters: list[TypeAst],
        explicit_generic_arguments: Dict[TypeAst, TypeAst],
        infer_from: Dict[IdentifierAst, TypeAst],
        map_to: Dict[IdentifierAst, TypeAst]) -> Dict[TypeAst, TypeAst]:

    """
    For the class:

    cls Point[T, U, V] {
        x: T
        y: U
        z: V
    }

    let p = Point(x=1, y="hello", z=False)

    the arguments:
        generic_parameters: [T, U, V]
        explicit_generic_arguments: {}
        infer_from: {x: 1, y: "hello", z: False}
        map_to: {x: T, y: U, z: V}
    """

    # Infer all possible generic arguments.
    inferred_generic_arguments = {value: map_to[identifier] for identifier, value in infer_from.items() if identifier in map_to.keys()}

    # Check no inferred generic arguments are already explicitly defined.
    for inferred_generic_argument in inferred_generic_arguments.keys():
        if inferred_generic_argument in explicit_generic_arguments:
            raise SemanticErrors.TYPE_REDECLARATION(inferred_generic_argument, explicit_generic_arguments[inferred_generic_argument], inferred_generic_arguments[inferred_generic_argument])

    # Check all generic parameters have been inferred or explicitly defined.
    for generic_parameter in generic_parameters:
        if generic_parameter not in explicit_generic_arguments and generic_parameter not in inferred_generic_arguments:
            raise SemanticErrors.MISSING_GENERIC_ARGUMENT(generic_parameter)

    # Return a union of the inferred and explicit generic arguments.
    return inferred_generic_arguments | explicit_generic_arguments


__all__ = ["AstUtils"]
