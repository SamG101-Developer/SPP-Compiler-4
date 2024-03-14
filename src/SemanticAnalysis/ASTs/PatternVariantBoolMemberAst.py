from dataclasses import dataclass

from src.SemanticAnalysis.ASTs.Meta.Ast import Ast
from src.SemanticAnalysis.ASTs.Meta.AstPrinter import *


@dataclass
class PatternVariantBoolMemberAst(Ast):
    """
    The PatternVariantBoolMemberAst node represents a boolean member pattern on a conditional branch. This is used to
    match the result of a boolean method or attribute against "true". For example,
    "case point then .is_valid() {...} .other_attr {...}".
    """

    def __post_init__(self):
        raise NotImplementedError("PatternVariantBoolMemberAst is not implemented yet.")

    def print(self, printer: AstPrinter) -> str:
        # Print the PatternVariantBoolMemberAst.
        return ""
