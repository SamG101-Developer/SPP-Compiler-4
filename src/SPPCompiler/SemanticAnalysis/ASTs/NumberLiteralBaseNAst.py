from dataclasses import dataclass, field
from typing import Optional, Tuple, Type

from SPPCompiler.SemanticAnalysis.ASTMixins.SemanticAnalyser import SemanticAnalyser
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstPrinter import AstPrinter
from SPPCompiler.LexicalAnalysis.Tokens import TokenType

from SPPCompiler.SemanticAnalysis.Utils.CommonTypes import CommonTypes
from SPPCompiler.SemanticAnalysis.ASTMixins.TypeInfer import TypeInfer

from SPPCompiler.SemanticAnalysis.ASTs.Meta.Ast import Ast

from SPPCompiler.SemanticAnalysis.ASTs.ConventionMovAst import ConventionMovAst
from SPPCompiler.SemanticAnalysis.ASTs.GenericIdentifierAst import GenericIdentifierAst
from SPPCompiler.SemanticAnalysis.ASTs.IdentifierAst import IdentifierAst
from SPPCompiler.SemanticAnalysis.ASTs.TypeSingleAst import TypeSingleAst


@dataclass
class NumberLiteralBaseNAst(Ast, SemanticAnalyser, TypeInfer):
    """
    The NumberLiteralBaseNAst node is the base class of explicit base numbers, containing common information such as the
    explicit number type, etc.

    Attributes:
        - integer: The integer part of the number.
        - raw_type: The number's raw type (identifier, like i64)
        - type: The number's true type (type, like I64)
    """

    value: "TokenAst"
    raw_type: Optional["IdentifierAst"]
    type: Optional["TypeAst"] = field(default=None, init=False)

    def __post_init__(self) -> None:
        if self.raw_type:
            corrected_raw_type = GenericIdentifierAst(self.raw_type.pos, self.raw_type.value.title(), None)
            std_namespace = IdentifierAst(self.raw_type.pos, "std")
            self.type = TypeSingleAst(self.raw_type.pos, [std_namespace, corrected_raw_type])

    def print(self, printer: AstPrinter) -> str:
        # Print the NumberLiteralBaseNAst.
        s = ""
        s += f"{self.value.print(printer)}"
        if self.raw_type:
            s += f"_{self.raw_type.print(printer)}"
        return s

    def do_semantic_analysis(self, scope_handler, **kwargs) -> None:
        ...

    def infer_type(self, scope_handler, **kwargs) -> Tuple[Type["ConventionAst"], "TypeAst"]:
        # The string literal's type is either `std.BigNum` or `std.BigDec` if no explicit type is given, otherwise it is
        # the explicit type.

        if self.type:
            return ConventionMovAst, self.type
        if self.value.token.token_type == TokenType.LxDecFloat:
            return ConventionMovAst, CommonTypes.big_dec(self.pos)
        return ConventionMovAst, CommonTypes.big_num(self.pos)

    def __eq__(self, other):
        # Check both ASTs are the same type and have the same type.
        return isinstance(other, NumberLiteralBaseNAst) and self.type == other.type


__all__ = ["NumberLiteralBaseNAst"]
