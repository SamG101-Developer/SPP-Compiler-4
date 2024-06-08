from dataclasses import dataclass, field
from typing import Optional

from SPPCompiler.SemanticAnalysis.ASTMixins.SemanticAnalyser import SemanticAnalyser
from SPPCompiler.SemanticAnalysis.ASTMixins.TypeInfer import TypeInfer, InferredType
from SPPCompiler.SemanticAnalysis.ASTs.Meta.Ast import Ast
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstPrinter import AstPrinter
from SPPCompiler.SemanticAnalysis.Utils.CommonTypes import CommonTypes


@dataclass
class NumberLiteralBaseNAst(Ast, SemanticAnalyser, TypeInfer):
    """
    The NumberLiteralBaseNAst node is the base class of explicit base numbers, containing common information such as the
    explicit number type, etc.

    Attributes:
        value: The integer part of the number.
        raw_type: The number's raw type (identifier, like i64)
        type: The number's true type (type, like I64)
    """

    value: "TokenAst"
    raw_type: Optional["IdentifierAst"]
    type: Optional["TypeAst"] = field(default=None, init=False)

    def __post_init__(self) -> None:
        from SPPCompiler.SemanticAnalysis.ASTs import GenericIdentifierAst, IdentifierAst, TypeAst

        if self.raw_type:
            corrected_raw_type = GenericIdentifierAst(self.raw_type.pos, self.raw_type.value.title(), None)
            std_namespace = IdentifierAst(self.raw_type.pos, "std")
            self.type = TypeAst(self.raw_type.pos, [std_namespace, corrected_raw_type])

    def print(self, printer: AstPrinter) -> str:
        # Print the NumberLiteralBaseNAst.
        s = ""
        s += f"{self.value.print(printer)}"
        if self.raw_type:
            s += f"_{self.raw_type.print(printer)}"
        return s

    def do_semantic_analysis(self, scope_handler, **kwargs) -> None:
        from SPPCompiler.LexicalAnalysis.Tokens import TokenType
        assert isinstance(self.value.token.token_metadata, int)
        assert self.value.token.token_type in [TokenType.LxDecFloat, TokenType.LxDecDigits]

    def infer_type(self, scope_handler, **kwargs) -> InferredType:
        from SPPCompiler.LexicalAnalysis.Tokens import TokenType
        from SPPCompiler.SemanticAnalysis.ASTs import ConventionMovAst

        # The string literal's type is either `std.BigNum` or `std.BigDec` if no explicit type is given, otherwise it is
        # the explicit type.
        if self.type:
            return InferredType(convention=ConventionMovAst, type=self.type)
        if self.value.token.token_type == TokenType.LxDecFloat:
            return InferredType(convention=ConventionMovAst, type=CommonTypes.big_dec(self.pos))
        else:
            return InferredType(convention=ConventionMovAst, type=CommonTypes.big_num(self.pos))

    def __eq__(self, other):
        # Check both ASTs are the same type and have the same type.
        return isinstance(other, NumberLiteralBaseNAst) and self.type == other.type


__all__ = ["NumberLiteralBaseNAst"]
