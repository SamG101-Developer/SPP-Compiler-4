from dataclasses import dataclass
from typing import List

from SPPCompiler.SemanticAnalysis.ASTMixins.SemanticAnalyser import SemanticAnalyser
from SPPCompiler.SemanticAnalysis.ASTMixins.TypeInfer import TypeInfer, InferredType
from SPPCompiler.SemanticAnalysis.ASTs.Meta.Ast import Ast
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstPrinter import *
from SPPCompiler.SemanticAnalysis.Utils.Scopes import ScopeHandler
from SPPCompiler.Utils.Sequence import Seq


@dataclass
class PatternVariantTupleAst(Ast, SemanticAnalyser, TypeInfer):
    """
    The PatternVariantTupleAst node represents a tuple destructuring pattern on a conditional branch. This is used to
    match a tuple of values to a tuple of patterns. For example, "== (1, (2, 3), a)" would match a tuple of 3 elements,
    where the 2nd element is a tuple of 2 elements, and the 3rd element is anything, bound to "a".

    Attributes:
        paren_l_token: The left parenthesis token.
        items: The items being destructured.
        paren_r_token: The right parenthesis token.
    """

    paren_l_token: "TokenAst"
    items: List["PatternVariantNestedAst"]
    paren_r_token: "TokenAst"

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        s = ""
        s += f"{self.paren_l_token.print(printer)}"
        s += f"{Seq(self.items).print(printer, ", ")}" if self.items else ""
        s += f"{self.paren_r_token.print(printer)}"
        return s

    def convert_to_variable(self) -> "LetStatementInitializedAst":
        from SPPCompiler.SemanticAnalysis.ASTs import (
            LocalVariableTupleAst, PatternVariantLiteralAst, PatternVariantSkipArgumentAst,
            PatternVariantVariableAssignmentAst, PatternVariantVariableAst)

        # Convert inner patterns to variables.
        converted_items = Seq(self.items).filter_to_type(
            PatternVariantVariableAssignmentAst,
            PatternVariantSkipArgumentAst,
            PatternVariantVariableAst,
            PatternVariantLiteralAst,
        ).map(lambda i: i.convert_to_variable())

        # Return the new LocalVariableTupleAst.
        bindings = LocalVariableTupleAst(
            pos=self.pos,
            paren_l_token=self.paren_l_token,
            items=converted_items.value,
            paren_r_token=self.paren_r_token)

        return bindings

    def do_semantic_analysis(self, scope_handler: ScopeHandler, **kwargs) -> None:
        from SPPCompiler.LexicalAnalysis.Tokens import TokenType
        from SPPCompiler.SemanticAnalysis.ASTs.LetStatementInitializedAst import LetStatementInitializedAst
        from SPPCompiler.SemanticAnalysis.ASTs.TokenAst import TokenAst

        bindings = self.convert_to_variable()
        declaration = LetStatementInitializedAst(
            pos=self.pos,
            let_keyword=TokenAst.dummy(TokenType.KwLet),
            assign_to=bindings,
            assign_token=TokenAst.dummy(TokenType.TkAssign),
            value=kwargs["condition"])

        declaration.do_semantic_analysis(scope_handler, **kwargs)

    def infer_type(self, scope_handler: ScopeHandler, **kwargs) -> InferredType:
        from SPPCompiler.SemanticAnalysis.ASTs.ConventionMovAst import ConventionMovAst
        return InferredType(convention=ConventionMovAst, type=kwargs["condition"].infer_type(scope_handler, **kwargs).type)
