from dataclasses import dataclass
from typing import List
import copy

from SPPCompiler.SemanticAnalysis.ASTs.Meta.Ast import Ast
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstMixins import SemanticAnalyser
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstUtils import TypeInfer, InferredType
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstPrinter import *
from SPPCompiler.SemanticAnalysis.Utils.CommonTypes import CommonTypes
from SPPCompiler.SemanticAnalysis.Utils.Scopes import ScopeHandler
from SPPCompiler.SemanticAnalysis.Utils.SemanticError import SemanticErrors
from SPPCompiler.Utils.Sequence import Seq


@dataclass
class PatternVariantObjectDestructureAst(Ast, SemanticAnalyser, TypeInfer):
    """
    The PatternVariantDestructureAst node represents a destructuring pattern on a conditional branch. This is used to
    match a value to a pattern. For example, "case point then == Point(x, y)" would destructure the "point" into "x" and
    "y".

    Attributes:
        class_type: The class type being destructured.
        bracket_l_token: The left bracket token.
        items: The items being destructured.
        bracket_r_token: The right bracket token.
    """

    class_type: "TypeAst"
    bracket_l_token: "TokenAst"
    items: List["PatternVariantNestedForObjectDestructureAst"]
    bracket_r_token: "TokenAst"

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        # Print the PatternVariantDestructureAst.
        s = ""
        s += f"{self.class_type.print(printer)}"
        s += f"{self.bracket_l_token.print(printer)}"
        s += f"{Seq(self.items).print(printer, ", ")}" if self.items else ""
        s += f"{self.bracket_r_token.print(printer)}"
        return s

    def convert_to_variable(self) -> "LocalVariableObjectDestructureAst":
        from SPPCompiler.SemanticAnalysis.ASTs import (
            LocalVariableObjectDestructureAst, PatternVariantNestedForObjectDestructureAst)

        # Convert inner patterns to variables.
        converted_items = Seq(self.items).filter_to_type(
            *PatternVariantNestedForObjectDestructureAst.__value__.__args__
        ).map(lambda i: i.convert_to_variable())

        # Return the new LocalVariableDestructureAst.
        bindings = LocalVariableObjectDestructureAst(
            pos=self.pos,
            class_type=self.class_type,
            bracket_l_token=self.bracket_l_token,
            items=converted_items.list(),
            bracket_r_token=self.bracket_r_token)

        return bindings

    def do_semantic_analysis(self, scope_handler: ScopeHandler, **kwargs) -> None:
        from SPPCompiler.LexicalAnalysis.Tokens import TokenType
        from SPPCompiler.SemanticAnalysis.ASTs import LetStatementInitializedAst, TokenAst

        # Get the condition's symbol (if it exists), and flow-type it.
        condition_symbol = scope_handler.current_scope.get_symbol(kwargs["condition"])
        flow_symbol = None
        if condition_symbol and condition_symbol.type.without_generics().symbolic_eq(CommonTypes.var([]), scope_handler.current_scope):

            # Ensure the composite type belongs to the variant type.
            variant_type = kwargs["condition"].infer_type(scope_handler, **kwargs).type
            composite_types = variant_type.types[-1].generic_arguments.arguments[-1].type.types[-1].generic_arguments.arguments
            if not any(self.class_type.symbolic_eq(composite_type.type, scope_handler.current_scope) for composite_type in composite_types):
                raise SemanticErrors.TYPE_DESTRUCTURING_INVALID_TYPE(self.class_type, variant_type)

            # Create the flow symbol.
            flow_symbol = copy.deepcopy(condition_symbol)
            flow_symbol.type = self.class_type
            scope_handler.current_scope.add_symbol(flow_symbol)

        # Convert the destructuring pattern into a variable, and analyse it.
        bindings = self.convert_to_variable()
        declaration = LetStatementInitializedAst(
            pos=self.pos,
            let_keyword=TokenAst.dummy(TokenType.KwLet),
            assign_to=bindings,
            assign_token=TokenAst.dummy(TokenType.TkAssign),
            value=kwargs["condition"])
        declaration.do_semantic_analysis(scope_handler, **kwargs)

        # Put the condition variable back (re-initialise memory)
        symbol = scope_handler.current_scope.get_symbol(kwargs["condition"])
        if symbol is not None:
            symbol.memory_info.ast_consumed = None
            symbol.memory_info.ast_partial_moves = []

    def infer_type(self, scope_handler: ScopeHandler, **kwargs) -> InferredType:
        # The destructuring pattern's type is the class type being destructured into.
        from SPPCompiler.SemanticAnalysis.ASTs.ConventionMovAst import ConventionMovAst
        return InferredType(convention=ConventionMovAst, type=self.class_type)


__all__ = ["PatternVariantObjectDestructureAst"]
