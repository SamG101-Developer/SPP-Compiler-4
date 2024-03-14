from dataclasses import dataclass
from typing import List, Tuple, Type

from src.SemanticAnalysis.Analysis.SemanticAnalysis import SemanticAnalysis
from src.SemanticAnalysis.Analysis.SemanticError import SemanticError
from src.SemanticAnalysis.Symbols.Scopes import ScopeHandler
from src.SemanticAnalysis.Types.TypeInfer import TypeInfer

from src.SemanticAnalysis.ASTs.Meta.Ast import Ast
from src.SemanticAnalysis.ASTs.Meta.AstPrinter import *

from src.SemanticAnalysis.ASTs.ConventionMovAst import ConventionMovAst
from src.SemanticAnalysis.ASTs.PatternVariantSkipArgumentAst import PatternVariantSkipArgumentAst

from src.Utils.Sequence import Seq


@dataclass
class PatternVariantTupleAst(Ast, SemanticAnalysis, TypeInfer):
    """
    The PatternVariantTupleAst node represents a tuple destructuring pattern on a conditional branch. This is used to
    match a tuple of values to a tuple of patterns. For example, "== (1, (2, 3), a)" would match a tuple of 3 elements,
    where the 2nd element is a tuple of 2 elements, and the 3rd element is anything, bound to "a".
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

    def do_semantic_analysis(self, scope_handler: ScopeHandler, if_condition: "ExpressionAst" = None, **kwargs) -> None:
        Seq(self.items).for_each(lambda x: x.do_semantic_analysis(scope_handler, if_condition=if_condition, **kwargs))

        # todo: offload this into the LocalVariableTupleAst
        # todo: offload from the let statement into the LocalVariableTupleAst
        # todo: currently has 2 implementations
        # todo: see "PatternVariantDestructureAst"

        has_skipped_args = None
        for argument in self.items:
            if isinstance(argument, PatternVariantSkipArgumentAst):
                if has_skipped_args:
                    exception = SemanticError(f"Multiple '..' given to pattern:")
                    exception.add_traceback(has_skipped_args.pos, f"1st variadic argument given here.")
                    exception.add_traceback_minimal(argument.variadic_token.pos, f"2nd variadic argument given here.")
                    raise exception
                has_skipped_args = argument
                continue

        lhs_tuple_type_elements = if_condition.infer_type(scope_handler, **kwargs)[1].parts[-1].generic_arguments.arguments
        rhs_tuple_type_elements = self.items
        if len(lhs_tuple_type_elements) != len(rhs_tuple_type_elements) and not has_skipped_args:
            exception = SemanticError(f"Invalid tuple assignment:")
            exception.add_traceback(self.pos, f"Assignment target tuple contains {len(rhs_tuple_type_elements)} elements.")
            exception.add_traceback(if_condition.pos, f"Assignment value tuple contains {len(lhs_tuple_type_elements)} elements.")
            raise exception

    def infer_type(self, scope_handler: ScopeHandler, if_condition: "ExpressionAst" = None, **kwargs) -> Tuple[Type["ConventionAst"], "TypeAst"]:
        return ConventionMovAst, if_condition.infer_type(scope_handler, **kwargs)[1]
