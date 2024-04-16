from dataclasses import dataclass
from typing import List

from src.SemanticAnalysis.ASTMixins.SemanticAnalyser import SemanticAnalyser
from src.SemanticAnalysis.Utils.SemanticError import SemanticError, SemanticErrorType
from src.SemanticAnalysis.Utils.Scopes import ScopeHandler

from src.SemanticAnalysis.ASTs.Meta.Ast import Ast
from src.SemanticAnalysis.ASTs.Meta.AstPrinter import *

from src.SemanticAnalysis.ASTs.LocalVariableSingleAst import LocalVariableSingleAst
from src.SemanticAnalysis.ASTs.LocalVariableSkipArgumentAst import LocalVariableSkipArgumentAst
from src.SemanticAnalysis.ASTs.TokenAst import TokenAst

from src.Utils.Sequence import Seq


@dataclass
class LocalVariableTupleAst(Ast, SemanticAnalyser):
    """
    The LocalVariableTupleAst node represents a tuple of local variables. This is an advanced form of a local variable,
    and is seen mostly in the "let" statement. For example, in the statement "let (mut x, y) = (5, 6)", "(mut x, y)" is
    the tuple of local variables. Both "mut x" and "y" are separate single local variables.

    Attributes:
        - paren_l_token: The left parenthesis token.
        - items: The local variables in the tuple.
        - paren_r_token: The right parenthesis token.
    """

    paren_l_token: TokenAst
    items: List[LocalVariableSingleAst]
    paren_r_token: TokenAst

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        # Print the LocalVariableTupleAst.
        s = ""
        s += f"{self.paren_l_token.print(printer)}"
        s += f"{Seq(self.items).print(printer, ", ")}"
        s += f"{self.paren_r_token.print(printer)}"
        return s

    def do_semantic_analysis(self, scope_handler: ScopeHandler, other_tuple: "ExpressionAst" = None, **kwargs) -> None:
        has_skipped_args = None
        for argument in self.items:
            if isinstance(argument, LocalVariableSkipArgumentAst):
                if has_skipped_args:
                    exception = SemanticError()
                    exception.add_info(
                        pos=has_skipped_args.pos,
                        tag_message=f"1st argument skip here")
                    exception.add_error(
                        pos=argument.variadic_token.pos,
                        error_type=SemanticErrorType.ORDER_ERROR,
                        message=f"Cannot have multiple skip arguments '..' in a destructure pattern",
                        tag_message=f"2nd argument skip here",
                        tip="Remove the additional skip argument")
                    raise exception
                has_skipped_args = argument
                continue

        rhs_tuple_type_elements = other_tuple.infer_type(scope_handler, **kwargs)[1].parts[-1].generic_arguments.arguments
        lhs_tuple_type_elements = self.items
        if len(lhs_tuple_type_elements) != len(rhs_tuple_type_elements) and not has_skipped_args:
            exception = SemanticError()
            exception.add_info(
                pos=self.pos,
                tag_message=f"Assignment target tuple contains {len(lhs_tuple_type_elements)} elements")
            exception.add_error(
                pos=other_tuple.pos,
                error_type=SemanticErrorType.ORDER_ERROR,
                message=f"Cannot destructure tuples into a different number of elements",
                tag_message=f"Assignment value tuple contains {len(rhs_tuple_type_elements)} elements",
                tip=f"Ensure the assignment target tuple contains {len(rhs_tuple_type_elements)} elements.")
            raise exception


__all__ = ["LocalVariableTupleAst"]
