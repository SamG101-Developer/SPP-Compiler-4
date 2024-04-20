from dataclasses import dataclass
from typing import Optional

from src.SemanticAnalysis.ASTMixins.SemanticAnalyser import SemanticAnalyser
from src.SemanticAnalysis.Utils.SemanticError import SemanticError, SemanticErrorType
from src.SemanticAnalysis.Utils.CommonTypes import CommonTypes

from src.SemanticAnalysis.ASTs.Meta.Ast import Ast
from src.SemanticAnalysis.ASTs.Meta.AstPrinter import *
from src.SemanticAnalysis.ASTs.Meta.AstUtils import AstUtils

from src.SemanticAnalysis.ASTs.TokenAst import TokenAst
from src.SemanticAnalysis.ASTs.ExpressionAst import ExpressionAst
from src.SemanticAnalysis.ASTs.ConventionMovAst import ConventionMovAst


@dataclass
class ReturnStatementAst(Ast, SemanticAnalyser):
    """
    The ReturnStatementAst node represents a return statement. This is a statement that returns a value from a function.
    It has a return keyword and an optional expression to return.

    Attributes:
        - return_keyword: The "ret" keyword.
        - expression: The expression to return.
    """

    return_keyword: TokenAst
    expression: Optional[ExpressionAst]

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        # Print the ReturnStatementAst.
        s = ""
        s += f"{self.return_keyword.print(printer)}"
        s += f"{self.expression.print(printer)}" if self.expression else ""
        return s

    def do_semantic_analysis(self, scope_handler, **kwargs) -> None:
        from src.SemanticAnalysis.ASTs.TypeAst import TypeAst
        from src.SemanticAnalysis.ASTs.GenericIdentifierAst import GenericIdentifierAst

        # If there is a value being returned, analyse it and ensure its memory status is "owned".
        if self.expression:
            self.expression.do_semantic_analysis(scope_handler, **kwargs)
            AstUtils.ensure_memory_integrity_of_expression(self.expression, scope_handler, **kwargs)

        # Check the return type matches the function's return type.
        target_return_type = kwargs["target-return-type"]
        # todo
        if kwargs["fn-proto"]._is_coro:
            target_return_type = target_return_type.parts[-1].generic_arguments["Return"]
            if not target_return_type:  # get default:
                function_return_type = scope_handler.current_scope.get_symbol(kwargs["target-return-type"])
                function_return_type_return_type_generic_parameter = function_return_type.associated_scope.get_symbol(TypeAst(-1, [GenericIdentifierAst(-1, "Return", None)])).type

        return_type = self.expression.infer_type(scope_handler, **kwargs) if self.expression else (ConventionMovAst, CommonTypes.void())
        if return_type[0] != ConventionMovAst or not target_return_type.symbolic_eq(return_type[1], scope_handler.current_scope):
            exception = SemanticError()
            exception.add_info(
                pos=target_return_type.pos,
                tag_message=f"Function has return type '{target_return_type}'")
            exception.add_error(
                pos=(self.expression or self.return_keyword).pos,
                error_type=SemanticErrorType.TYPE_ERROR,
                message="Returning variable of incorrect type",
                tag_message=f"Type inferred as '{return_type[0]}{return_type[1]}'",
                tip=f"Return a {target_return_type} object instead")
            raise exception


__all__ = ["ReturnStatementAst"]
