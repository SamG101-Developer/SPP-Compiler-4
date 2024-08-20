import copy
import json
from dataclasses import dataclass

from SPPCompiler.LexicalAnalysis.Tokens import TokenType
from SPPCompiler.SemanticAnalysis.ASTs import LetStatementUninitializedAst, LocalVariableSingleIdentifierAst
from SPPCompiler.SemanticAnalysis.ASTs.Meta.Ast import Ast
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstMixins import SemanticAnalyser
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstPrinter import *
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstUtils import InferredType
from SPPCompiler.SemanticAnalysis.Utils.CommonTypes import CommonTypes
from SPPCompiler.SemanticAnalysis.Utils.Scopes import ScopeHandler
from SPPCompiler.SemanticAnalysis.Utils.SemanticError import SemanticErrors
from SPPCompiler.SemanticAnalysis.Utils.Symbols import MemoryStatus
from SPPCompiler.Utils.Sequence import Seq


@dataclass
class LoopExpressionConditionIterableAst(Ast, SemanticAnalyser):
    """
    The LoopExpressionConditionIterable node is used to represent a condition in a loop expression that represents a
    range. A range is any iterable object, ie an object superimposing IterMov, IterRef or IterMut.
    """

    variable: "LocalVariableAst"
    in_keyword: "TokenAst"
    iterable: "ExpressionAst"

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        # Print the LoopExpressionConditionIterable.
        s = ""
        s += f"{self.variable.print(printer)} "
        s += f"{self.in_keyword.print(printer)}"
        s += f"{self.iterable.print(printer)}"
        return s

    def do_semantic_analysis(self, scope_handler: ScopeHandler, **kwargs) -> None:
        from SPPCompiler.SemanticAnalysis.ASTs import ConventionMovAst, TokenAst

        # Analyse the iterable.
        self.iterable.do_semantic_analysis(scope_handler, **kwargs)

        # Ensure the iterable is an iterable type.
        given_iterable_type = self.iterable.infer_type(scope_handler, **kwargs)
        empty_iterable_type = InferredType(convention=given_iterable_type.convention, type=given_iterable_type.type.without_generics())

        available_generator_types = Seq([CommonTypes.gen_mov().without_generics(), CommonTypes.gen_ref().without_generics(), CommonTypes.gen_mut().without_generics()])
        available_generator_types = available_generator_types.map(lambda t: InferredType(convention=ConventionMovAst, type=t))
        if not any(empty_iterable_type.symbolic_eq(t, scope_handler.current_scope) for t in available_generator_types):
            raise SemanticErrors.INVALID_ITERABLE_TYPE(self, given_iterable_type)

        yield_type = given_iterable_type.type.types[-1].generic_arguments["Yield"].type

        # Form a let statement with the variable.
        let_statement = LetStatementUninitializedAst(
            pos=self.variable.pos,
            let_keyword=TokenAst.dummy(TokenType.KwLet),
            assign_to=self.variable,
            colon_token=TokenAst.dummy(TokenType.TkColon),
            type_declaration=yield_type)

        let_statement.do_semantic_analysis(scope_handler, **kwargs)

        # Set initialization / borrow asts for the symbol
        if isinstance(self.variable, LocalVariableSingleIdentifierAst):
            variable_symbol = scope_handler.current_scope.get_symbol(self.variable.identifier)
            variable_symbol.memory_info = MemoryStatus(
                ast_initialized=self,
                ast_consumed=None,
                ast_borrow=self if given_iterable_type.type.types[-1].value in ["GenRef", "GenMut"] else None,
                is_borrow_mut=given_iterable_type.type.types[-1].value == "GenMut",
                is_borrow_ref=given_iterable_type.type.types[-1].value == "GenRef")


__all__ = ["LoopExpressionConditionIterableAst"]
