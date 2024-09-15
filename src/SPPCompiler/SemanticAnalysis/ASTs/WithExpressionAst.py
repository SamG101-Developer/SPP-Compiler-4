from dataclasses import dataclass
from typing import Optional

from SPPCompiler.SemanticAnalysis.ASTs.Meta.Ast import Ast
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstMixins import SemanticAnalyser
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstPrinter import *
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstUtils import TypeInfer, InferredType
from SPPCompiler.SemanticAnalysis.Utils.CommonTypes import CommonTypes
from SPPCompiler.SemanticAnalysis.Utils.Scopes import ScopeHandler
from SPPCompiler.SemanticAnalysis.Utils.SemanticError import SemanticErrors
from SPPCompiler.Utils.Sequence import Seq


@dataclass
class WithExpressionAst(Ast, SemanticAnalyser, TypeInfer):
    """
    The WithExpressionAst node represents a context block, where an object's ".enter()" method is called on scope entry,
    and the ".exit()" method is called on scope exit. These methods are defined on the "std.Ctx" type which must be
    superimposed over the type of the object being entered.

    Attributes:
        with_keyword: The "with" keyword.
        alias: The optional alias to use for the object being entered (variable introduction).
        expression: The expression to enter.
        body: The body of the context block.
    """

    with_keyword: "TokenAst"
    alias: Optional["WithExpressionAliasAst"]
    expression: "ExpressionAst"
    body: "InnerScopeAst[StatementAst]"

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        # Print the WithExpressionAst.
        s = ""
        s += f"{self.with_keyword.print(printer)}"
        s += f"{self.alias.print(printer)}" if self.alias else ""
        s += f"{self.expression.print(printer)} {self.body.print(printer)}"
        return s

    def do_semantic_analysis(self, scope_handler: ScopeHandler, **kwargs) -> None:
        # TODO: This won't work (... in sup_scopes): sup_scopes needs transforming
        scope_handler.into_new_scope("<with-block>")

        # Analyse the expression.
        self.expression.do_semantic_analysis(scope_handler, **kwargs)

        # Get the expression's type and its type scope's superimposed scopes.
        expression_type = self.expression.infer_type(scope_handler, **kwargs).type
        sup_scopes = Seq(scope_handler.current_scope.get_symbol(expression_type).associated_scope.sup_scopes).map(lambda s: s[1].identifier.without_generics())
        if not sup_scopes.any(lambda s: CommonTypes.ctx_ref().symbolic_eq(s, scope_handler.current_scope) or CommonTypes.ctx_mut().symbolic_eq(s, scope_handler.current_scope)):
            raise SemanticErrors.INVALID_WITH_EXPRESSION(self.expression, expression_type)

        # Analyse the alias if it exists, including symbol injection.
        if self.alias:
            self.alias.do_semantic_analysis(scope_handler, with_expression=self.expression, **kwargs)

        # Analyse the body and exit the scope.
        self.body.do_semantic_analysis(scope_handler, **kwargs)
        scope_handler.exit_cur_scope()

    def infer_type(self, scope_handler: ScopeHandler, **kwargs) -> InferredType:
        from SPPCompiler.SemanticAnalysis.ASTs.ConventionMovAst import ConventionMovAst

        # Return the final expression's type, or "Void" for an empty body.
        if self.body.members:
            return self.body.members[-1].infer_type(scope_handler, **kwargs)
        return InferredType(convention=ConventionMovAst, type=CommonTypes.void(pos=self.pos))


__all__ = ["WithExpressionAst"]
