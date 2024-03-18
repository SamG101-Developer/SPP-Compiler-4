from dataclasses import dataclass
from typing import Optional, Tuple, Type

from src.SemanticAnalysis.ASTMixins.SemanticAnalyser import SemanticAnalyser
from src.SemanticAnalysis.Utils.SemanticError import SemanticError
from src.SemanticAnalysis.Utils.Scopes import ScopeHandler
from src.SemanticAnalysis.Utils.CommonTypes import CommonTypes
from src.SemanticAnalysis.ASTMixins.TypeInfer import TypeInfer

from src.SemanticAnalysis.ASTs.Meta.Ast import Ast
from src.SemanticAnalysis.ASTs.Meta.AstPrinter import *

from src.SemanticAnalysis.ASTs.ConventionMovAst import ConventionMovAst


@dataclass
class WithExpressionAst(Ast, SemanticAnalyser, TypeInfer):
    """
    The WithExpressionAst node represents a context block, where an object's ".enter()" method is called on scope entry,
    and the ".exit()" method is called on scope exit. These methods are defined on the "std.Ctx" type which must be
    superimposed over the type of the object being entered.

    Attributes:
        - with_keyword: The "with" keyword.
        - alias: The optional alias to use for the object being entered (variable introduction).
        - expression: The expression to enter.
        - body: The body of the context block.
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
        # Enter the new scope.
        scope_handler.into_new_scope("<with-expression>")

        # Check that the type of object used in the "with" expression superimposes Ctx.
        self.expression.do_semantic_analysis(scope_handler, **kwargs)
        object_type = self.expression.infer_type(scope_handler, **kwargs)
        object_type_sup_types = scope_handler.current_scope.get_symbol(object_type).associated_scope.sup_scopes
        if CommonTypes.ctx() not in object_type_sup_types:
            exception = SemanticError(f"Type '{object_type}' does not superimpose Ctx:")
            exception.add_traceback(self.expression.pos, f"Expression '{self.expression}' has type '{object_type}'.")
            raise exception

        # Create the symbol for the alias.
        if self.alias:
            self.alias.do_semantic_analysis(scope_handler, with_expression=self.expression, **kwargs)

        # Analyse the body and exit the scope.
        self.body.do_semantic_analysis(scope_handler, **kwargs)
        scope_handler.exit_cur_scope()

    def infer_type(self, scope_handler: ScopeHandler, **kwargs) -> Tuple[Type["ConventionAst"], "TypeAst"]:
        # Return the type of the final expression, or "Void" for an empty body.
        if self.body.members:
            return self.body.members[-1].infer_type(scope_handler, **kwargs)
        return ConventionMovAst, CommonTypes.void(pos=self.pos)
