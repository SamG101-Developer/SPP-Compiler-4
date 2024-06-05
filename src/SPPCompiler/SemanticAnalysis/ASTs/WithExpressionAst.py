from dataclasses import dataclass
from typing import Optional, Tuple, Type

from SPPCompiler.SemanticAnalysis.ASTMixins.SemanticAnalyser import SemanticAnalyser
from SPPCompiler.SemanticAnalysis.Utils.SemanticError import SemanticError, SemanticErrorType
from SPPCompiler.SemanticAnalysis.Utils.Scopes import ScopeHandler
from SPPCompiler.SemanticAnalysis.Utils.CommonTypes import CommonTypes
from SPPCompiler.SemanticAnalysis.ASTMixins.TypeInfer import TypeInfer

from SPPCompiler.SemanticAnalysis.ASTs.Meta.Ast import Ast
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstPrinter import *

from SPPCompiler.SemanticAnalysis.ASTs.ConventionMovAst import ConventionMovAst


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
        ...

    def infer_type(self, scope_handler: ScopeHandler, **kwargs) -> Tuple[Type["ConventionAst"], "TypeAst"]:
        # Return the type of the final expression, or "Void" for an empty body.
        if self.body.members:
            return self.body.members[-1].infer_type(scope_handler, **kwargs)
        return ConventionMovAst, CommonTypes.void(pos=self.pos)
