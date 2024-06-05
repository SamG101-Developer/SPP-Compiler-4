import copy
from dataclasses import dataclass
from typing import List, Tuple, Type

from SPPCompiler.LexicalAnalysis.Tokens import TokenType

from SPPCompiler.SemanticAnalysis.ASTMixins.SemanticAnalyser import SemanticAnalyser
from SPPCompiler.SemanticAnalysis.Utils.SemanticError import SemanticError, SemanticErrorType
from SPPCompiler.SemanticAnalysis.Utils.CommonTypes import CommonTypes
from SPPCompiler.SemanticAnalysis.ASTMixins.TypeInfer import TypeInfer
from SPPCompiler.SemanticAnalysis.Utils.Scopes import ScopeHandler

from SPPCompiler.SemanticAnalysis.ASTs.Meta.Ast import Ast
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstPrinter import *
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstUtils import AstUtils

from SPPCompiler.SemanticAnalysis.ASTs.ConventionNonInitAst import ConventionNonInitAst
from SPPCompiler.SemanticAnalysis.ASTs.ConventionMovAst import ConventionMovAst
from SPPCompiler.SemanticAnalysis.ASTs.IdentifierAst import IdentifierAst
from SPPCompiler.SemanticAnalysis.ASTs.PostfixExpressionAst import PostfixExpressionAst
from SPPCompiler.SemanticAnalysis.ASTs.PostfixExpressionOperatorMemberAccessAst import PostfixExpressionOperatorMemberAccessAst

from SPPCompiler.Utils.Sequence import Seq


@dataclass
class AssignmentStatementAst(Ast, SemanticAnalyser, TypeInfer):
    """
    The AssignmentStatementAst node is used to represent a variable being assigned a value. The LHS can be any
    expression but is semantically analysed to ensure that it is a variable or attribute being assigned to. There can be
    multiple LHSs, but only if the "=" token is used; this is also enforced in the semantic analysis.

    Attributes:
        - lhs: The left-hand-side of the assignment, the variable or attribute being assigned to.
        - op: The operator being used in the assignment, ie "=" or "+=".
        - rhs: The right-hand-side of the assignment, the value being assigned to the LHS.
    """

    lhs: List["ExpressionAst"]
    op: "TokenAst"
    rhs: "ExpressionAst"

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        # Print the AssignmentStatementAst.
        s = ""
        s += f"{Seq(self.lhs).print(printer, ", ")} "
        s += f"{self.op.print(printer)} "
        s += f"{self.rhs.print(printer)}"
        return s

    def do_semantic_analysis(self, scope_handler: ScopeHandler, **kwargs) -> None:
        ...

    def infer_type(self, scope_handler: ScopeHandler, **kwargs) -> Tuple[Type["ConventionAst"], "TypeAst"]:
        # Assignment never returns anything, so return the Void type. This is so that the memory rules of the language
        # can be adhered to.
        return ConventionMovAst, CommonTypes.void()


__all__ = ["AssignmentStatementAst"]
