import copy
from dataclasses import dataclass
from typing import Optional, Tuple, Type

from SPPCompiler.SemanticAnalysis.ASTMixins.SemanticAnalyser import SemanticAnalyser
from SPPCompiler.SemanticAnalysis.Utils.SemanticError import SemanticError, SemanticErrorStringFormatType, SemanticErrorType
from SPPCompiler.SemanticAnalysis.Utils.Scopes import Scope, ScopeHandler
from SPPCompiler.SemanticAnalysis.Utils.Symbols import TypeSymbol
from SPPCompiler.SemanticAnalysis.ASTMixins.TypeInfer import TypeInfer

from SPPCompiler.LexicalAnalysis.Tokens import TokenType

from SPPCompiler.SemanticAnalysis.ASTs.Meta.Ast import Ast
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstPrinter import *
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstUtils import AstUtils

from SPPCompiler.SemanticAnalysis.ASTs.ConventionMovAst import ConventionMovAst
from SPPCompiler.SemanticAnalysis.ASTs.ConventionNonInitAst import ConventionNonInitAst
from SPPCompiler.SemanticAnalysis.ASTs.ConventionPartInitAst import ConventionPartInitAst
from SPPCompiler.SemanticAnalysis.ASTs.FunctionArgumentNamedAst import FunctionArgumentNamedAst
from SPPCompiler.SemanticAnalysis.ASTs.FunctionArgumentNormalAst import FunctionArgumentNormalAst
from SPPCompiler.SemanticAnalysis.ASTs.GenericArgumentGroupAst import GenericArgumentGroupAst
from SPPCompiler.SemanticAnalysis.ASTs.GenericIdentifierAst import GenericIdentifierAst
from SPPCompiler.SemanticAnalysis.ASTs.IdentifierAst import IdentifierAst
from SPPCompiler.SemanticAnalysis.ASTs.PostfixExpressionAst import PostfixExpressionAst
from SPPCompiler.SemanticAnalysis.ASTs.TokenAst import TokenAst
from SPPCompiler.SemanticAnalysis.ASTs.TypeSingleAst import TypeSingleAst

from SPPCompiler.Utils.Sequence import Seq


@dataclass
class PostfixExpressionOperatorFunctionCallAst(Ast, SemanticAnalyser, TypeInfer):
    """
    The PostfixExpressionOperatorFunctionCallAst node represents a function call operation, with given generic arguments
    and function arguments. This node also resolves overloads at compile time. Folding can be applied to a function for
    tuple operations.

    Attributes:
        - generic_arguments: The generic arguments of the function call.
        - arguments: The arguments of the function call.
        - fold_token: The optional fold token of the function call.
    """

    generic_arguments: Optional["GenericArgumentGroupAst"]
    arguments: "FunctionArgumentGroupAst"
    fold_token: Optional["TokenAst"]

    def __post_init__(self):
        self.generic_arguments = self.generic_arguments or GenericArgumentGroupAst.default()

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        # Print the PostfixExpressionOperatorFunctionCallAst.
        s = ""
        s += f"{self.generic_arguments.print(printer)}" if self.generic_arguments else ""
        s += f"{self.arguments.print(printer)}"
        s += f"{self.fold_token.print(printer)}" if self.fold_token else ""
        return s

    def __get_matching_overload(self, scope_handler: ScopeHandler, function_name: "ExpressionAst", **kwargs) -> tuple["FunctionPrototypeAst", Scope, Optional["FunctionArgumentNamedAst"]]:
        ...

    def do_semantic_analysis(self, scope_handler: ScopeHandler, lhs: "ExpressionAst" = None, **kwargs) -> None:
        ...

    def infer_type(self, scope_handler: ScopeHandler, lhs: "ExpressionAst" = None, **kwargs) -> Tuple[Type["ConventionAst"], "TypeAst"]:
        # Get the matching overload and return its return-type. 2nd class borrows mean the object returned is always
        # owned, ie ConventionMovAst.
        function_proto, function_scope, _ = self.__get_matching_overload(scope_handler, lhs, **kwargs)
        function_return_type = copy.deepcopy(function_proto.return_type)
        function_return_type = function_scope.get_symbol(function_return_type).fq_type
        return ConventionMovAst, function_return_type


__all__ = ["PostfixExpressionOperatorFunctionCallAst"]
