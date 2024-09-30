from dataclasses import dataclass, field

from SPPCompiler.SemanticAnalysis.ASTs.Meta.Ast import Ast
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstMixins import SemanticAnalyser, PreProcessor
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstPrinter import *
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstUtils import VisibilityEnabled, Visibility
from SPPCompiler.SemanticAnalysis.Utils.Scopes import ScopeHandler
from SPPCompiler.SemanticAnalysis.Utils.SemanticError import SemanticErrors
from SPPCompiler.Utils.Sequence import Seq


@dataclass
class AnnotationAst(Ast, PreProcessor, SemanticAnalyser):
    """
    The AnnotationAst node represents an annotation to a module or sup level definition (over a function, class, etc).
    Custom annotations can only be applied to functions.

    Attributes:
        at_token: The @ token.
        identifier: The identifier(s) of the annotation, "::" separated.
        # function_call: The function call of the annotation.
    """

    at_token: "TokenAst"
    identifier: "IdentifierAst | PostfixExpressionAst"
    # function_call: "PostfixExpressionOperatorFunctionCallAst"

    _builtin: bool = field(default=True, init=False, repr=False)

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        # Print the AnnotationAst.
        s = ""
        s += f"{self.at_token.print(printer)}"
        s += f"{Seq(self.identifier).join("::")}"
        # s += f"{self.function_call.print(printer)}"
        return s

    def pre_process(self, context) -> None:
        match Seq(self.identifier).map(lambda x: x.value).list():
            case ["virtual_method"]:
                context._virtual = True

            case ["abstract_method"]:
                context._virtual = context._abstract = True

            case ["public"]:
                if not isinstance(context, VisibilityEnabled): raise SemanticErrors.INVALID_ACCESS_MODIFIER_APPLICATION(self.pos, context)
                context.visibility = Visibility.Public

            case ["protected"]:
                if not isinstance(context, VisibilityEnabled): raise SemanticErrors.INVALID_ACCESS_MODIFIER_APPLICATION(self.pos, context)
                context.visibility = Visibility.Protected

            case ["private"]:
                if not isinstance(context, VisibilityEnabled): raise SemanticErrors.INVALID_ACCESS_MODIFIER_APPLICATION(self.pos, context)
                context.visibility = Visibility.Private

            case _:
                raise NotImplementedError()

    def do_semantic_analysis(self, scope_handler: ScopeHandler, **kwargs) -> None:
        ...
        # from SPPCompiler.SemanticAnalysis.ASTs import PostfixExpressionAst
        #
        # Merge the identifier and function call into a PostfixExpressionAst and analyse it.
        # if not self._builtin:
        #     postfix_expression = PostfixExpressionAst(self.pos, self.identifier, self.function_call)
        #     postfix_expression.do_semantic_analysis(scope_handler, **kwargs)


__all__ = ["AnnotationAst"]
