from dataclasses import dataclass

from SPPCompiler.SemanticAnalysis.ASTs.Meta.Ast import Ast
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstMixins import SemanticAnalyser
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstPrinter import *
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstUtils import TypeInfer, InferredType
from SPPCompiler.SemanticAnalysis.Utils.CommonTypes import CommonTypes
from SPPCompiler.SemanticAnalysis.Utils.Scopes import ScopeHandler
from SPPCompiler.SemanticAnalysis.Utils.SemanticError import SemanticErrors


@dataclass
class PatternVariantUnionDestructureAst(Ast, SemanticAnalyser, TypeInfer):
    """
    """

    type: "TypeAst"

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        s = ""
        s += f"{self.type}"
        return s

    def do_semantic_analysis(self, scope_handler: ScopeHandler, **kwargs) -> None:
        # Ensure that the type is one of the types of the union type being analysed.
        union_type = kwargs["condition"].infer_type(scope_handler, **kwargs).type
        if not union_type.without_generics().symbolic_eq(CommonTypes.var([])):
            raise SemanticErrors.TYPE_DESTRUCTURING_NON_UNION_TYPE(union_type)

    def infer_type(self, scope_handler: ScopeHandler, **kwargs) -> InferredType:
        from SPPCompiler.SemanticAnalysis.ASTs.ConventionMovAst import ConventionMovAst
        return InferredType(convention=ConventionMovAst, type=self.type)
