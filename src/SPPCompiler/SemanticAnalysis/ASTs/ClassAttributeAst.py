from __future__ import annotations
from dataclasses import dataclass
from typing import List

from SPPCompiler.SemanticAnalysis.ASTs.Meta.Ast import Ast
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstMixins import SemanticAnalyser
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstPrinter import *
from SPPCompiler.SemanticAnalysis.Utils.CommonTypes import CommonTypes
from SPPCompiler.SemanticAnalysis.Utils.SemanticError import SemanticErrors
from SPPCompiler.Utils.Sequence import Seq


@dataclass
class ClassAttributeAst(Ast, SemanticAnalyser):
    """
    The ClassAttributeAst node is used to represent an attribute in a ClassPrototypeAst node. The attribute contains the
    name and its type, and any annotations that are attached to the attribute.

    Attributes:
        annotations: The annotations attached to the attribute.
        identifier: The name of the attribute.
        colon_token: The colon token.
        type_declaration: The attribute's type.
    """

    annotations: List["AnnotationAst"]
    identifier: "IdentifierAst"
    colon_token: "TokenAst"
    type_declaration: "TypeAst"

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        # Print the ClassAttributeAst
        s = ""
        s += f"{Seq(self.annotations).print(printer, "\n")}"
        s += f"{self.identifier.print(printer)}"
        s += f"{self.colon_token.print(printer)} "
        s += f"{self.type_declaration.print(printer)}"
        return s

    def do_semantic_analysis(self, scope_handler, **kwargs) -> None:
        self.type_declaration.do_semantic_analysis(scope_handler, **kwargs)
        if self.type_declaration.symbolic_eq(CommonTypes.void(self.pos), scope_handler.current_scope):
            raise SemanticErrors.INVALID_CLASS_ATTRIBUTE_TYPE(self.type_declaration)


__all__ = ["ClassAttributeAst"]
