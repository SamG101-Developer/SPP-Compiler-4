from __future__ import annotations
from dataclasses import dataclass
from typing import List

from src.SemanticAnalysis.Analysis.SemanticAnalysis import SemanticAnalysis

from src.SemanticAnalysis.ASTs.Meta.Ast import Ast
from src.SemanticAnalysis.ASTs.Meta.AstPrinter import *

from src.Utils.Sequence import Seq


@dataclass
class ClassAttributeAst(Ast, SemanticAnalysis):
    """
    The ClassAttributeAst node is used to represent an attribute in a ClassPrototypeAst node. The attribute contains the
    name and type of the attribute, and any annotations that are attached to the attribute.

    Attributes:
        - annotations: The annotations attached to the attribute.
        - identifier: The name of the attribute.
        - colon_token: The colon token.
        - type_declaration: The type of the attribute.
    """

    annotations: List[AnnotationAst]
    identifier: IdentifierAst
    colon_token: TokenAst
    type_declaration: TypeAst

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
        # Check the annotations and that the type is valid.
        Seq(self.annotations).for_each(lambda a: a.do_semantic_analysis(scope_handler, **kwargs))
        self.type_declaration.do_semantic_analysis(scope_handler, **kwargs)


__all__ = ["ClassAttributeAst"]
