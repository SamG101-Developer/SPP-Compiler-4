from dataclasses import dataclass
from typing import Optional

from src.SemanticAnalysis.PreProcessor import PreProcessor
from src.SemanticAnalysis.Types.CommonTypes import CommonTypes

from src.SemanticAnalysis.ASTs.Meta.Ast import Ast
from src.SemanticAnalysis.ASTs.Meta.AstPrinter import *

from src.SemanticAnalysis.ASTs.GenericParameterGroupAst import GenericParameterGroupAst
from src.SemanticAnalysis.ASTs.ModulePrototypeAst import ModulePrototypeAst
from src.SemanticAnalysis.ASTs.TokenAst import TokenAst
from src.SemanticAnalysis.ASTs.TypedefStatementItemAst import TypedefStatementItemAst
from src.SemanticAnalysis.ASTs.TypedefStatementOldNamespaceAst import TypedefStatementOldNamespaceAst

from src.Utils.Sequence import Seq


@dataclass
class TypedefStatementAst(Ast, PreProcessor):
    """
    The TypedefStatementAst node is used to represent a typedef statement. This can be used to create a new type from an
    existing type, or to reduce the namespace of a type.
    
    Attributes:
        - use_keyword: The "use" keyword token.
        - generic_parameters: The generic parameters of the typedef statement.
        - old_type_namespace: The old type namespace of the typedef statement.
        - items: The items of the typedef statement.
    """
    
    use_keyword: TokenAst
    generic_parameters: GenericParameterGroupAst
    old_type_namespace: Optional[TypedefStatementOldNamespaceAst]
    items: TypedefStatementItemAst
    
    def __post_init__(self):
        raise NotImplementedError("TypedefStatementAst is not implemented yet.")

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        # Print the TypedefStatementAst.
        s = ""
        s += f"{self.use_keyword.print(printer)}{self.generic_parameters.print(printer)}"
        s += f"{self.old_type_namespace.print(printer)}" if self.old_type_namespace else ""
        s += f"{self.items.print(printer)}"
        return s

    def pre_process(self, context: ModulePrototypeAst) -> None:
        # Substitute the "Self" type in generic parameters.
        Seq(self.generic_parameters.get_opt()).for_each(lambda p: p.default_value.substitute_generics(CommonTypes.self(), context.identifier))
        ...
    

__all__ = ["TypedefStatementAst"]
