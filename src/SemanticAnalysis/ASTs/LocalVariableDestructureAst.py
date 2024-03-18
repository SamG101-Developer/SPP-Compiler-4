from dataclasses import dataclass
from typing import List, Tuple, Type

from src.SemanticAnalysis.ASTMixins.SemanticAnalyser import SemanticAnalyser
from src.SemanticAnalysis.Utils.SemanticError import SemanticError
from src.SemanticAnalysis.ASTMixins.TypeInfer import TypeInfer
from src.SemanticAnalysis.Utils.Scopes import ScopeHandler

from src.SemanticAnalysis.ASTs.Meta.Ast import Ast
from src.SemanticAnalysis.ASTs.Meta.AstPrinter import *

from src.SemanticAnalysis.ASTs.ConventionMovAst import ConventionMovAst
from src.SemanticAnalysis.ASTs.LocalVariableSingleAst import LocalVariableSingleAst
from src.SemanticAnalysis.ASTs.LocalVariableAssignmentAst import LocalVariableAssignmentAst
from src.SemanticAnalysis.ASTs.LocalVariableSkipArgumentAst import LocalVariableSkipArgumentAst

from src.Utils.Sequence import Seq

# todo: docs & comment


@dataclass
class LocalVariableDestructureAst(Ast, SemanticAnalyser, TypeInfer):
    class_type: "TypeAst"
    bracket_l_token: "TokenAst"
    items: List["LocalVariableSingleAst"]
    bracket_r_token: "TokenAst"

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        return f"{self.class_type.print(printer)}{self.bracket_l_token.print(printer)}{Seq(self.items).print(printer, ", ")}{self.bracket_r_token.print(printer)}"

    def do_semantic_analysis(self, scope_handler: ScopeHandler, **kwargs) -> None:
        # Analyse the class type, and get its symbol and attributes.
        self.class_type.do_semantic_analysis(scope_handler, **kwargs)
        class_type_sym = scope_handler.current_scope.get_symbol(self.class_type)
        attributes = Seq(class_type_sym.type.body.members)

        has_skipped_args = None
        for argument in self.items:

            # Check the ".." skip argument is not used more than once.
            if isinstance(argument, LocalVariableSkipArgumentAst):
                if has_skipped_args:
                    exception = SemanticError(f"Multiple '..' given to pattern:")
                    exception.add_traceback(has_skipped_args.pos, f"1st variadic argument given here.")
                    exception.add_traceback_minimal(argument.variadic_token.pos, f"2nd variadic argument given here.")
                    raise exception
                has_skipped_args = argument
                continue

            # Don't allow the "..x" unpacking token in a class destructure pattern.
            unpack = argument.unpack_token if isinstance(argument, LocalVariableSingleAst) else None
            if unpack:
                exception = SemanticError(f"Cannot use the unpack token '..' in a destructure pattern:")
                exception.add_traceback(unpack.pos, f"Unpack token '..' found here.")
                raise exception

            # Check the target variable exists as an attribute on the class type.
            if not attributes.map(lambda a: a.identifier).contains(argument.identifier):
                exception = SemanticError(f"Invalid destructure assignment:")
                exception.add_traceback(self.class_type.pos, f"Class '{self.class_type}' declared here with attributes: {attributes}")
                exception.add_traceback(argument.pos, f"Attribute '{argument.identifier}' not found on class '{self.class_type}'")
                raise exception

            # Special case for binding to an attribute with a pre-assigned value.
            if isinstance(argument, LocalVariableAssignmentAst):

                # Analyse the value
                value = argument.value
                value.do_semantic_analysis(scope_handler, **kwargs)

                # Check the value's type matches the attribute's type.
                corresponding_attribute = attributes.find(lambda attribute: attribute.identifier == argument.identifier)
                value_type = value.infer_type(scope_handler, **kwargs)
                if value_type[0] != ConventionMovAst or not value_type[1].symbolic_eq(corresponding_attribute.type_declaration, scope_handler.current_scope):
                    exception = SemanticError(f"Invalid type '{value_type[0]}{value_type[1]}' given to attribute '{argument.identifier}':")
                    exception.add_traceback(corresponding_attribute.identifier.pos, f"Attribute '{corresponding_attribute.identifier}' declared here with type '{corresponding_attribute.type_declaration}'.")
                    exception.add_traceback(value.pos, f"Attribute '{argument.identifier}' given value here with type '{value_type[0]}{value_type[1]}'.")
                    raise exception

        # Make sure all attributes have been bound to, unless the ".." skip argument is used.
        attributes_assigned_to = Seq(self.items).filter(lambda a: not isinstance(a, LocalVariableSkipArgumentAst))
        if attributes_assigned_to.length < attributes.length and not has_skipped_args:
            exception = SemanticError(f"Missing attribute(s) in pattern for type '{self.class_type}':")
            exception.add_traceback(class_type_sym.type.identifier.pos, f"Class '{self.class_type}' declared here with attributes '{attributes}'.")
            exception.add_traceback(self.pos, f"Initialization missing attributes '{attributes.map(lambda a: a.identifier) - attributes_assigned_to.map(lambda v: v.identifier)}'. Consider adding a '..' to mark variables as deliberately excluded.")
            raise exception

    def infer_type(self, scope_handler: ScopeHandler, **kwargs) -> Tuple[Type["ConventionAst"], "TypeAst"]:
        return ConventionMovAst, self.class_type
