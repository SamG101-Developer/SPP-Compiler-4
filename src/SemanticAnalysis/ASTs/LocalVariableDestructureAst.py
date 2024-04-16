from dataclasses import dataclass
from typing import List, Tuple, Type

from src.SemanticAnalysis.ASTMixins.SemanticAnalyser import SemanticAnalyser
from src.SemanticAnalysis.Utils.SemanticError import SemanticError, SemanticErrorStringFormatType, SemanticErrorType
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
                    exception = SemanticError()
                    exception.add_info(
                        pos=has_skipped_args.pos,
                        tag_message=f"1st argument skip here")
                    exception.add_error(
                        pos=argument.variadic_token.pos,
                        error_type=SemanticErrorType.ORDER_ERROR,
                        message=f"Cannot have multiple skip arguments '..' in a destructure pattern",
                        tag_message=f"2nd argument skip here",
                        tip="Remove the additional skip argument")
                    raise exception
                has_skipped_args = argument
                continue

            # Don't allow the "..x" unpacking token in a class destructure pattern.
            # todo: what does this do? (it works fine for 1 ".." to be used)
            unpack = argument.unpack_token if isinstance(argument, LocalVariableSingleAst) else None
            if unpack:
                raise SemanticError().add_error(
                    pos=unpack.pos,
                    error_type=SemanticErrorType.ORDER_ERROR,
                    message="Cannot use the unpack token '..' in a destructure pattern",
                    tag_message="Unpack token '..' found here",
                    tip="Remove the unpack token '..' from the destructure pattern")

            # Check the target variable exists as an attribute on the class type.
            if not attributes.map(lambda a: a.identifier).contains(argument.identifier):
                exception = SemanticError()
                exception.add_info(
                    pos=class_type_sym.type.identifier.pos,
                    tag_message=f"Class '{self.class_type}' declared here with attributes: {attributes.map(lambda a: a.identifier).map(str).join(', ')}")
                exception.add_error(
                    pos=argument.identifier.pos,
                    error_type=SemanticErrorType.ORDER_ERROR,
                    message=f"Attribute '{argument.identifier}' not found on class '{self.class_type}'",
                    tag_message=f"Attribute '{argument.identifier}' not found on class '{self.class_type}'",
                    tip="Check the class type and attribute names")
                raise exception

            # Special case for binding to an attribute with a pre-assigned value, for example let p = Vec(pos=Point(x, y)).
            if isinstance(argument, LocalVariableAssignmentAst):
                # Analyse the value
                value = argument.value
                value.do_semantic_analysis(scope_handler, **kwargs)

                # Check the value's type matches the attribute's type.
                corresponding_attribute = attributes.find(lambda attribute: attribute.identifier == argument.identifier)
                value_type = value.infer_type(scope_handler, **kwargs)
                if value_type[0] != ConventionMovAst or not value_type[1].symbolic_eq(corresponding_attribute.type_declaration, scope_handler.current_scope):
                    exception = SemanticError()
                    exception.add_info(
                        pos=corresponding_attribute.identifier.pos,
                        tag_message=f"Attribute '{argument.identifier}' declared here with type '{corresponding_attribute.type_declaration}'.")
                    exception.add_error(
                        pos=argument.identifier.pos,
                        error_type=SemanticErrorType.TYPE_ERROR,
                        message=f"Type mismatch between attribute '{argument.identifier}' and value",
                        tag_message=f"Attribute '{argument.identifier}' given value here with type '{value_type[0]}{value_type[1]}'.",
                        tip="Check the type of the value being assigned to the attribute")
                    raise exception

        # Make sure all attributes have been bound to, unless the ".." skip argument is used.
        attributes_assigned_to = Seq(self.items).filter(lambda a: not isinstance(a, LocalVariableSkipArgumentAst))
        if attributes_assigned_to.length < attributes.length and not has_skipped_args:
            missing_attributes = attributes.map(lambda a: a.identifier).set_subtract(attributes_assigned_to.map(lambda a: a.identifier))
            exception = SemanticError()
            exception.add_info(
                pos=class_type_sym.type.identifier.pos,
                tag_message=f"Class '{self.class_type}' declared here with attributes: {attributes.map(lambda a: a.identifier).map(str).join(', ')}")
            exception.add_error(
                pos=self.pos,
                error_type=SemanticErrorType.ORDER_ERROR,
                message=f"Missing attribute(s) in pattern for type '{self.class_type}'",
                tag_message=f"Initialization missing attributes: {missing_attributes.map(str).join(', ')}",
                tip="Consider adding a '..' to mark variables as deliberately excluded")
            raise exception

    def infer_type(self, scope_handler: ScopeHandler, **kwargs) -> Tuple[Type["ConventionAst"], "TypeAst"]:
        return ConventionMovAst, self.class_type
