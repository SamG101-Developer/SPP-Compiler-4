import copy
from dataclasses import dataclass, field
from typing import Optional, Tuple, Type

from src.LexicalAnalysis.Tokens import TokenType

from src.SemanticAnalysis.ASTMixins.SemanticAnalyser import SemanticAnalyser
from src.SemanticAnalysis.Utils.SemanticError import SemanticError, SemanticErrorStringFormatType, SemanticErrorType
from src.SemanticAnalysis.Utils.Scopes import ScopeHandler
from src.SemanticAnalysis.ASTMixins.TypeInfer import TypeInfer

from src.SemanticAnalysis.ASTs.Meta.Ast import Ast
from src.SemanticAnalysis.ASTs.Meta.AstPrinter import *
from src.SemanticAnalysis.ASTs.Meta.AstUtils import AstUtils

from src.SemanticAnalysis.ASTs.ConventionAst import ConventionAst
from src.SemanticAnalysis.ASTs.ConventionMovAst import ConventionMovAst
from src.SemanticAnalysis.ASTs.IdentifierAst import IdentifierAst
from src.SemanticAnalysis.ASTs.ObjectInitializerArgumentGroupAst import ObjectInitializerArgumentGroupAst
from src.SemanticAnalysis.ASTs.ObjectInitializerArgumentNamedAst import ObjectInitializerArgumentNamedAst
from src.SemanticAnalysis.ASTs.TokenAst import TokenAst
from src.SemanticAnalysis.ASTs.TypeAst import TypeAst

from src.Utils.Sequence import Seq


@dataclass
class ObjectInitializerAst(Ast, SemanticAnalyser, TypeInfer):
    """
    The ObjectInitializerAst node represents the construction of an object of a class type, with the given arguments.

    Attributes:
        - class_type: The type of the object to be constructed.
        - arguments: The arguments to be given to the object's constructor.

        - _modified_type: The type of the object to be constructed, with generic arguments inferred.
    """

    class_type: TypeAst
    arguments: ObjectInitializerArgumentGroupAst
    _modified_type: Optional[TypeAst] = field(default=None, init=False)

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        # Print the ObjectInitializerAst.
        s = ""
        s += f"{self.class_type.print(printer)}"
        s += f"{self.arguments.print(printer)}"
        return s

    def do_semantic_analysis(self, scope_handler: ScopeHandler, **kwargs) -> None:
        # Analyse the class type to make sure it exists.
        self.class_type.do_semantic_analysis(scope_handler, **kwargs)

        # Get the type symbols and scopes for the class type.
        # TODO: move most of this into the ObjectInitializerArgumentGroupAst
        type_sym = scope_handler.current_scope.get_symbol(self.class_type)
        type_scope = type_sym.associated_scope

        # The only way a type symbol can exist without a type is if it is a generic type, which are non-instantiable.
        if not type_sym.type:
            exception = SemanticError()
            exception.add_info(
                pos=type_sym.name.pos,
                tag_message=f"Generic type '{self.class_type}' defined here")
            exception.add_error(
                pos=self.class_type.pos,
                error_type=SemanticErrorType.TYPE_ERROR,
                message=f"Class '{self.class_type}' does not exist",
                tag_message=f"Object initializer declared here",
                tip="Only instantiate classes that have been defined")
            raise exception

        attributes = Seq(type_sym.type.body.members)
        attribute_names = attributes.map(lambda s: s.identifier)
        sup_classes = type_scope.exclusive_sup_scopes

        # Analyse the arguments being given to the object initializer.
        self.arguments.do_semantic_analysis(scope_handler, **kwargs)
        arguments = Seq(self.arguments.arguments)

        # Check if a default value has been given in "else=...":
        default_value_given = (arguments
               .filter(lambda a: isinstance(a, ObjectInitializerArgumentNamedAst))
               .filter(lambda a: isinstance(a.identifier, TokenAst) and a.identifier.token.token_type == TokenType.KwElse))

        # Check if a tuple of superclass instances has been given in "sup=(...)"
        sup_value_given = (arguments
               .filter(lambda a: isinstance(a, ObjectInitializerArgumentNamedAst))
               .filter(lambda a: isinstance(a.identifier, TokenAst) and a.identifier.token.token_type == TokenType.KwSup))

        # Filter the arguments to attribute arguments (remove "else" and "sup" arguments)
        arguments = arguments.filter(lambda a: isinstance(a.identifier, IdentifierAst))

        # Check there is a maximum of 1 default value given:
        if default_value_given.length > 1:
            exception = SemanticError()
            exception.add_info(
                pos=default_value_given[0].pos,
                tag_message=f"1st default value given here")
            exception.add_error(
                pos=default_value_given[1].pos,
                error_type=SemanticErrorType.ORDER_ERROR,
                message="Multiple default values given in object initializer",
                tag_message=f"2nd default value given here",
                tip="Remove the additional default value")
            raise exception

        # Check that the default value's memory status is correct:
        if default_value_given:
            AstUtils.ensure_memory_integrity_of_expression(default_value_given[0].value, scope_handler, **kwargs)

        # Check that the default value is of the correct type:
        if default_value_given:
            default_value_given_type = default_value_given[0].value.infer_type(scope_handler, **kwargs)
            if default_value_given_type[0] != ConventionMovAst or not self.class_type.symbolic_eq(default_value_given_type[1], scope_handler.current_scope):
                exception = SemanticError()
                exception.add_info(
                    pos=self.class_type.pos,
                    tag_message=f"Type '{self.class_type}' being instantiated here")
                exception.add_error(
                    pos=default_value_given[0].value.pos,
                    error_type=SemanticErrorType.TYPE_ERROR,
                    message=f"The default value's type must match the class type",
                    tag_message=f"Default value inferred as '{default_value_given_type[0]}{default_value_given_type[1]}' here",
                    tip=f"Ensure the default value is of type '{self.class_type}'")
                raise exception

        # Mark the symbol for the default value as consumed:
        if default_value_given:
            AstUtils.ensure_memory_integrity_of_expression(default_value_given[0].value, scope_handler, keep_consume=True, **kwargs)

        # Check no arguments are given that don't exist on the class; this picks up when too many arguments too via the
        # Pigeonhole Principle:
        if invalid_argument_names := arguments.map(lambda a: a.identifier).filter(lambda arg_name: arg_name not in attribute_names):
            exception = SemanticError()
            exception.add_info(
                pos=type_sym.type.identifier.pos,
                tag_message=f"Class '{self.class_type}' declared here with attributes: {attribute_names.map(str).join(', ')}")
            exception.add_error(
                pos=invalid_argument_names[0].pos,
                error_type=SemanticErrorType.ORDER_ERROR,
                message="Invalid argument names given to class instantiation",
                tag_message=f"Argument '{invalid_argument_names[0]}' found here",
                tip="Ensure the argument names exist as attributes on the class")
            raise exception

        # Check that all required attributes have been given a value:
        if not default_value_given and (unfilled_required_attributes := attribute_names.set_subtract(arguments.map(lambda a: a.identifier))):
            exception = SemanticError()
            exception.add_info(
                pos=type_sym.type.identifier.pos,
                tag_message=f"Class '{self.class_type}' declared here with attributes: {attribute_names.map(str).join(', ')}")
            exception.add_error(
                pos=self.pos,
                error_type=SemanticErrorType.ORDER_ERROR,
                message="Missing attribute(s) in object initializer",
                tag_message=f"Object initializer declared here",
                tip=f"Give the attributes {unfilled_required_attributes.map(str).join(', ')} values in the object initializer, or pass a default value using 'else=...'.")
            raise exception

        # Check that no attributes are given a value more than once:
        if duplicate_attributes := arguments.map(lambda a: a.identifier).non_unique_items():
            duplicate_attributes = duplicate_attributes[0]
            exception = SemanticError()
            exception.add_info(
                pos=duplicate_attributes[0].pos,
                tag_message=f"1st instance of attribute '{duplicate_attributes[0]}' given here")
            exception.add_error(
                pos=duplicate_attributes[1].pos,
                error_type=SemanticErrorType.ORDER_ERROR,
                message="Duplicate attributes given in object initializer",
                tag_message=f"2nd instance of attribute '{duplicate_attributes[1]}' given here",
                tip="Remove the additional attribute")
            raise exception

        inferred_generic_arguments = AstUtils.infer_generic_argument_values(
            scope_handler=scope_handler,
            generic_parameters=Seq(type_sym.type.generic_parameters.parameters),
            infer_from=attributes.map(lambda p: p.type_declaration),
            replace_with=Seq(self.arguments.arguments).map(lambda a: a.value.infer_type(scope_handler, **kwargs)[1]),
            obj_definition=type_sym.type)

        all_generic_arguments = AstUtils.verify_generic_arguments(
            generic_parameters=Seq(type_sym.type.generic_parameters.parameters),
            inferred_generic_arguments=inferred_generic_arguments,
            generic_arguments=Seq(self.class_type.parts[-1].generic_arguments.arguments),
            obj_definition=type_sym.type,
            usage=self,
            scope_handler=scope_handler,
            **kwargs)

        modified_type = copy.deepcopy(self.class_type)
        modified_type.parts[-1].generic_arguments.arguments = all_generic_arguments.value
        modified_type.do_semantic_analysis(scope_handler, **kwargs)
        self._modified_type = modified_type

        class_scope = type_scope
        type_sym = scope_handler.current_scope.get_symbol(modified_type)
        type_scope = type_sym.associated_scope
        attributes = Seq(type_sym.type.body.members)  # re-get attributes (generic type substitution)

        # Check that each attribute's given value is of the correct type:
        for attribute in attributes:
            given_argument = arguments.find(lambda a: a.identifier == attribute.identifier)

            # No argument given means that the default value will be used.
            # TODO : partial move needs to be registered for the default value where certain attributes are taken. Or
            #  just consume the whole thing?
            if not given_argument:
                continue

            original_pos = given_argument.pos
            given_argument = given_argument.value if isinstance(given_argument, ObjectInitializerArgumentNamedAst) else given_argument.identifier

            # Check the memory status of the object initializer argument
            AstUtils.ensure_memory_integrity_of_expression(given_argument, scope_handler, **kwargs)

            # Type check
            given_argument_type = given_argument.infer_type(scope_handler, **kwargs)
            if given_argument_type[0] != ConventionMovAst or not given_argument_type[1].symbolic_eq(attribute.type_declaration, type_scope, class_scope):
                exception = SemanticError()
                exception.add_info(
                    pos=attribute.identifier.pos,
                    tag_message=f"Attribute '{attribute.identifier}' defined here with type '{attribute.type_declaration}'")
                exception.add_error(
                    pos=original_pos,
                    error_type=SemanticErrorType.TYPE_ERROR,
                    message=f"Invalid type given to attribute",
                    tag_message=f"Attribute '{attribute.identifier}' given value here with type '{given_argument_type[0]}{given_argument_type[1]}'",
                    tip=f"Ensure the attribute's type matches the given value's type")
                raise exception

        # Check that each parent class has a value given in "sup=", if the parent class is stateful:
        parent_classes = type_scope.exclusive_sup_scopes

        # TODO : below
        # Check that each parent class has a value given in "sup=", if the parent class is stateful:
        # for parent_class in Seq(sup_classes).filter(lambda s: s.all_symbols(exclusive=True).filter(lambda s: isinstance(s, VariableSymbol)).length > 0):
        #     sup_argument = (Seq(self.arguments.arguments)
        #             .filter(lambda a: isinstance(a, ObjectInitializerArgumentNamedAst))
        #             .filter(lambda a: isinstance(a.identifier, TokenAst) and a.identifier.token.token_type == TokenType.KwSup))
        #
        #     # Check that there is a maximum of 1 "sup=" argument given:
        #     if sup_argument.length > 1:
        #         exception = SemanticError(f"Multiple 'sup=' arguments given in object initializer:")
        #         exception.add_traceback(sup_argument[0].pos, f"1st 'sup=' argument given here.")
        #         exception.add_traceback(sup_argument[1].pos, f"2nd 'sup=' argument given here.")
        #         raise exception
        #
        #     # Check that the "sup=" argument is of the correct type:
        #     sup_class_types = Seq(sup_classes).map(lambda s: s.type).value
        #     if sup_argument and sup_argument[0].value.infer_type(scope_handler, **kwargs) != CommonTypes.tuple(sup_class_types):
        #         exception = SemanticError(f"Invalid type '{sup_argument[0].value.infer_type(scope_handler, **kwargs)}' given to 'sup=':")
        #         exception.add_traceback(sup_argument[0].value.pos, f"'sup=' argument given here with type '{sup_argument[0].value.infer_type(scope_handler, **kwargs)}', instead of '{CommonTypes.tuple(sup_class_types)}'.")
        #         exception.add_traceback(self.pos, f"Object initializer declared here.")
        #         raise exception

        # TODO : generic arguments

    def infer_type(self, scope_handler: ScopeHandler, **kwargs) -> Tuple[Type[ConventionAst], TypeAst]:
        # The returning type of the object initializer is the type of the object being constructed.
        return ConventionMovAst, self._modified_type


__all__ = ["ObjectInitializerAst"]
