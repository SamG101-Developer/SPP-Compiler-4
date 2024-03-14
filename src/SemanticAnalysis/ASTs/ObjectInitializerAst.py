import copy
from dataclasses import dataclass, field
from typing import Optional, Tuple, Type

from src.LexicalAnalysis.Tokens import TokenType

from src.SemanticAnalysis.Analysis.SemanticAnalysis import SemanticAnalysis
from src.SemanticAnalysis.Analysis.SemanticError import SemanticError
from src.SemanticAnalysis.Symbols.Scopes import ScopeHandler
from src.SemanticAnalysis.Types.TypeInfer import TypeInfer

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
class ObjectInitializerAst(Ast, SemanticAnalysis, TypeInfer):
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
            exception = SemanticError(f"Multiple default values given in object initializer:")
            exception.add_traceback(default_value_given[0].pos, f"1st default value given here.")
            exception.add_traceback_minimal(default_value_given[1].pos, f"2nd default value given here.")
            raise exception

        # Check that the default value's memory status is correct:
        if default_value_given:
            AstUtils.ensure_memory_integrity_of_expression(default_value_given[0].value, scope_handler, **kwargs)

        # Check that the default value is of the correct type:
        if default_value_given:
            default_value_given_type = default_value_given[0].value.infer_type(scope_handler, **kwargs)
            if ConventionMovAst == default_value_given_type[0] and self.class_type.symbolic_eq(default_value_given_type[1], scope_handler.current_scope):
                exception = SemanticError(f"Invalid type default value type:")
                exception.add_traceback(type_sym.type.identifier.pos, f"Object initializer declared here with type '{self.class_type}'.")
                exception.add_traceback_minimal(default_value_given[0].value.pos, f"Object initializer given value here with type '{default_value_given_type[0]}{default_value_given_type[1]}'.")
                raise exception

        # Mark the symbol for the default value as consumed:
        if default_value_given:
            AstUtils.ensure_memory_integrity_of_expression(default_value_given[0].value, scope_handler, keep_consume=True, **kwargs)

        # Check no arguments are given that don't exist on the class; this picks up when too many arguments too via the
        # Pigeonhole Principle:
        if invalid_argument_names := arguments.map(lambda a: a.identifier).filter(lambda arg_name: arg_name not in attribute_names):
            exception = SemanticError(f"Invalid argument names given to function call:")
            exception.add_traceback(type_sym.type.identifier.pos, f"Class {self.class_type} declared here with attributes: {attribute_names}")
            exception.add_traceback_minimal(invalid_argument_names[0].pos, f"Argument <{invalid_argument_names[0]}> found here.")
            raise exception

        # Check that all required attributes have been given a value:
        if not default_value_given and (unfilled_required_attributes := attribute_names.set_subtract(arguments.map(lambda a: a.identifier))):
            exception = SemanticError(f"Missing attribute(s) in object initializer for type '{self.class_type}':")
            exception.add_traceback(type_sym.type.identifier.pos, f"Class '{self.class_type}' declared here with attributes '{attribute_names}'.")
            exception.add_traceback(self.pos, f"Object initializer missing attributes '{unfilled_required_attributes.map(str).join(", ")}'.")
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
                exception = SemanticError(f"Invalid type '{given_argument_type[0]}{given_argument_type[1]}' given to attribute '{attribute.identifier}':")
                exception.add_traceback(attribute.identifier.pos, f"Attribute '{attribute.identifier}' declared here with type '{attribute.type_declaration}'.")
                exception.add_traceback(given_argument.pos, f"Attribute '{attribute.identifier}' given value here with type '{given_argument_type[0]}{given_argument_type[1]}'.")
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
