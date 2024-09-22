from dataclasses import dataclass
from typing import List

from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstMixins import SemanticAnalyser
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstUtils import InferredType, ensure_memory_integrity
from SPPCompiler.SemanticAnalysis.ASTs.Meta.Ast import Ast
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstPrinter import *
from SPPCompiler.SemanticAnalysis.Utils.CommonTypes import CommonTypes
from SPPCompiler.SemanticAnalysis.Utils.Scopes import ScopeHandler
from SPPCompiler.SemanticAnalysis.Utils.SemanticError import SemanticErrors
from SPPCompiler.SemanticAnalysis.Utils.Symbols import VariableSymbol
from SPPCompiler.Utils.Sequence import Seq


@dataclass
class ObjectInitializerArgumentGroupAst(Ast, SemanticAnalyser):
    """
    The ObjectInitializerArgumentGroupAst node is used to represent the arguments of an object initializer. It contains
    named and normal arguments of the object initializer.

    Attributes:
        paren_l_token: The left parenthesis token.
        arguments: The arguments of the object initializer.
        paren_r_token: The right parenthesis token.
    """

    paren_l_token: "TokenAst"
    arguments: List["ObjectInitializerArgumentAst"]
    paren_r_token: "TokenAst"

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        # Print the object initializer argument group.
        s = ""
        s += f"{self.paren_l_token.print(printer)}"
        s += f"{Seq(self.arguments).print(printer, ", ")}" if self.arguments else ""
        s += f"{self.paren_r_token.print(printer)}"
        return s

    def do_semantic_pre_analysis(self, scope_handler: ScopeHandler, **kwargs) -> None:
        from SPPCompiler.SemanticAnalysis.ASTs import IdentifierAst, PostfixExpressionAst

        attribute_identifiers = kwargs["attributes"]

        # Analyse the arguments of the object initializer, and ensure the memory integrity of the arguments.
        for argument in self.arguments:
            argument.do_semantic_analysis(scope_handler, **kwargs)
            ensure_memory_integrity(self, self.get_argument_value(argument), argument, scope_handler)

        # Check there are no duplicate named-argument identifiers for this group, and raise an exception if there are.
        named_arguments = Seq(self.arguments).map(lambda a: a.identifier)
        if named_arguments.contains_duplicates():
            duplicate_named_argument_identifiers = named_arguments.non_unique_items()[0]
            raise SemanticErrors.DUPLICATE_ITEM(duplicate_named_argument_identifiers, "object initializer argument")

        # Split the arguments into default, sup and normal/named arguments.
        def_arguments = self.get_def_args()
        sup_arguments = self.get_sup_args()
        arguments = Seq(self.arguments).filter(lambda a: isinstance(a.identifier, IdentifierAst))
        argument_identifiers = arguments.map(lambda a: a.identifier)

        # Check there is a maximum of one default argument and one "sup" argument.
        if len(def_arguments) > 1:
            raise SemanticErrors.DUPLICATE_ITEM(def_arguments, "default object initializer argument")
        if len(sup_arguments) > 1:
            raise SemanticErrors.DUPLICATE_ITEM(sup_arguments, "sup object initializer argument")

        def_argument = def_arguments[0] if def_arguments else None

        # Mark the default value as moved.
        # Todo: mark it as partially moved with all the necessary attributes moved.
        if def_argument:
            match def_argument.value:
                case IdentifierAst(): scope_handler.current_scope.get_symbol(def_argument.value).memory_info.ast_consumed = def_argument
                case PostfixExpressionAst(): scope_handler.current_scope.get_outermost_variable_symbol(def_argument.value).memory_info.ast_partial_moves.append(def_argument.value)

        # Check all the arguments are attributes of the class.
        if invalid_arguments := argument_identifiers.set_subtract(attribute_identifiers):
            raise SemanticErrors.UNKNOWN_IDENTIFIER(invalid_arguments[0], attribute_identifiers.map(lambda i: i.value).list(), "attribute")

        # Check all the attributes have been assigned a value.
        if not def_argument and (missing_arguments := attribute_identifiers.set_subtract(argument_identifiers)):
            raise SemanticErrors.MISSING_ARGUMENT(self, missing_arguments[0], "object initializer", "attribute")

    def do_semantic_analysis(self, scope_handler: ScopeHandler, **kwargs) -> None:
        from SPPCompiler.SemanticAnalysis.ASTs import ConventionMovAst, TypeAst, ClassPrototypeAst

        class_type = kwargs["class-type"]
        type_symbol = scope_handler.current_scope.get_symbol(class_type)
        attributes = Seq(type_symbol.associated_scope.all_symbols(exclusive=True)).filter_to_type(VariableSymbol)

        # Check each argument is the correct type for the attribute.
        for attribute in attributes:
            # Get the argument with the same identifier as the attribute. No argument means the default argument will be
            # used. This will definitely exist, as pre-analysis checks for this.
            argument = Seq(self.arguments).find(lambda a: a.identifier == attribute.name)
            if not argument: continue

            # Extract the argument being passed into the attribute. For named args, ie Point(x=1, y=1), the arguments
            # are the values, and for shorthand: "Point(x, y)", the arguments are the identifiers.
            argument = self.get_argument_value(argument)
            argument_type = argument.infer_type(scope_handler)
            attribute_type = InferredType(convention=ConventionMovAst, type=attribute.type)

            # Compare the types of the argument and the attribute.
            if not attribute_type.symbolic_eq(argument_type, type_symbol.associated_scope, scope_handler.current_scope):
                raise SemanticErrors.TYPE_MISMATCH_2(None, argument, attribute_type, argument_type, scope_handler)

        # Ensure all the "sup" arguments are present in the object initializer.
        sup_types = type_symbol.associated_scope._sup_scopes
        sup_types = Seq(sup_types).filter(lambda s: isinstance(s[0].name, TypeAst))
        expected_sup_types = Seq()

        for sup_scope, sup_block in sup_types:
            # Only stateful superclasses must be present, so check the number of attributes.
            super_class_symbol = sup_scope.associated_type_symbol
            super_class_attribute_count = len(super_class_symbol.type.body.members)

            # If the class is stateful, append the superclass type.
            if super_class_attribute_count > 0:
                expected_sup_types.append(super_class_symbol.fq_type)

        # Check a "sup" argument is given if there are expected superclasses.
        if not self.get_sup_args() and expected_sup_types:
            raise SemanticErrors.MISSING_OBJ_INIT_SUP_ARGUMENT(self)
        elif self.get_sup_args() and not expected_sup_types:
            raise SemanticErrors.UNEXPECTED_OBJ_INIT_SUP_ARGUMENT(self, self.get_sup_args()[0])

        # Todo: mark sup-args as moved.
        if self.get_sup_args():
            # Check the superclass argument is a tuple.
            given_sup_argument = self.get_sup_args()[0]
            given_sup_argument_type = given_sup_argument.value.infer_type(scope_handler, **kwargs)
            if not given_sup_argument_type.type.without_generics().symbolic_eq(CommonTypes.tuple([]), scope_handler.current_scope):
                raise SemanticErrors.OBJ_INIT_SUP_ARGUMENT_NON_TUPLE(self, given_sup_argument, given_sup_argument_type)

            # Get the individual types of the superclass argument.
            given_sup_argument_individual_types = Seq(given_sup_argument_type.type.types[-1].generic_arguments.arguments).map(lambda g: g.type)

            # Check all the expected superclasses are present in the superclass argument.
            for expected_sup_type in expected_sup_types:
                if not Seq(given_sup_argument_individual_types).any(lambda t: expected_sup_type.symbolic_eq(t, scope_handler.current_scope)):
                    raise SemanticErrors.MISSING_OBJ_INIT_SUPER_CLASS(given_sup_argument, expected_sup_type)
                matching_type = Seq(given_sup_argument_individual_types).find(lambda t: expected_sup_type.symbolic_eq(t, scope_handler.current_scope))
                given_sup_argument_individual_types.remove(matching_type)

            # Raise an error for any leftover superclasses that haven't been provided.
            if given_sup_argument_individual_types:
                raise SemanticErrors.UNEXPECTED_OBJ_INIT_SUPER_CLASS(self, given_sup_argument_individual_types[0])

    def get_def_args(self) -> List["ObjectInitializerArgumentNamedAst"]:
        from SPPCompiler.LexicalAnalysis.Tokens import TokenType
        from SPPCompiler.SemanticAnalysis.ASTs import ObjectInitializerArgumentNamedAst, TokenAst

        named_arguments = Seq(self.arguments).filter_to_type(ObjectInitializerArgumentNamedAst)
        token_arguments = named_arguments.filter(lambda a: isinstance(a.identifier, TokenAst))
        default_arguments = token_arguments.filter(lambda a: a.identifier.token.token_type == TokenType.KwElse)
        return default_arguments.list()

    def get_sup_args(self) -> List["ObjectInitializerArgumentAst"]:
        from SPPCompiler.LexicalAnalysis.Tokens import TokenType
        from SPPCompiler.SemanticAnalysis.ASTs import ObjectInitializerArgumentNamedAst, TokenAst

        named_arguments = Seq(self.arguments).filter_to_type(ObjectInitializerArgumentNamedAst)
        token_arguments = named_arguments.filter(lambda a: isinstance(a.identifier, TokenAst))
        default_arguments = token_arguments.filter(lambda a: a.identifier.token.token_type == TokenType.KwSup)
        return default_arguments.list()

    @staticmethod
    def get_argument_value(ast: "ObjectInitializerArgumentAst") -> Ast:
        from SPPCompiler.SemanticAnalysis.ASTs import (
            ObjectInitializerArgumentNormalAst, ObjectInitializerArgumentNamedAst)

        match ast:
            case ObjectInitializerArgumentNormalAst(): return ast.identifier
            case ObjectInitializerArgumentNamedAst(): return ast.value

    def __copy__(self):
        return ObjectInitializerArgumentGroupAst(self.pos, self.paren_l_token, self.arguments.copy(), self.paren_r_token)


__all__ = ["ObjectInitializerArgumentGroupAst"]
