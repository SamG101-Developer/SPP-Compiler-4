from dataclasses import dataclass
from typing import List

from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstMixins import SemanticAnalyser
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstUtils import InferredType
from SPPCompiler.SemanticAnalysis.ASTs.Meta.Ast import Ast
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstPrinter import *
from SPPCompiler.SemanticAnalysis.Utils.Scopes import ScopeHandler
from SPPCompiler.SemanticAnalysis.Utils.SemanticError import SemanticErrors
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

        # Analyse the arguments of the object initializer.
        Seq(self.arguments).for_each(lambda a: a.do_semantic_analysis(scope_handler, **kwargs))

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

        # Check there is a maximum of one default argument.
        if len(def_arguments) > 1:
            raise SemanticErrors.DUPLICATE_ITEM(def_arguments, "default object initializer argument")

        # Check there is a maximum of one sup argument.
        if len(sup_arguments) > 1:
            raise SemanticErrors.DUPLICATE_ITEM(sup_arguments, "sup object initializer argument")

        # Todo: check the sup-argument is a tuple of all stateful, non-default super-classes in order
        if True:
            ...

        def_argument = def_arguments[0] if def_arguments else None

        # Mark the default value as moved.#
        if def_argument:
            match def_argument.value:
                case IdentifierAst(): scope_handler.current_scope.get_symbol(def_argument.value).memory_info.ast_consumed = def_argument
                case PostfixExpressionAst(): scope_handler.current_scope.get_outermost_variable_symbol(def_argument.value).memory_info.ast_partial_moves.append(def_argument.value)

        # Check all the arguments are attributes of the class.
        if invalid_arguments := argument_identifiers.set_subtract(attribute_identifiers):
            raise SemanticErrors.UNKNOWN_IDENTIFIER(invalid_arguments[0], attribute_identifiers.map(lambda i: i.value).value, "attribute")

        # Check all the attributes have been assigned a value.
        if not def_argument and (missing_arguments := attribute_identifiers.set_subtract(argument_identifiers)):
            raise SemanticErrors.MISSING_ARGUMENT(self, missing_arguments[0], "object initializer", "attribute")

    def do_semantic_analysis(self, scope_handler: ScopeHandler, **kwargs) -> None:
        from SPPCompiler.SemanticAnalysis.ASTs import ConventionMovAst, ObjectInitializerArgumentNamedAst

        class_type = kwargs["class-type"]
        type_symbol = scope_handler.current_scope.get_symbol(class_type)
        attributes = type_symbol.type.body.members

        # Check each argument is the correct type for the attribute.
        for attribute in attributes:
            # Get the argument with the same identifier as the attribute. No argument means the default argument will be
            # used. This will definitely exist, as pre-analysis checks for this.
            argument = Seq(self.arguments).find(lambda a: a.identifier == attribute.identifier)
            if not argument: continue

            # Extract the argument being passed into the attribute. For named args, ie Point(x=1, y=1), the arguments
            # are the values, and for shorthand: "Point(x, y)", the arguments are the identifiers.
            argument = argument.value if isinstance(argument, ObjectInitializerArgumentNamedAst) else argument.identifier
            argument_type = argument.infer_type(scope_handler)
            attribute_type = InferredType(convention=ConventionMovAst, type=attribute.type_declaration)

            # Compare the types of the argument and the attribute.
            if not argument_type.symbolic_eq(attribute_type, scope_handler.current_scope, type_symbol.associated_scope):
                raise SemanticErrors.TYPE_MISMATCH(argument, attribute_type, argument_type)

        # Todo: super-class checks

    def get_def_args(self) -> List["ObjectInitializerArgumentNamedAst"]:
        from SPPCompiler.LexicalAnalysis.Tokens import TokenType
        from SPPCompiler.SemanticAnalysis.ASTs import ObjectInitializerArgumentNamedAst, TokenAst

        named_arguments = Seq(self.arguments).filter_to_type(ObjectInitializerArgumentNamedAst)
        token_arguments = named_arguments.filter(lambda a: isinstance(a.identifier, TokenAst))
        default_arguments = token_arguments.filter(lambda a: a.identifier.token.token_type == TokenType.KwElse)
        return default_arguments.value

    def get_sup_args(self) -> List["ObjectInitializerArgumentAst"]:
        from SPPCompiler.LexicalAnalysis.Tokens import TokenType
        from SPPCompiler.SemanticAnalysis.ASTs import ObjectInitializerArgumentNamedAst, TokenAst

        named_arguments = Seq(self.arguments).filter_to_type(ObjectInitializerArgumentNamedAst)
        token_arguments = named_arguments.filter(lambda a: isinstance(a.identifier, TokenAst))
        default_arguments = token_arguments.filter(lambda a: a.identifier.token.token_type == TokenType.KwSup)
        return default_arguments.value


__all__ = ["ObjectInitializerArgumentGroupAst"]
