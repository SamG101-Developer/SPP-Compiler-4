import copy
from dataclasses import dataclass
from typing import Optional, Tuple, Type

from src.SemanticAnalysis.ASTMixins.SemanticAnalyser import SemanticAnalyser
from src.SemanticAnalysis.Utils.SemanticError import SemanticError
from src.SemanticAnalysis.Utils.Scopes import Scope, ScopeHandler
from src.SemanticAnalysis.Utils.Symbols import TypeSymbol
from src.SemanticAnalysis.ASTMixins.TypeInfer import TypeInfer

from src.LexicalAnalysis.Tokens import TokenType

from src.SemanticAnalysis.ASTs.Meta.Ast import Ast
from src.SemanticAnalysis.ASTs.Meta.AstPrinter import *
from src.SemanticAnalysis.ASTs.Meta.AstUtils import AstUtils

from src.SemanticAnalysis.ASTs.ConventionMovAst import ConventionMovAst
from src.SemanticAnalysis.ASTs.FunctionArgumentNamedAst import FunctionArgumentNamedAst
from src.SemanticAnalysis.ASTs.FunctionArgumentNormalAst import FunctionArgumentNormalAst
from src.SemanticAnalysis.ASTs.GenericArgumentGroupAst import GenericArgumentGroupAst
from src.SemanticAnalysis.ASTs.GenericIdentifierAst import GenericIdentifierAst
from src.SemanticAnalysis.ASTs.IdentifierAst import IdentifierAst
from src.SemanticAnalysis.ASTs.PostfixExpressionAst import PostfixExpressionAst
from src.SemanticAnalysis.ASTs.TokenAst import TokenAst
from src.SemanticAnalysis.ASTs.TypeSingleAst import TypeSingleAst

from src.Utils.Sequence import Seq


@dataclass
class PostfixExpressionOperatorFunctionCallAst(Ast, SemanticAnalyser, TypeInfer):
    """
    The PostfixExpressionOperatorFunctionCallAst node represents a function call operation, with given generic arguments
    and function arguments. This node also resolves overloads at compile time. Folding can be applied to a function for
    tuple operations.

    Attributes:
        - generic_arguments: The generic arguments of the function call.
        - arguments: The arguments of the function call.
        - fold_token: The optional fold token of the function call.
    """

    generic_arguments: Optional["GenericArgumentGroupAst"]
    arguments: "FunctionArgumentGroupAst"
    fold_token: Optional["TokenAst"]

    def __post_init__(self):
        self.generic_arguments = self.generic_arguments or GenericArgumentGroupAst.default()

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        # Print the PostfixExpressionOperatorFunctionCallAst.
        s = ""
        s += f"{self.generic_arguments.print(printer)}" if self.generic_arguments else ""
        s += f"{self.arguments.print(printer)}"
        s += f"{self.fold_token.print(printer)}" if self.fold_token else ""
        return s

    def __get_matching_overload(self, scope_handler: ScopeHandler, function_name: "ExpressionAst", **kwargs) -> tuple["FunctionPrototypeAst", Scope, Optional["FunctionArgumentNamedAst"]]:
        match function_name:
            case PostfixExpressionAst():
                function_name_lhs_part_scope = scope_handler.current_scope.get_symbol(function_name.lhs.infer_type(scope_handler, **kwargs)[1]).associated_scope
                function_name_rhs_part_scope = function_name_lhs_part_scope.get_symbol(TypeSingleAst(pos=-1, parts=[GenericIdentifierAst(pos=-1, value=f"MOCK_{function_name.op.identifier}", generic_arguments=None)])).associated_scope
            case IdentifierAst():
                function_name_rhs_part_scope = scope_handler.current_scope.get_symbol(function_name.infer_type(scope_handler, **kwargs)[1]).associated_scope
            case _:
                exception = SemanticError(f"Invalid function call:")
                exception.add_traceback(function_name.pos, f"Function call '{function_name}' found here. Can only call identifiers.")
                raise exception

        # The only classes possibly superimposed over a MOCK_ class are the Fun classes.
        mock_function_object_sup_scopes = function_name_rhs_part_scope.sup_scopes
        function_overloads = Seq(mock_function_object_sup_scopes).map(lambda s: s[1].body.members).flat()  # TODO: limit to functions only, not typedefs etc too.
        function_overload_errors = []
        valid_overloads = []

        # Check the argument values are valid (names only) for type inference. The rest of argument checks are after.
        Seq(self.arguments.arguments).map(lambda a: a.value.do_semantic_analysis(scope_handler, **kwargs))

        # Convert each normal argument into a named argument that maps to the overload's parameter names.
        for i, function_overload in function_overloads.enumerate():
            # print(i, function_overload)
            try:
                inferred_generic_arguments = AstUtils.infer_generic_argument_values(
                    scope_handler=scope_handler,
                    generic_parameters=Seq(function_overload.generic_parameters.parameters),
                    infer_from=Seq(function_overload.parameters.parameters).map(lambda p: p.type_declaration),
                    replace_with=Seq(self.arguments.arguments).map(lambda a: a.value.infer_type(scope_handler, **kwargs)[1]),
                    obj_definition=function_overload)

                all_generic_arguments = AstUtils.verify_generic_arguments(
                    generic_parameters=Seq(function_overload.generic_parameters.parameters),
                    inferred_generic_arguments=inferred_generic_arguments,
                    generic_arguments=Seq(self.generic_arguments.arguments),
                    obj_definition=function_overload,
                    usage=self,
                    scope_handler=scope_handler,
                    **kwargs)

            except SemanticError as e:
                function_overload_errors.append(e)
                continue

            function_overload_scope = mock_function_object_sup_scopes[i][0]._children_scopes[0]
            available_parameter_names = Seq(function_overload.parameters.parameters).map(lambda p: p.identifier)
            arguments = Seq(copy.deepcopy(self.arguments.arguments))

            # If the function is an instance method (the first parameter is a "self" parameter), then the lhs as the
            # "self" argument, as this is the instance the method is being applied over.
            if self_param := function_overload.parameters.get_self():
                arguments.append(FunctionArgumentNamedAst(
                    pos=function_name.lhs.pos,
                    identifier=IdentifierAst(-1, "self"),
                    assignment_token=TokenAst.dummy(TokenType.TkAssign),
                    convention=self_param.convention,
                    value=function_name.lhs))

            # Check if there are any named arguments with names that don't match any parameter names.
            if invalid_argument_names := Seq(self.arguments.arguments).filter(lambda a: isinstance(a, FunctionArgumentNamedAst)).map(lambda a: a.identifier).filter(lambda arg_name: arg_name not in available_parameter_names):
                exception = SemanticError(f"Invalid argument names given to function call:")
                exception.add_traceback(function_overload.pos, f"Function overload declared here with parameters: {available_parameter_names.map(str).join(", ")}")
                exception.add_traceback_minimal(invalid_argument_names[0].pos, f"Argument <{invalid_argument_names[0]}> found here.")
                function_overload_errors.append(exception)
                continue

            # Remove all named arguments from the available parameter names list.
            for argument in arguments.filter(lambda a: isinstance(a, FunctionArgumentNamedAst)):
                available_parameter_names.remove(argument.identifier)

            # Check there aren't too many arguments provided for this overload
            if arguments.length > len(function_overload.parameters.parameters):
                exception = SemanticError(f"Too many arguments given to function call:")
                exception.add_traceback(function_overload.pos, f"Function overload declared here with parameters: {Seq(function_overload.parameters.parameters).map(lambda p: p.identifier).map(str).join(", ")}")
                exception.add_traceback_minimal(arguments[available_parameter_names.length].pos, f"{arguments.length - len(function_overload.parameters.parameters)} extra arguments found here.")
                function_overload_errors.append(exception)
                continue

            # Convert each normal argument to a named argument with the next available parameter name.
            for argument in arguments.filter(lambda a: isinstance(a, FunctionArgumentNormalAst)):
                new_argument = FunctionArgumentNamedAst(argument.pos, available_parameter_names.pop(0), TokenAst.dummy(TokenType.TkAssign), argument.convention, argument.value)
                arguments.replace(argument, new_argument)

            # Check that all required parameters have been given an argument.
            if unfilled_required_parameters := Seq(function_overload.parameters.get_req()).map(lambda p: p.identifier).contains_any(available_parameter_names):
                exception = SemanticError(f"Missing arguments in function call:")
                exception.add_traceback(function_overload.pos, f"Function overload declared here with required parameters: {available_parameter_names.map(str).join(", ")}")
                exception.add_traceback_minimal(self.pos, f"Missing arguments: {unfilled_required_parameters.map(str).join(", ")}")
                function_overload_errors.append(exception)
                continue

            # If there are generic arguments, then create a new function overload with the generic arguments filled in.
            # Add this to the scopes with the FunctionPrototypeAST's methods.
            new_scope = False
            if all_generic_arguments:
                # Copy the current overload, because a new one will be created with the generic parameters being
                # substituted with their corresponding generic arguments in the parameter and return types.
                non_generic_function_overload = copy.deepcopy(function_overload)
                non_generic_function_overload.generic_parameters.parameters = []
                for generic_argument in all_generic_arguments:
                    Seq(non_generic_function_overload.parameters.parameters).map(lambda p: p.type_declaration).for_each(lambda t: t.substitute_generics(generic_argument.identifier, generic_argument.type))
                    non_generic_function_overload.return_type.substitute_generics(generic_argument.identifier, generic_argument.type)

                # If this function doesn't have the newly created specialization already stored as a scope, then create
                # a new function scope for it. It will be stored alongside the original function scope in a sup block.
                # This is ONLY done to reduce the copies being made if a specialization is required > 1 time.
                if non_generic_function_overload not in function_overload._specializations:

                    # Save the substituted overload (specialization) into the original function's specializations list.
                    # This is to ensure that the same specialization isn't created twice.
                    function_overload._specializations.append(non_generic_function_overload)
                    function_overload._ctx.body.members.append(non_generic_function_overload)
                    non_generic_function_overload.pre_process(function_overload._ctx)

                    # Save the current scope in the scope handler. Set the current scope to the parent scope of the
                    # current (non-substituted) function overload, and generate the substituted function overload. This
                    # will create the scope in the correct place. Get the newly created scope (end of child scope list)
                    restore_scope = scope_handler.current_scope
                    scope_handler.current_scope = function_overload_scope._parent_scope
                    non_generic_function_overload.generate(scope_handler)
                    non_generic_function_overload_scope = mock_function_object_sup_scopes[i][0]._children_scopes[-1]

                    # Set the current scope to the substituted function overload's scope, and analyse the substituted
                    # function overload. This will re-create the inner symbols of the correct types.
                    scope_handler.current_scope = non_generic_function_overload_scope
                    non_generic_function_overload.do_semantic_analysis(scope_handler, override_scope=True)

                    # Restore the current scope of the scope handler
                    scope_handler.current_scope = restore_scope

                    # Next, create type symbols mapping the generic parameters to their generic arguments, in the scope
                    # of the substituted functions. This is because the generic parameters might still be used inside
                    # the function as a type (let x: T), so they need to map to their correct type.
                    for generic_argument in all_generic_arguments:
                        type_sym = scope_handler.current_scope.get_symbol(generic_argument.type)
                        non_generic_function_overload_scope.add_symbol(TypeSymbol(generic_argument.identifier, type_sym.type))
                        non_generic_function_overload_scope.get_symbol(generic_argument.identifier).associated_scope = type_sym.associated_scope

                    # Mark that a new scope has been created, and provide a mechanism to remove it if the type checking
                    # fails and this substituted overload is no longer needed.
                    new_scope = True

                    def remove_scope():
                        function_overload_scope._parent_scope._children_scopes.remove(non_generic_function_overload_scope)

                    # Overwrite the function overload & its scope being considered with the substituted function
                    # overload.
                    function_overload = non_generic_function_overload
                    function_overload_scope = non_generic_function_overload_scope

                else:
                    # Otherwise, the specialization exists. Find it in the function overloads list by using a structural
                    # comparison.
                    non_generic_function_overload = Seq(function_overload._specializations).find(lambda s: s == non_generic_function_overload)

                    # Next, the scope of this specialization is required. Because there will be multiple "call_ref" etc
                    # scopes inside the "sup" scope, the only way to get the correct scope, is to check that the generic
                    # types being stored in the scope match the generic arguments being considered. Given that the
                    # specialization exists, it stands that there must be 1 scope with matching generic type symbols.
                    non_generic_function_overload_scope = None
                    for scope in mock_function_object_sup_scopes[i][0]._children_scopes:
                        type_symbols = Seq(scope.all_symbols(True)).filter_to_type(TypeSymbol)
                        if all([type_symbol.name in all_generic_arguments.map(lambda a: a.identifier) and type_symbol.type == scope_handler.current_scope.get_symbol(all_generic_arguments.find(lambda a: a.identifier == type_symbol.name).type).type for type_symbol in type_symbols]):
                            non_generic_function_overload_scope = scope
                            break

                    # Overwrite the function overload & its scope being considered with the substituted function
                    # overload.
                    function_overload = non_generic_function_overload
                    function_overload_scope = non_generic_function_overload_scope

            # Type check between each argument and its corresponding parameter.
            type_error = False
            for argument in arguments:
                corresponding_parameter = Seq(function_overload.parameters.parameters).find(lambda p: p.identifier == argument.identifier)
                argument_type = argument.infer_type(scope_handler, **kwargs)

                # Special case for "self" => use the param.convention, not the inferred type convention.
                if argument.identifier.value == "self":
                    argument_type = (type(corresponding_parameter.convention), argument_type[1])

                if not argument_type[1].symbolic_eq(corresponding_parameter.type_declaration, function_overload_scope) or argument_type[0] != type(corresponding_parameter.convention):
                    exception = SemanticError(f"Invalid argument type given to function call:")
                    exception.add_traceback(function_overload.pos, f"Function overload declared here with parameters: {Seq(function_overload.parameters.parameters).map(lambda p: p.identifier).map(str).join(", ")}")
                    exception.add_traceback_minimal(argument.pos, f"Argument <{argument}> found here with type '{argument_type[0]}{argument_type[1]}', instead of '{corresponding_parameter.convention}{corresponding_parameter.type_declaration}'.")
                    function_overload_errors.append(exception)
                    type_error = True
                    break

            # No argument type errors => this is a valid overload.
            if not type_error:
                valid_overloads.append((function_overload, function_overload_scope, arguments.find(lambda a: a.identifier.value == "self")))
            elif new_scope:
                # Remove the newly created overload for generics.
                remove_scope()

        # If there were no valid overloads, display each overload and why it couldn't be selected. Raise the error here
        # so no valid overload is attempted to be pulled from an empty list.
        if not valid_overloads:
            error = SemanticError("Invalid function call")
            error.add_traceback(self.pos, f"Function call {self} found here.")
            error.next_exceptions = function_overload_errors
            raise error

        # TODO: Select the most precise match: this is the overload with the least amount of parameters that have generic types.
        # TODO: This can lead to ambiguities: func(a: Str, b: Vec[T) and func(a: T, b: Arr[I8]) for func("hello", [1, 2, 3])
        return valid_overloads[0]

    def do_semantic_analysis(self, scope_handler: ScopeHandler, lhs: "ExpressionAst" = None, **kwargs) -> None:
        # Check that a matching overload exists for the function call. Also get the "self" argument (for analysis)
        _, _, self_arg = self.__get_matching_overload(scope_handler, lhs, **kwargs)
        Seq(self.generic_arguments.arguments).for_each(lambda x: x.type.do_semantic_analysis(scope_handler, **kwargs))

        # Analyse the arguments (including the "self" argument, to check for conflicting borrows)
        if self_arg:
            self.arguments.arguments.append(self_arg)
            self.arguments.do_semantic_analysis(scope_handler, **kwargs)
            self.arguments.arguments.remove(self_arg)
        else:
            self.arguments.do_semantic_analysis(scope_handler, **kwargs)

    def infer_type(self, scope_handler: ScopeHandler, lhs: "ExpressionAst" = None, **kwargs) -> Tuple[Type["ConventionAst"], "TypeAst"]:
        # Get the matching overload and return its return-type. 2nd class borrows mean the object returned is always
        # owned, ie ConventionMovAst.
        function_proto, function_scope, _ = self.__get_matching_overload(scope_handler, lhs, **kwargs)
        function_return_type = copy.deepcopy(function_proto.return_type)
        function_return_type = function_scope.get_symbol(function_return_type).fq_type
        return ConventionMovAst, function_return_type


__all__ = ["PostfixExpressionOperatorFunctionCallAst"]
