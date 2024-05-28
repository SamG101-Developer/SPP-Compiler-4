from __future__ import annotations
import copy

from src.LexicalAnalysis.Tokens import TokenType

from src.SemanticAnalysis.Utils.Scopes import ScopeHandler
from src.SemanticAnalysis.Utils.SemanticError import SemanticError, SemanticErrorStringFormatType, SemanticErrorType
from src.SemanticAnalysis.Utils.CommonTypes import CommonTypes

from src.Utils.Sequence import Seq


class AstUtils:
    @staticmethod
    def ensure_memory_integrity_of_expression(
            expression: ExpressionAst,
            scope_handler: ScopeHandler,
            keep_consume: bool = False,
            **kwargs) -> None:

        from src.SemanticAnalysis.ASTs.ConventionMovAst import ConventionMovAst
        from src.SemanticAnalysis.ASTs.FunctionArgumentNormalAst import FunctionArgumentNormalAst
        from src.SemanticAnalysis.ASTs.FunctionArgumentGroupAst import FunctionArgumentGroupAst
        from src.SemanticAnalysis.ASTs.IdentifierAst import IdentifierAst
        from src.SemanticAnalysis.ASTs.PostfixExpressionAst import PostfixExpressionAst
        from src.SemanticAnalysis.ASTs.PostfixExpressionOperatorMemberAccessAst import PostfixExpressionOperatorMemberAccessAst
        from src.SemanticAnalysis.ASTs.TokenAst import TokenAst

        # Wrap the expression as a function argument and analyse it, which applies the memory rules to it
        wrapped_value = FunctionArgumentNormalAst(expression.pos, ConventionMovAst(expression.pos), None, expression)
        wrapped_value = FunctionArgumentGroupAst(expression.pos, TokenAst.dummy(TokenType.TkParenL), [wrapped_value], TokenAst.dummy(TokenType.TkParenR))
        wrapped_value.do_semantic_analysis(scope_handler, **kwargs)

        # For identifiers, the symbol will be "moved" by the memory check above, so mark as non-consumed. An exception is
        # the "keep_consume" flag, which will keep the symbol "consumed"
        if not keep_consume:
            match expression:
                case IdentifierAst():
                    scope_handler.current_scope.get_symbol(expression).memory_info.ast_consumed = None

                case PostfixExpressionAst() if isinstance(expression.op, PostfixExpressionOperatorMemberAccessAst):
                    # TODO: potentially something with partial moves needs to be resolved here
                    scope_handler.current_scope.get_symbol(expression.lhs).memory_info.ast_consumed = None

    @staticmethod
    def verify_generic_arguments(
            generic_parameters: Seq[GenericParameterAst],
            inferred_generic_arguments: Seq[GenericArgumentAst],
            generic_arguments: Seq[GenericArgumentAst],
            obj_definition: IdentifierAst,
            usage: Ast,
            scope_handler: ScopeHandler,
            **kwargs) -> Seq[GenericArgumentAst]:

        from src.SemanticAnalysis.ASTs.ClassPrototypeAst import ClassPrototypeAst
        from src.SemanticAnalysis.ASTs.FunctionPrototypeAst import FunctionPrototypeAst
        from src.SemanticAnalysis.ASTs.GenericArgumentNamedAst import GenericArgumentNamedAst
        from src.SemanticAnalysis.ASTs.GenericArgumentNormalAst import GenericArgumentNormalAst
        from src.SemanticAnalysis.ASTs.GenericParameterRequiredAst import GenericParameterRequiredAst
        from src.SemanticAnalysis.ASTs.GenericParameterOptionalAst import GenericParameterOptionalAst
        from src.SemanticAnalysis.ASTs.GenericParameterVariadicAst import GenericParameterVariadicAst
        from src.SemanticAnalysis.ASTs.IdentifierAst import IdentifierAst
        from src.SemanticAnalysis.ASTs.TokenAst import TokenAst
        from src.SemanticAnalysis.ASTs.TypeSingleAst import TypeSingleAst

        match obj_definition:
            case FunctionPrototypeAst(): what = "Function overload"
            case ClassPrototypeAst(): what = "Class"
            case TypeSingleAst(): what = "Type"
            case _: what = "Unknown"

        # TODO : change "function overload" in error messages to be based on obj_definition etc

        available_generic_parameter_names = generic_parameters.map(lambda p: p.identifier)
        required_generic_parameters = generic_parameters.filter_to_type(GenericParameterRequiredAst)

        # Check order of generic parameters is explicit-required -> inferrable-required -> ...
        # todo

        # Check that inferred generics haven't been given explicitly:
        if inferred_generic_arguments.length + generic_arguments.length > generic_parameters.length and (not generic_parameters or not isinstance(generic_parameters[-1], GenericParameterVariadicAst)):
            # todo: the use of [0] will probably get incorrect generic identifiers in some cases
            if generic_parameters:
                exception = SemanticError()
                exception.add_info(
                    pos=inferred_generic_arguments[0].pos,
                    tag_message=f"Generic parameter '{inferred_generic_arguments[0].identifier}' inferred here")
                exception.add_error(
                    pos=generic_arguments[0].pos,
                    error_type=SemanticErrorType.NAME_ERROR,
                    message=f"Do not provide inferrable generic parameters with explicit generic arguments",
                    tag_message=f"Generic argument for generic parameter '{generic_parameters[0]}' provided explicitly here",
                    tip=f"Remove explicit generic arguments for inferrable generic parameters")

            else:
                exception = SemanticError()
                exception.add_info(
                    pos=obj_definition.identifier.pos,
                    tag_message=f"No generic parameters declared here.")
                exception.add_error(
                    pos=generic_arguments[0].pos,
                    error_type=SemanticErrorType.NAME_ERROR,
                    message=f"Too many generic arguments given",
                    tag_message=f"Generic argument '{generic_arguments[0]}' given here",
                    tip=f"Remove generic arguments")
            raise exception

        # Check if there are any named generic arguments with names that don't match any generic parameter names:
        if invalid_generic_argument_names := generic_arguments.filter_to_type(GenericArgumentNamedAst).map(lambda a: a.identifier).filter(lambda arg_name: arg_name not in available_generic_parameter_names):
            # exception = SemanticError(f"Invalid generic argument names given:")
            # exception.add_error(obj_definition.pos, f"{what} declared here with generic parameters: {available_generic_parameter_names.map(str).join(", ")}")
            # exception.add_error(invalid_generic_argument_names[0].pos, f"Generic argument <{invalid_generic_argument_names[0]}> found here.", SemanticErrorStringFormatType.MINIMAL)

            exception = SemanticError()
            exception.add_info(
                pos=obj_definition.generic_parameters.pos,
                tag_message=f"Generic parameters declared here")
            exception.add_error(
                pos=invalid_generic_argument_names[0].pos,
                error_type=SemanticErrorType.NAME_ERROR,
                message=f"Invalid generic argument names given to {what}: {invalid_generic_argument_names.map(str).join(", ")}",
                tag_message=f"Invalid generic argument '{invalid_generic_argument_names[0]}' found here",
                tip=f"Valid generic argument names are: {available_generic_parameter_names.map(str).join(', ')}")
            raise exception

        # Remove all named generic arguments from the available generic parameter names list:
        for generic_argument in generic_arguments.filter(lambda a: isinstance(a, GenericArgumentNamedAst)) + inferred_generic_arguments:
            available_generic_parameter_names.remove(generic_argument.identifier)

        # Check there aren't too many generic arguments provided for this overload:
        if generic_arguments.length > generic_parameters.length and not isinstance(generic_parameters[-1], GenericParameterVariadicAst):
            exception = SemanticError(f"Too many generic arguments given:")
            exception.add_error(obj_definition.pos, f"{what} declared here with generic parameters: {generic_parameters.map(lambda p: p.identifier).map(str).join(", ")}")
            exception.add_error(generic_arguments[available_generic_parameter_names.length].pos, f"{generic_arguments.length - generic_parameters.length} extra generic arguments found here.", SemanticErrorStringFormatType.MINIMAL)
            raise exception

        # Convert each normal generic argument to a named generic argument with the next available generic parameter name:
        variadic_generic_argument = None
        for generic_argument in generic_arguments.filter_to_type(GenericArgumentNormalAst):
            # Check if there are no more generic parameter names (in which case append to variadic)
            if not available_generic_parameter_names:
                variadic_generic_argument.type.parts[-1].generic_arguments.arguments.append(generic_argument)
                # variadic_generic_argument.type.do_semantic_analysis(scope_handler, **kwargs)
                generic_arguments.remove(generic_argument)
                continue

            # Check if available generic parameter names' current item is variadic
            variadic = isinstance(generic_parameters.find(lambda p: p.identifier == available_generic_parameter_names[0]), GenericParameterVariadicAst)

            # If if generic parameter is not variadic, the argument is just the argument. Otherwise, (for a variadic),
            # package the argument in a tuple, so following arguments for the variadic parameter can be bound to the tuple
            # too.
            if not variadic:
                generic_argument_type = generic_argument.type
            else:
                generic_argument_type = CommonTypes.tuple([generic_argument.type])
                generic_argument_type.do_semantic_analysis(scope_handler, **kwargs)

            # Create the new generic argument, naming it with the first available name, and replace the old generic argument
            # with the new one. If this is the first argument for a variadic parameter, mark it as such.
            new_generic_argument_identifier = available_generic_parameter_names.pop(0)
            new_generic_argument = GenericArgumentNamedAst(
                pos=generic_argument.pos,
                raw_identifier=IdentifierAst(new_generic_argument_identifier.pos, new_generic_argument_identifier.parts[-1].value),
                assignment_token=TokenAst.dummy(TokenType.TkAssign),
                type=generic_argument_type)

            generic_arguments.replace(generic_argument, new_generic_argument, limit=1)
            if variadic:
                variadic_generic_argument = new_generic_argument

        # Check that all required generic parameters have been given an argument:
        if unfilled_required_generic_parameters := required_generic_parameters.map(lambda p: p.identifier).contains_any(available_generic_parameter_names):
            exception = SemanticError()
            exception.add_info(
                pos=obj_definition.generic_parameters.pos,
                tag_message=f"Generic parameters declared here")
            exception.add_error(
                pos=usage.pos,
                error_type=SemanticErrorType.NAME_ERROR,
                message=f"Missing generic arguments",
                tag_message=f"Missing generic arguments: {unfilled_required_generic_parameters.map(str).join(', ')}",
                tip=f"Make sure all required generic arguments are provided")
            raise exception

        # Combine the inferred generic arguments with the rest of the generic arguments.
        all_generic_arguments = inferred_generic_arguments + generic_arguments

        # Load the missing optional generic parameters with their default values.
        for optional_generic_parameter in generic_parameters.filter_to_type(GenericParameterOptionalAst):
            if not all_generic_arguments.find(lambda a: a.identifier == optional_generic_parameter.identifier):
                default_type = copy.deepcopy(optional_generic_parameter.default_value)

                # Fully qualify the default type with its namespace.
                type_scope_namespace = scope_handler.current_scope.get_symbol(usage.without_generics()).associated_scope.scopes_as_namespace
                default_type.parts[:-1] = type_scope_namespace

                all_generic_arguments.append(GenericArgumentNamedAst(
                    pos=optional_generic_parameter.pos,
                    raw_identifier=IdentifierAst(optional_generic_parameter.identifier.pos, optional_generic_parameter.identifier.parts[-1].value),
                    assignment_token=TokenAst.dummy(TokenType.TkAssign),
                    type=default_type))

        return all_generic_arguments

    @staticmethod
    def infer_generic_argument_values(
            scope_handler: ScopeHandler,
            generic_parameters: Seq[GenericParameterAst],
            infer_from: Seq[TypeAst],
            replace_with: Seq[TypeAst],
            obj_definition: IdentifierAst) -> Seq[GenericArgumentNamedAst]:

        from src.SemanticAnalysis.ASTs.GenericArgumentNamedAst import GenericArgumentNamedAst
        from src.SemanticAnalysis.ASTs.IdentifierAst import IdentifierAst
        from src.SemanticAnalysis.ASTs.TokenAst import TokenAst
        print("#" * 100)
        print(generic_parameters, infer_from, replace_with, obj_definition)

        # Infer any generic type arguments that can be inferred (from parameter types etc)
        # Return a list of non-inferred generic argument types.
        inferred_generic_parameters = Seq([])
        for generic_parameter in generic_parameters:
            for parameter_t, argument_t in infer_from.zip(replace_with):
                print(parameter_t, argument_t)
                # Check if the parameter type or any of its nested generic argument names matches the generic parameter.
                parameter_type_parts = Seq(list(iter(parameter_t)))
                if generic_parameter.identifier not in parameter_type_parts:
                    continue

                # Check if the generic argument has already been inferred with a different type.
                duplicate_inferred_parameter = inferred_generic_parameters.find(lambda p: p.identifier == generic_parameter.identifier)
                if duplicate_inferred_parameter and not duplicate_inferred_parameter.type.symbolic_eq(argument_t, scope_handler.current_scope):
                    exception = SemanticError()
                    exception.add_info(
                        pos=duplicate_inferred_parameter.pos,
                        tag_message=f"Generic parameter '{generic_parameter.identifier}' inferred here as '{duplicate_inferred_parameter.type}'")
                    exception.add_error(
                        pos=argument_t.pos,
                        error_type=SemanticErrorType.TYPE_ERROR,
                        message=f"Generic parameter given conflicting types",
                        tag_message=f"Generic argument '{generic_parameter.identifier}' inferred here as '{argument_t}'",
                        tip=f"Make sure all instances of '{generic_parameter.identifier}' are inferred as the same type")
                    raise exception

                argument_t_namespace = Seq(list(iter(argument_t))).filter(lambda p: isinstance(p, IdentifierAst)).value
                if not duplicate_inferred_parameter:
                    for p_1, p_2 in zip(iter(parameter_t), iter(argument_t)):
                        if generic_parameter.identifier == p_1:
                            inferred_generic_parameters.append(GenericArgumentNamedAst(
                                pos=p_2.pos,
                                raw_identifier=IdentifierAst(generic_parameter.identifier.pos, generic_parameter.identifier.parts[-1].value),
                                assignment_token=TokenAst.dummy(TokenType.TkAssign),
                                type=p_2))
                            break

        return inferred_generic_parameters


__all__ = ["AstUtils"]
