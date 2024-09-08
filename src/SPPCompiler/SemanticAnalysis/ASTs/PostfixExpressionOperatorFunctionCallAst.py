import copy
from dataclasses import dataclass, field
from typing import Optional, Tuple

from SPPCompiler.LexicalAnalysis.Tokens import TokenType
from SPPCompiler.SemanticAnalysis.ASTs.Meta.Ast import Ast
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstMixins import SemanticAnalyser
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstPrinter import *
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstUtils import TypeInfer, InferredType, infer_generics_types, convert_function_arguments_to_named, convert_generic_arguments_to_named, get_all_function_scopes
from SPPCompiler.SemanticAnalysis.Utils.CommonTypes import CommonTypes
from SPPCompiler.SemanticAnalysis.Utils.Scopes import Scope, ScopeHandler
from SPPCompiler.SemanticAnalysis.Utils.SemanticError import SemanticError, SemanticErrors
from SPPCompiler.Utils.Sequence import Seq


@dataclass
class PostfixExpressionOperatorFunctionCallAst(Ast, SemanticAnalyser, TypeInfer):
    """
    The PostfixExpressionOperatorFunctionCallAst node represents a function call operation, with given generic arguments
    and function arguments. This node also resolves overloads at compile time. Folding can be applied to a function for
    tuple operations.

    Attributes:
        generic_arguments: The generic arguments of the function call.
        arguments: The arguments of the function call.
        fold_token: The optional fold token of the function call.
    """

    generic_arguments: Optional["GenericArgumentGroupAst"]
    arguments: "FunctionArgumentGroupAst"
    fold_token: Optional["TokenAst"]

    _overload: Optional[Tuple["FunctionPrototypeAst", "FunctionArgumentGroupAst", Scope]] = field(default=None, init=False)
    _is_async: Optional[Ast] = field(default=None, init=False)

    def __post_init__(self):
        from SPPCompiler.SemanticAnalysis.ASTs import GenericArgumentGroupAst
        self.generic_arguments = self.generic_arguments or GenericArgumentGroupAst.default()

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        # Print the PostfixExpressionOperatorFunctionCallAst.
        s = ""
        s += f"{self.generic_arguments.print(printer)}" if self.generic_arguments else ""
        s += f"{self.arguments.print(printer)}"
        s += f"{self.fold_token.print(printer)}" if self.fold_token else ""
        return s

    def _get_matching_overload(self, scope_handler: ScopeHandler, lhs: "ExpressionAst", **kwargs) -> Tuple["FunctionPrototypeAst", "FunctionArgumentGroupAst", Scope]:
        from SPPCompiler.SemanticAnalysis.ASTs import PostfixExpressionAst, PostfixExpressionOperatorMemberAccessAst
        from SPPCompiler.SemanticAnalysis.ASTs import FunctionArgumentNormalAst, FunctionArgumentNamedAst
        from SPPCompiler.SemanticAnalysis.ASTs import FunctionParameterVariadicAst, FunctionParameterSelfAst
        from SPPCompiler.SemanticAnalysis.ASTs import TokenAst, ConventionMovAst, IdentifierAst

        if self._overload:
            return self._overload

        # Get the LHS-type of the function call. For postfix identifiers, this is the rightmost identifier left of the
        # function name.
        re_analyse = False
        type_scope = None
        match lhs:
            case PostfixExpressionAst() if lhs.op.dot_token.token.token_type == TokenType.TkDot:
                lhs_type = lhs.lhs.infer_type(scope_handler, **kwargs).type
                lhs_identifier = lhs.op.identifier
                re_analyse = True

            case PostfixExpressionAst() if lhs.op.dot_token.token.token_type == TokenType.TkDblColon:
                lhs_type = lhs.lhs
                lhs_identifier = lhs.op.identifier
                re_analyse = False
                type_scope = scope_handler.current_scope.get_symbol(lhs_type).associated_scope

            case IdentifierAst():
                lhs_type = scope_handler.current_scope.parent_module.name
                lhs_identifier = lhs
                re_analyse = True

            case _:
                raise SemanticErrors.UNCALLABLE_TYPE(lhs)

        # Create a new AST, using the type to form the function. For example, "vec.test(other)" becomes
        # "Vec::test(vec, other)". This provides uniform analysis for all function calls.
        if re_analyse:

            # Insert the "self" argument into the argument list if the function is a method.
            arguments = copy.copy(self.arguments)
            if isinstance(lhs, PostfixExpressionAst) and lhs.op.dot_token.token.token_type == TokenType.TkDot:
                ast_2 = FunctionArgumentNormalAst(lhs.pos, ConventionMovAst(lhs.pos), None, lhs.lhs)
                arguments.arguments.insert(0, ast_2)

            # Create the new AST, and perform semantic analysis on it.
            ast_0 = PostfixExpressionOperatorMemberAccessAst(self.pos, TokenAst.dummy(TokenType.TkDblColon), lhs_identifier)
            ast_1 = PostfixExpressionAst(self.pos, lhs_type, ast_0)
            ast_2 = PostfixExpressionOperatorFunctionCallAst(self.pos, copy.copy(self.generic_arguments), arguments, self.fold_token)
            self._overload = ast_2._get_matching_overload(scope_handler, ast_1, **kwargs)
            return ast_2._overload

        # Get the function scopes from the LHS-type. These will be the "MOCK_..." classes created as type symbols inside
        # the owner-type's scope. Their associated scopes are attached to the symbols.
        func_scopes = get_all_function_scopes(type_scope, lhs_identifier)

        # Lists to save valid overloads and func overload errors. These lists are mutually exclusive.
        valid_overloads = []
        func_overload_errors = []

        for func_scope, func_overload, owner_scope_generic_arguments in func_scopes:

            # Get the parameter (+ identifiers) the arguments (+ named identifiers) and the generic arguments.
            original_func_overload = func_overload = func_overload.body.members[-1]
            parameters = Seq(func_overload.parameters.parameters.copy())
            parameter_identifiers = Seq(func_overload.parameters.parameters).map(lambda p: p.identifier_for_param())
            parameter_identifiers_required = Seq(func_overload.parameters.get_req()).map(lambda p: p.identifier_for_param())
            arguments = Seq(self.arguments.arguments.copy())
            named_argument_identifiers = Seq(arguments).filter_to_type(FunctionArgumentNamedAst).map(lambda a: a.identifier)

            generic_parameters = Seq(func_overload.generic_parameters.parameters.copy())
            generic_arguments = Seq(self.generic_arguments.arguments.copy())
            is_function_variadic = func_overload.parameters.parameters and isinstance(func_overload.parameters.parameters[-1], FunctionParameterVariadicAst)
            specialized_scope_info = {"created": False, "remove_function": None}

            # A try-except block is needed, to catch overload errors and allow the movement into the next overload.
            try:
                # Check too many arguments haven't been passed to the function.
                if arguments.length > parameters.length and not is_function_variadic:
                    raise SemanticErrors.TOO_MANY_ARGUMENTS(arguments[parameter_identifiers.length])

                # Check for any named arguments that don't match the function's parameters.
                if invalid_named_argument_identifiers := named_argument_identifiers.set_subtract(parameter_identifiers):
                    raise SemanticErrors.UNKNOWN_IDENTIFIER(invalid_named_argument_identifiers[0], parameter_identifiers.map(str).list(), "parameter")

                # Remove all named arguments from the parameter identifiers list.
                for argument in arguments.filter_to_type(FunctionArgumentNamedAst):
                    parameter_identifiers.remove(argument.identifier)

                # Name all the anonymous function arguments and generic type arguments, and overwrite the named lists.
                arguments = convert_function_arguments_to_named(arguments, parameters)
                generic_arguments = convert_generic_arguments_to_named(generic_arguments, generic_parameters, func_scope)
                named_argument_identifiers = arguments.filter_to_type(FunctionArgumentNamedAst).map(lambda a: a.identifier)

                # Check all the required parameters have been given a corresponding named argument.
                if missing_parameter_identifiers := parameter_identifiers_required.set_subtract(named_argument_identifiers):
                    raise SemanticErrors.MISSING_ARGUMENT(self, missing_parameter_identifiers[0], "function call", "parameter")

                # Infer generic arguments, and inherit from the lhs type.
                generic_arguments = infer_generics_types(
                    ast=self,
                    generic_parameters=Seq(func_overload.generic_parameters.get_req()).map(lambda p: p.identifier).list(),
                    explicit_generic_arguments=(generic_arguments + owner_scope_generic_arguments).map(lambda a: (a.identifier, a.type)).dict(),
                    infer_from=arguments.map(lambda a: (a.identifier, a.infer_type(scope_handler, **kwargs).type)).dict(),
                    map_to=parameters.map(lambda p: (p.identifier_for_param(), p.type_declaration)).dict(),
                    scope_handler=scope_handler).list()

                # New overload generation for generic functions or variadic functions.
                if generic_arguments or is_function_variadic:

                    # Temporarily remove the body of the overload before copying it (faster).
                    func_body = func_overload.body
                    func_overload.body = None

                    # Remove the generic arguments from the specialisation, and re-link the body.
                    specialized_func_overload = copy.deepcopy(func_overload)
                    specialized_func_overload.generic_parameters.parameters = []
                    specialized_func_overload.body = func_body
                    specialized_func_overload._orig = func_overload._orig
                    func_overload.body = func_body

                    # Substitute the generics types in the [parameter / return type] declarations.
                    for generic_argument in generic_arguments:
                        for parameter in specialized_func_overload.parameters.parameters:
                            parameter.type_declaration.substitute_generics(generic_argument.identifier, generic_argument.type)
                        specialized_func_overload.return_type.substitute_generics(generic_argument.identifier, generic_argument.type)

                    # Do semantic analysis on the new function overload signature.
                    for parameter in specialized_func_overload.parameters.parameters:
                        parameter.do_semantic_analysis(scope_handler, **kwargs)
                    specialized_func_overload.return_type.do_semantic_analysis(scope_handler, **kwargs)

                    # If the variadic parameter is non-generic, substitute it with the correct tuple.
                    if is_function_variadic and not func_scope.get_symbol(parameters[-1].type_declaration).is_generic:
                        variadic_argument_tuple_type = CommonTypes.tuple([parameters[-1].type_declaration for _ in range(len(arguments[-1].value.items))])
                        specialized_func_overload.parameters.parameters[-1].type_declaration = variadic_argument_tuple_type

                    # Load the new parameters and function scope for type-checking post-generic substitution.
                    parameters = Seq(specialized_func_overload.parameters.parameters)
                    func_overload = specialized_func_overload

                # Type checking arguments against the parameters. This has to come after generic substitution.
                sorted_arguments = arguments.sort(key=lambda a: parameter_identifiers.index(a.identifier))
                for i, (argument, parameter) in sorted_arguments.zip(parameters).enumerate():
                    argument_symbol = scope_handler.current_scope.get_outermost_variable_symbol(argument.value)
                    argument_type = argument.infer_type(scope_handler, **kwargs)
                    parameter_type = InferredType(convention=type(parameter.convention), type=parameter.type_declaration)

                    # Special case for the variadic parameter: check all the elements in the tuple match.
                    if isinstance(parameter, FunctionParameterVariadicAst):
                        unique_generics = Seq(parameter.type_declaration.types[-1].generic_arguments.arguments).map(lambda a: a.type).unique_items()
                        if unique_generics.length > 1:
                            raise SemanticErrors.VARIADIC_ARGUMENT_MULTIPLE_TYPES(parameter, unique_generics[0], unique_generics[1])

                    if isinstance(parameter, FunctionParameterSelfAst):
                        argument_type.convention = parameter_type.convention
                        argument.convention = parameter.convention

                    # Otherwise, check the argument type directly matches the parameter type.
                    elif not argument_type.symbolic_eq(parameter_type, scope_handler.current_scope, func_scope):
                        extra = f" for parameter '{parameter.identifier_for_param().value}'"
                        raise SemanticErrors.TYPE_MISMATCH_2(None, argument, parameter_type, argument_type, scope_handler, extra=extra)

                # Save the overload and scope to the list of valid overloads.
                valid_overloads.append((func_overload, arguments.copy(), func_scope))

            except SemanticError as e:
                # Save the error to the list of overload errors.
                func_overload_errors.append((original_func_overload, e))

                # If a specialized scope was created for this overload, remove it.
                if specialized_scope_info["created"]:
                    specialized_scope_info["remove_function"]()

        # Format the display name for the method or function.
        match lhs:
            case PostfixExpressionAst():
                display_name = f"{lhs_type}::{lhs.op.identifier}"
                display_ast = lhs.op.identifier
            case _:
                display_name = f"{lhs}"
                display_ast = lhs

        called_signature = f"\n\nCalled signature:\n  - {display_name}("
        for argument in self.arguments.arguments:
            called_signature += f"{argument.infer_type(scope_handler, **kwargs)}, "
        called_signature = called_signature[:-2] + ")"

        # Error if there are no valid overloads for this function call.
        if not valid_overloads:
            # Merge all the overload errors.
            signatures = f"{called_signature}\n\nAvailable signatures:\n"
            for func_overload, func_overload_error in func_overload_errors:
                error_string = f"{display_name}{func_overload.print_signature(AstPrinter())}"
                signatures += f"  - {error_string}\n"

            for func_overload, func_overload_error in func_overload_errors:
                error_string = f"{display_name}{func_overload.print_signature(AstPrinter())}"
                error_string += func_overload_error.additional_info[-1][1].replace("\n", "\n\t")
                signatures += f"\n{error_string}\n"
            raise SemanticErrors.NO_VALID_OVERLOADS(display_ast, signatures)

        # Error if there are multiple valid overloads for this function call (after generic substitution).
        if len(valid_overloads) > 1:
            signatures = f"{called_signature}\n\nAvailable signatures:\n"
            for func_overload, *_ in valid_overloads:
                error_string  = f"{display_name}{func_overload.print_signature(AstPrinter())}"
                signatures += f"  - {error_string}\n"
            raise SemanticErrors.AMBIGUOUS_FUNCTION_CALL(self, signatures)

        self._overload = valid_overloads[0]
        return self._overload

    def do_semantic_analysis(self, scope_handler: ScopeHandler, lhs: "ExpressionAst" = None, **kwargs) -> None:
        # Check a matching overload exists for the function call. Also get the "self" argument (for analysis)
        if self._overload is None:
            self.arguments.do_semantic_pre_analysis(scope_handler, **kwargs)
            self._get_matching_overload(scope_handler, lhs, **kwargs)
            self.arguments.arguments = self._overload[1]
            self.generic_arguments.do_semantic_analysis(scope_handler, **kwargs)
            self.arguments.do_semantic_analysis(scope_handler, function_prototype_ast=self._overload[0], is_async=self._is_async, **kwargs)

    def infer_type(self, scope_handler: ScopeHandler, lhs: "ExpressionAst" = None, **kwargs) -> InferredType:
        from SPPCompiler.SemanticAnalysis.ASTs import ConventionMovAst, PostfixExpressionAst
        function_name = lhs

        # Get the matching overload and return its return-type. 2nd class borrows mean the object returned is always
        # owned => ConventionMovAst.
        function_proto, _, function_scope = self._overload
        function_return_type = copy.deepcopy(function_proto.return_type)

        if isinstance(function_name, PostfixExpressionAst):
            owner_scope = scope_handler.current_scope.get_symbol(function_name.lhs.infer_type(scope_handler, **kwargs).type).associated_scope
            function_return_type = owner_scope.get_symbol(function_return_type).fq_type

        return InferredType(convention=ConventionMovAst, type=function_return_type)


__all__ = ["PostfixExpressionOperatorFunctionCallAst"]
