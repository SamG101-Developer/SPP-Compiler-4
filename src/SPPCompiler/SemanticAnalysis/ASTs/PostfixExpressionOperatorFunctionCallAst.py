import copy
from dataclasses import dataclass, field
from typing import Optional, Tuple

from SPPCompiler.LexicalAnalysis.Tokens import TokenType
from SPPCompiler.SemanticAnalysis.ASTs.Meta.Ast import Ast
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstMixins import SemanticAnalyser
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstPrinter import *
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstUtils import TypeInfer, InferredType, infer_generics_types, convert_function_arguments_to_named, convert_generic_arguments_to_named
from SPPCompiler.SemanticAnalysis.Utils.Scopes import Scope, ScopeHandler
from SPPCompiler.SemanticAnalysis.Utils.SemanticError import SemanticError, SemanticErrors
from SPPCompiler.SemanticAnalysis.Utils.Symbols import TypeSymbol
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

    _overload: Optional[Tuple["FunctionPrototypeAst", Scope]] = field(default=None, init=False)

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

    def _get_matching_overload(self, scope_handler: ScopeHandler, function_name: "ExpressionAst", **kwargs) -> Tuple["FunctionPrototypeAst", Scope]:
        """
        Determine the correct overload to select based on the arguments given to the function call.

        Returns
            The matching function prototype, the scope of the function, and the "self" argument (if applicable). The
            "self" argument is created, and required for borrow-checking.
        """

        from SPPCompiler.LexicalAnalysis.Lexer import Lexer
        from SPPCompiler.SemanticAnalysis.ASTs import (
            IdentifierAst, PostfixExpressionAst, FunctionArgumentNamedAst, TokenAst, GenericArgumentGroupAst,
            FunctionParameterVariadicAst)
        from SPPCompiler.SyntacticAnalysis.Parser import Parser

        # Get the scope of the function. This is either in the current scope (to global), or from inside the sup scope
        # of the owner of the function. Raise an error for non-callable types.
        # Todo: change to check for Fun... super-impositions instead?
        # Todo: only other things would be literals, and they can't be called (still, for the sake of normalisation).
        match function_name:
            case IdentifierAst():
                function_scope = scope_handler.current_scope.get_symbol(Parser(Lexer(f"MOCK_{function_name.value}").lex(), "").parse_type().parse_once()).associated_scope
            case PostfixExpressionAst():
                owner_scope = scope_handler.current_scope.get_symbol(function_name.lhs.infer_type(scope_handler, **kwargs).type).associated_scope
                function_scope = owner_scope.get_symbol(Parser(Lexer(f"MOCK_{function_name.op.identifier}").lex(), "").parse_type().parse_once()).associated_scope
            case _:
                raise SemanticErrors.UNCALLABLE_TYPE(function_name)

        # todo: function_scope.sup_scopes are empty

        mock_function_sup_scopes = function_scope.sup_scopes
        function_overloads = Seq(mock_function_sup_scopes).map(lambda s: s[1].body.members).flat()
        function_overload_errors = []
        valid_overloads = []

        for (i, function_overload), function_overload_scope in function_overloads.enumerate().zip(Seq(mock_function_sup_scopes)):
            try:
                function_overload_scope = mock_function_sup_scopes[i][0]._children_scopes[0]
                parameter_identifiers = Seq(function_overload.parameters.parameters).map(lambda p: p.identifier_for_param())
                generic_arguments = Seq(self.generic_arguments.arguments.copy())

                restore_function_arguments = Seq(self.arguments.arguments)
                restore_generic_arguments = Seq(self.generic_arguments.arguments)

                # todo: function folding: "function(tup).."

                # Create a dummy "self" argument for class method calls.
                if self_param := function_overload.parameters.get_self():
                    self_arg = FunctionArgumentNamedAst(
                        pos=function_name.lhs.pos,
                        identifier=IdentifierAst(pos=-1, value="self"),
                        assignment_token=TokenAst.dummy(TokenType.TkAssign),
                        convention=self_param.convention,
                        value=function_name.lhs)
                    self.arguments.arguments.insert(0, self_arg)

                arguments = Seq(self.arguments.arguments)
                named_argument_identifiers = Seq(arguments).filter_to_type(FunctionArgumentNamedAst).map(lambda a: a.identifier)

                # Check too many arguments haven't been passed to the function.
                is_variadic_function = function_overload.parameters.parameters and isinstance(function_overload.parameters.parameters[-1], FunctionParameterVariadicAst)
                if arguments.length > len(function_overload.parameters.parameters) and not is_variadic_function:
                    raise SemanticErrors.TOO_MANY_ARGUMENTS(arguments[parameter_identifiers.length])

                # Check there aren't any named arguments that don't match the function's parameters.
                if invalid_arguments := named_argument_identifiers.set_subtract(parameter_identifiers):
                    raise SemanticErrors.UNKNOWN_IDENTIFIER(invalid_arguments[0], parameter_identifiers.map(lambda p: p.value).value, "parameter")

                # Remove all named arguments from the available parameters, leaving only the unnamed parameters.
                for argument in arguments.filter_to_type(FunctionArgumentNamedAst):
                    parameter_identifiers.remove(argument.identifier)

                # Convert all anonymous arguments and generics to named counterparts.
                self.arguments.arguments = convert_function_arguments_to_named(arguments, parameter_identifiers, is_variadic_function).value
                self.generic_arguments.arguments = convert_generic_arguments_to_named(generic_arguments, Seq(function_overload.generic_parameters.parameters)).value

                # Check all the required parameters have been assigned a value.
                argument_identifiers = arguments.map(lambda a: a.identifier)
                required_parameter_identifiers = Seq(function_overload.parameters.get_req()).map(lambda p: p.identifier_for_param())
                if missing_parameters := required_parameter_identifiers.set_subtract(argument_identifiers):
                    raise SemanticErrors.MISSING_ARGUMENT(self, missing_parameters[0], "function call", "parameter")

                # Inherit any generics from the owner scope into the function's generics.
                # Todo: Change "function_name.lhs" to the actual object that owns the functions (inc. sup scopes)
                # Todo: Otherwise, generics from sup-inheritance blocks aren't loaded
                if isinstance(function_name, PostfixExpressionAst) and function_name.op.dot_token.token.token_type == TokenType.TkDot:
                    owner_scope_generic_arguments = function_name.lhs.infer_type(scope_handler, **kwargs).type.parts[-1].generic_arguments.arguments
                else:
                    owner_scope_generic_arguments = []

                self.generic_arguments = GenericArgumentGroupAst.from_dict(infer_generics_types(
                    self,
                    Seq(function_overload.generic_parameters.get_req()).map(lambda p: p.identifier).value,
                    Seq(self.generic_arguments.arguments + owner_scope_generic_arguments).map(lambda a: (a.identifier, a.type)).dict(),
                    Seq(self.arguments.arguments).map(lambda a: (a.identifier, a.infer_type(scope_handler, **kwargs).type)).dict(),
                    Seq(function_overload.parameters.parameters).map(lambda p: (p.identifier_for_param(), p.type_declaration)).dict(),
                    scope_handler))

                # If there are generic arguments, create a new function overload, giving the generic parameters' symbols
                # their associated types, from the generic arguments.
                new_scope = False
                if self.generic_arguments.arguments:
                    # The body of the function won't be copied, but will be linked (much faster).
                    restore_body = function_overload.body
                    function_overload.body = None

                    # Copy the current overload, and remove the generic parameters.
                    specialized_function_overload = copy.deepcopy(function_overload)
                    specialized_function_overload.generic_parameters.parameters = []
                    specialized_function_overload.body = restore_body
                    function_overload.body = restore_body
                    for generic_argument in self.generic_arguments.arguments:
                        Seq(specialized_function_overload.parameters.parameters).map(lambda p: p.type_declaration).for_each(lambda t: t.substitute_generics(generic_argument.identifier, generic_argument.type))
                        specialized_function_overload.return_type.substitute_generics(generic_argument.identifier, generic_argument.type)

                    # Register the specialisation to the "parent" method, and add it to the module/sup-block.
                    function_overload._ctx.body.members.append(specialized_function_overload)
                    specialized_function_overload.pre_process(function_overload._ctx)

                    # Generate a scope for the specialisation in the correct place (end of the module/sup-block).
                    restore_scope = scope_handler.current_scope
                    scope_handler.current_scope = function_overload_scope._parent_scope
                    specialized_function_overload.generate(scope_handler)
                    non_generic_function_overload_scope = mock_function_sup_scopes[i][0]._children_scopes[-1]

                    # Set the current scope to the specialization's Fun sup-block scope, creating the inner symbols.
                    scope_handler.current_scope = non_generic_function_overload_scope
                    specialized_function_overload.do_semantic_analysis(scope_handler, override_scope=True)

                    # Restore the current scope of the scope handler
                    scope_handler.current_scope = restore_scope

                    # Set the values of the generic parameters' type symbols to the types of the generic arguments.
                    for generic_argument in self.generic_arguments.arguments:
                        type_sym = scope_handler.current_scope.get_symbol(generic_argument.type)
                        non_generic_function_overload_scope.add_symbol(TypeSymbol(name=generic_argument.identifier, type=type_sym.type))
                        non_generic_function_overload_scope.get_symbol(generic_argument.identifier).associated_scope = type_sym.associated_scope

                    # Mark that a new scope has been created.
                    new_scope = True

                    # Provide a removal mechanism should type-checking fail; the specialisation becomes unnecessary.
                    def remove_scope():
                        function_overload_scope._parent_scope._children_scopes.remove(non_generic_function_overload_scope)

                    # Overwrite the current function overload & its scope with the specialisation.
                    function_overload = specialized_function_overload
                    function_overload_scope = non_generic_function_overload_scope

                # Check the types of the arguments match the types of the parameters. Sort the arguments by the
                # parameter order for easy comparison. Parameters identifiers were removed from earlier to recreate the
                # list.
                parameter_identifiers = Seq(function_overload.parameters.parameters).map(lambda p: p.identifier_for_param())
                arguments = Seq(arguments).sort(key=lambda a: parameter_identifiers.index(a.identifier))
                for j, (argument, parameter) in arguments.zip(parameter_identifiers).enumerate():
                    argument_type = argument.infer_type(scope_handler, **kwargs)
                    parameter_type = InferredType(
                        convention=type(function_overload.parameters.parameters[j].convention),
                        type=function_overload.parameters.parameters[j].type_declaration)

                    argument_symbol = scope_handler.current_scope.get_outermost_variable_symbol(argument.value)
                    # For the variadic parameter, check all the argument type's tuple-elements match the parameter type.
                    if is_variadic_function and j == parameter_identifiers.length - 1:
                        for variadic_argument in argument.value.items:
                            tuple_element_type = variadic_argument.infer_type(scope_handler, **kwargs)
                            if not tuple_element_type.symbolic_eq(parameter_type, scope_handler.current_scope):
                                raise SemanticErrors.TYPE_MISMATCH(variadic_argument, parameter_type, tuple_element_type, argument_symbol, extra=f" for '{parameter.value}'")

                    # Skip the "self" argument (the type is guaranteed to be correct).
                    elif parameter.value == "self":
                        continue

                    # Otherwise, check the argument type directly matches the parameter type.
                    elif not argument_type.symbolic_eq(parameter_type, scope_handler.current_scope, function_overload_scope):
                        raise SemanticErrors.TYPE_MISMATCH(argument, parameter_type, argument_type, argument_symbol, extra=f" for '{parameter.value}'")

                # If the function call is valid, then add it to the list of valid overloads.
                valid_overloads.append((function_overload, function_overload_scope))
                if new_scope:
                    remove_scope()

                if function_overload.parameters.get_self():
                    self.arguments.arguments.remove(self_arg)

            except SemanticError as e:
                self.arguments.arguments = restore_function_arguments.value
                self.generic_arguments.arguments = restore_generic_arguments.value
                function_overload_errors.append((function_overload, e))

        # If there are no valid overloads, display each overload, and why it is invalid for the arguments.
        if not valid_overloads:
            signatures = ""

            match function_name:
                case PostfixExpressionAst():
                    owner_type = str(function_name.lhs.infer_type(scope_handler, **kwargs).type) + "::"
                    function_name_raw = function_name.op.identifier
                case _:
                    owner_type = ""
                    function_name_raw = function_name

            for function_overload, error in function_overload_errors:
                function_overload_string = f"{function_overload}"
                function_overload_string = f"{function_overload_string[:function_overload_string.index("{") - 1]}"
                function_overload_string = f"{owner_type}{function_name_raw}{function_overload_string[function_overload_string.index("("):]}"
                signatures += f"\n\t{function_overload_string} {error.additional_info[-1][1]}\n"

            match function_name:
                case PostfixExpressionAst(): ast = function_name.op.identifier
                case _: ast = function_name
            raise SemanticErrors.NO_VALID_OVERLOADS(ast, signatures)

        # Todo: handle multiple valid overloads. Use most precise, error for equal precision (ambiguous call).
        # Return the valid overload
        self._overload = valid_overloads[0]
        return self._overload

    def do_semantic_analysis(self, scope_handler: ScopeHandler, lhs: "ExpressionAst" = None, **kwargs) -> None:
        # Check a matching overload exists for the function call. Also get the "self" argument (for analysis)
        self.arguments.do_semantic_pre_analysis(scope_handler, **kwargs)
        self._get_matching_overload(scope_handler, lhs, **kwargs)
        self.generic_arguments.do_semantic_analysis(scope_handler, **kwargs)
        self.arguments.do_semantic_analysis(scope_handler, **kwargs)

    def infer_type(self, scope_handler: ScopeHandler, lhs: "ExpressionAst" = None, **kwargs) -> InferredType:
        from SPPCompiler.SemanticAnalysis.ASTs import ConventionMovAst, PostfixExpressionAst
        function_name = lhs

        # Get the matching overload and return its return-type. 2nd class borrows mean the object returned is always
        # owned => ConventionMovAst.
        function_proto, function_scope = self._overload
        function_return_type = copy.deepcopy(function_proto.return_type)

        if isinstance(function_name, PostfixExpressionAst) and function_name.op.dot_token.token.token_type == TokenType.TkDblColon:
            owner_scope = scope_handler.current_scope.get_symbol(function_name.lhs.infer_type(scope_handler, **kwargs).type).associated_scope
            function_return_type = owner_scope.get_symbol(function_return_type).fq_type

        return InferredType(convention=ConventionMovAst, type=function_return_type)


__all__ = ["PostfixExpressionOperatorFunctionCallAst"]
