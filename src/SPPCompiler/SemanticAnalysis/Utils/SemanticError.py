from __future__ import annotations

import difflib

import inflection
from colorama import Fore, Style
from fastenum import Enum
from typing import List, Optional, Tuple

from SPPCompiler.SemanticAnalysis.ASTs.Meta.Ast import Ast
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstUtils import InferredType
from SPPCompiler.SemanticAnalysis.Utils.Symbols import VariableSymbol, TypeSymbol


class SemanticErrorStringFormatType(Enum):
    NORMAL = 0
    MINIMAL = 1
    NO_FORMAT = 2


class SemanticErrorType(Enum):
    _NONE = -1
    TYPE_ERROR = 0
    VALUE_ERROR = 1
    LITERAL_ERROR = 2
    NAME_ERROR = 3
    ORDER_ERROR = 4
    MEMORY_ERROR = 5
    ASYNC_ERROR = 6


class SemanticError(Exception):
    additional_info: List[Tuple[int, str, str, SemanticErrorStringFormatType]]

    def __init__(self) -> None:
        super().__init__("")
        self.additional_info = []

    def add_error(self, pos: int, error_type: SemanticErrorType, message: str, tag_message: str, tip: str, format_: SemanticErrorStringFormatType = SemanticErrorStringFormatType.NORMAL) -> SemanticError:
        error_type = inflection.titleize(str(error_type).rsplit(".", 1)[-1].replace("_", " "))
        message = f"\n{Style.BRIGHT}{error_type}: {Style.NORMAL}{message}\n{Fore.LIGHTCYAN_EX}{Style.BRIGHT}Tip: {Style.NORMAL}{tip}"
        self.additional_info.append((pos, message, tag_message, format_))
        return self

    def add_info(self, pos: int, tag_message: str) -> SemanticError:
        self.add_error(pos, SemanticErrorType._NONE, "", tag_message, "", SemanticErrorStringFormatType.MINIMAL)
        return self


class SemanticErrors:
    @staticmethod
    def INVALID_ASSIGNMENT_LHS_EXPR(lhs: Ast) -> SemanticError:
        exception = SemanticError()
        exception.add_error(
            pos=lhs.pos, error_type=SemanticErrorType.VALUE_ERROR,
            tag_message=f"Non-identifier found.",
            message="Only identifiers can be assigned to.",
            tip="Change the LHS to a variable or attribute.")
        return exception

    @staticmethod
    def CANNOT_MUTATE_IMMUTABLE_SYM(lhs: Ast, lhs_symbol: VariableSymbol) -> SemanticError:
        exception = SemanticError()
        exception.add_info(
            pos=lhs_symbol.memory_info.ast_initialized.pos,
            tag_message=f"Variable '{lhs_symbol.name}' is declared immutable here.")
        exception.add_error(
            pos=lhs.pos, error_type=SemanticErrorType.VALUE_ERROR,
            tag_message=f"Assignment to immutable variable '{lhs_symbol.name}' here.",
            message="Cannot assign to an immutable variable.",
            tip="ADd the 'mut' keyword to declare the variable as mutable.")
        return exception

    @staticmethod
    def TYPE_MISMATCH(ast: Ast, lhs_type: InferredType, rhs_type: InferredType, lhs_symbol: Optional[VariableSymbol] = None, extra: str = "") -> SemanticError:
        # Todo: Accept the lhs_type_symbol, so the name of the class + namespace can be used (bypass aliases)
        # Todo: Change parameters to LHS, RHS, LHS-Type, RHS-Type, Extra
        exception = SemanticError()
        if lhs_symbol: exception.add_info(
            pos=(lhs_symbol.memory_info.ast_initialized or lhs_symbol.memory_info.ast_consumed).pos,
            tag_message=f"Variable '{lhs_symbol.name}' declared as '{lhs_type}'.")
        exception.add_error(
            pos=ast.pos, error_type=SemanticErrorType.TYPE_ERROR,
            tag_message=f"Inferred '{rhs_type}'.",
            message=f"Type mismatch between '{lhs_type}'{extra} and '{rhs_type}'.",
            tip="Ensure the RHS type exactly matches the LHS.")
        return exception

    @staticmethod
    def INVALID_BINARY_FOLD_EXPR_TYPE(rhs: Ast, rhs_type: InferredType) -> SemanticError:
        exception = SemanticError()
        exception.add_error(
            pos=rhs.pos, error_type=SemanticErrorType.VALUE_ERROR,
            message="Binary fold operations can only be applied to tuples.",
            tag_message=f"Inferred as '{rhs_type}' instead.",
            tip="Ensure that the right-hand-side of the binary fold operation is a tuple.")
        return exception

    @staticmethod
    def INVALID_BINARY_FOLD_EXPR_ELEMENT_TYPE(rhs: Ast, rhs_type: InferredType) -> SemanticError:
        exception = SemanticError()
        exception.add_error(
            pos=rhs.pos, error_type=SemanticErrorType.TYPE_ERROR,
            message="Binary fold operations can only be applied to tuples with elements of the same type.",
            tag_message=f"Inferred as '{rhs_type}' instead.",
            tip="Ensure that all elements of the right-hand-side tuple are of the same type.")
        return exception

    @staticmethod
    def INVALID_BINARY_FOLD_EXPR_ELEMENT_COUNT(rhs: Ast, count: int) -> SemanticError:
        exception = SemanticError()
        exception.add_error(
            pos=rhs.pos, error_type=SemanticErrorType.VALUE_ERROR,
            message="Binary fold operations can only be applied to tuples with at least 2 elements.",
            tag_message=f"The tuple has {count} elements.",
            tip="Ensure that the right-hand-side tuple has at least 2 elements.")
        return exception

    @staticmethod
    def INVALID_CLASS_ATTRIBUTE_TYPE(ast: Ast) -> SemanticError:
        exception = SemanticError()
        exception.add_error(
            pos=ast.pos, error_type=SemanticErrorType.TYPE_ERROR,
            message="Attributes cannot have the type 'Void'.",
            tag_message="Attribute type inferred as 'Void'.",
            tip="Change the type of the attribute to a valid type.")
        return exception

    @staticmethod
    def DUPLICATE_ITEM(duplicates: list[Ast], what: str) -> SemanticError:
        exception = SemanticError()
        exception.add_info(
            pos=duplicates[0].pos, tag_message=f"First instance of '{duplicates[0]}' found here.")
        exception.add_error(
            pos=duplicates[1].pos, error_type=SemanticErrorType.NAME_ERROR,
            tag_message=f"Second instance of '{duplicates[1]}' found here.",
            message=f"Duplicate {what} found.",
            tip=f"Ensure all {what}s are unique.")
        return exception

    @staticmethod
    def INVALID_ORDER(difference: list[tuple[type, Ast]], classification_ordering: dict[type, str], what: str) -> SemanticError:
        exception = SemanticError()
        exception.add_info(
            pos=difference[-2][1].pos,
            tag_message=f"{classification_ordering[difference[-2][0]]} {what} declared here.")
        exception.add_error(
            pos=difference[-1][1].pos, error_type=SemanticErrorType.ORDER_ERROR,
            tag_message=f"{classification_ordering[difference[-1][0]]} {what} declared here.",
            message=f"{classification_ordering[difference[-2][0]]} {what}s must follow {classification_ordering[difference[-1][0]].lower()} {what}s.",
            tip=f"Ensure the ordering of {what}s are correct.")
        return exception

    @staticmethod
    def USING_NON_INITIALIZED_VALUE(ast: Ast, symbol: VariableSymbol) -> SemanticError:
        exception = SemanticError()
        exception.add_info(
            pos=symbol.memory_info.ast_consumed.pos, tag_message=f"Variable '{ast}' is uninitialized/moved here.")
        exception.add_error(
            pos=ast.pos, error_type=SemanticErrorType.MEMORY_ERROR,
            tag_message=f"'{symbol.name}' used here.",
            message="Using a non-initialized value.",
            tip="Ensure that the value is fully initialized before being used.")
        return exception

    @staticmethod
    def USING_PARTIAL_MOVED_VALUE(ast: Ast, symbol: VariableSymbol) -> SemanticError:
        exception = SemanticError()
        exception.add_info(
            pos=symbol.memory_info.ast_partial_moves[0].pos, tag_message=f"Attribute '{symbol.memory_info.ast_partial_moves[0]}' is moved here.")
        exception.add_error(
            pos=ast.pos, error_type=SemanticErrorType.MEMORY_ERROR,
            tag_message=f"'{ast}' is partially moved.",
            message="Using a partially moved value.",
            tip="Ensure that the value is fully initialized before being used.")
        return exception

    @staticmethod
    def MEMORY_OVERLAP_CONFLICT(existing: Ast, conflict: Ast, existing_how: str, conflicting_how: str) -> SemanticError:
        exception = SemanticError()
        exception.add_info(
            existing.pos,
            tag_message=f"Object '{existing}' {existing_how.rstrip("e")}ed here")
        exception.add_error(
            pos=conflict.pos,
            error_type=SemanticErrorType.MEMORY_ERROR,
            tag_message=f"Overlapping object '{conflict}' {conflicting_how.rstrip("e")}ed here.",
            message=f"Cannot {conflicting_how} an object that's currently being {existing_how.rstrip("e")}ed.",
            tip=f"TODO")
        return exception

    @staticmethod
    def MOVING_FROM_BORROWED_CONTEXT(ast: Ast, operation: Ast, symbol: VariableSymbol) -> SemanticError:
        exception = SemanticError()
        exception.add_info(
            pos=symbol.memory_info.ast_borrow.pos,
            tag_message=f"Value {symbol.name} declared as borrowed here.")
        exception.add_error(
            pos=operation.pos, error_type=SemanticErrorType.MEMORY_ERROR,
            tag_message=f"Partial move of '{ast}' from '{symbol.name}' here.",
            message="Cannot move from a borrowed context.",
            tip="Ensure that the variable is not borrowed when moving attributes off it.")
        return exception

    @staticmethod
    def MUTABLE_BORROW_FROM_IMMUTABLE_SOURCE(ast: Ast, symbol: VariableSymbol) -> SemanticError:
        exception = SemanticError()
        exception.add_info(
            pos=symbol.memory_info.ast_initialized.pos,
            tag_message=f"Value {symbol.name} declared as immutable here.")
        exception.add_error(
            pos=ast.pos, error_type=SemanticErrorType.MEMORY_ERROR,
            tag_message=f"Mutable borrow of '{ast}' taken here.",
            message="Cannot take a mutable borrow from an immutable source.",
            tip="Declare the variable as mutable, using the 'mut' keyword.")
        return exception

    @staticmethod
    def MULTIPLE_VARIADIC_PARAMETERS(first: Ast, second: Ast) -> SemanticError:
        exception = SemanticError()
        exception.add_info(
            pos=first.pos,
            tag_message=f"1st variadic parameter '{first}' declared here")
        exception.add_error(
            pos=second.pos,
            error_type=SemanticErrorType.ORDER_ERROR,
            message=f"Cannot have more than 1 variadic parameter in function prototype",
            tag_message=f"2nd variadic parameter '{second}' declared here",
            tip="Remove the extra variadic parameter, or make it non-variadic")
        return exception

    @staticmethod
    def OPTIONAL_PARAM_REQUIRES_MOV_CONVENTION(ast: Ast) -> SemanticError:
        exception = SemanticError()
        exception.add_error(
            pos=ast.pos,
            error_type=SemanticErrorType.MEMORY_ERROR,
            message="Optional parameters must use the by-mov convention",
            tag_message=f"Convention '{ast}' used here.",
            tip=f"Remove the '{ast}' convention from the parameter declaration")
        return exception

    @staticmethod
    def MISSING_RETURN_STATEMENT(ret: Ast, end: Ast) -> SemanticError:
        exception = SemanticError()
        exception.add_info(
            pos=ret.pos,
            tag_message=f"Function return type defined as '{ret}'")
        exception.add_error(
            pos=end.pos, error_type=SemanticErrorType.TYPE_ERROR,
            tag_message="Return statement expected here.",
            message="Missing return statement at the end of the function.",
            tip="Ensure that the function returns a value.")
        return exception

    @staticmethod
    def UNKNOWN_IDENTIFIER(ast: Ast, similar: list, what: str) -> SemanticError:
        from SPPCompiler.SemanticAnalysis.ASTs import TypeAst
        value = ast.types[-1].value if isinstance(ast, TypeAst) else ast.value
        closest = difflib.get_close_matches(value, similar, n=1, cutoff=0)
        closest = f" Did you mean '{closest[0]}'?" if closest else ""

        exception = SemanticError()
        exception.add_error(
            pos=ast.pos, error_type=SemanticErrorType.NAME_ERROR,
            tag_message=f"{what.title()} '{ast}' does not exist.",
            message=f"Undefined {what}.",
            tip=f"Define the {what} in this scope.{closest}")
        return exception

    @staticmethod
    def CONFLICTING_IF_BRANCH_TYPES(t1: InferredType, t2: InferredType) -> SemanticError:
        exception = SemanticError()
        exception.add_info(
            pos=t1.type.pos,
            tag_message=f"Type '{t1}' inferred here.")
        exception.add_error(
            pos=t2.type.pos,
            error_type=SemanticErrorType.TYPE_ERROR,
            tag_message=f"Type '{t2}' inferred here.",
            message=f"Conflicting types found in if-expression being used for assignment.",
            tip=f"Ensure all branches return the same type.")
        return exception

    @staticmethod
    def ELSE_BRANCH_WRONG_POSITION(ast: Ast) -> SemanticError:
        exception = SemanticError()
        exception.add_error(
            pos=ast.pos,
            error_type=SemanticErrorType.ORDER_ERROR,
            tag_message="Else branch found here.",
            message="The else branch must be the final branch in an if-expression.",
            tip="Ensure the else branch is the final branch.")
        return exception

    @staticmethod
    def NO_ELSE_BRANCH(ast: Ast) -> SemanticError:
        exception = SemanticError()
        exception.add_error(
            pos=ast.pos,
            error_type=SemanticErrorType.ORDER_ERROR,
            tag_message="Non-else branch found here.",
            message="The final branch in an if-expression being used for assignment must be an else branch.",
            tip="Ensure the final branch is an else branch.")
        return exception

    @staticmethod
    def UNREACHABLE_CODE(ret: Ast, end: Ast) -> SemanticError:
        exception = SemanticError()
        exception.add_info(
            pos=ret.pos, tag_message="Return statement found.")
        exception.add_error(
            pos=end.pos, error_type=SemanticErrorType.ORDER_ERROR,
            tag_message="Unreachable code detected.",
            message="Code after a return statement is unreachable.",
            tip="Ensure that no code comes after a return statement.")
        return exception

    @staticmethod
    def NON_INSTANTIABLE_TYPE(ast: Ast, symbol: TypeSymbol) -> SemanticError:
        exception = SemanticError()
        exception.add_info(
            pos=symbol.name.pos, tag_message=f"Generic type '{symbol.name}' defined here.")
        exception.add_error(
            pos=ast.pos, error_type=SemanticErrorType.TYPE_ERROR,
            tag_message=f"Type '{ast}' instantiated here.",
            message="Cannot instantiate a generic type.",
            tip="Ensure the type is not a generic type.")
        return exception

    @staticmethod
    def MISSING_ARGUMENT(ast: Ast, arg: Ast, what: str, item: str) -> SemanticError:
        exception = SemanticError()
        exception.add_error(
            pos=ast.pos,
            error_type=SemanticErrorType.ORDER_ERROR,
            tag_message=f"Missing {item} '{arg}' in {what}",
            message=f"{what.title()} declared here",
            tip=f"Give the missing {item} '{arg}' a value")
        return exception

    @staticmethod
    def UNCALLABLE_TYPE(ast: Ast) -> SemanticError:
        exception = SemanticError()
        exception.add_error(
            pos=ast.pos,
            error_type=SemanticErrorType.TYPE_ERROR,
            tag_message=f"'{ast}' is not callable as a function.",
            message=f"Non-function type is being called.",
            tip="Call (postfix-) identifiers.")
        return exception

    @staticmethod
    def TOO_MANY_ARGUMENTS(ast: Ast) -> SemanticError:
        exception = SemanticError()
        exception.add_error(
            pos=ast.pos,
            error_type=SemanticErrorType.VALUE_ERROR,
            tag_message="Extra arguments provided.",
            message="Too many arguments provided",
            tip="Ensure the correct number of arguments are provided.")
        return exception

    @staticmethod
    def NO_VALID_OVERLOADS(ast: Ast, signatures: str) -> SemanticError:
        exception = SemanticError()
        exception.add_error(
            pos=ast.pos,
            error_type=SemanticErrorType.NAME_ERROR,
            tag_message="No valid overloads found.",
            message="No valid overloads found for function with signatures.",
            tip=f"Ensure the function is called with the correct arguments:\n{signatures}")
        return exception

    @staticmethod
    def AMBIGUOUS_FUNCTION_CALL(ast: Ast, overloads: str) -> SemanticError:
        exception = SemanticError()
        exception.add_error(
            pos=ast.pos,
            error_type=SemanticErrorType.NAME_ERROR,
            tag_message="Ambiguous function call.",
            message="Multiple overloads found for function call after generic substitution.",
            tip=f"Ensure the function call is unambiguous:\n{overloads}")
        return exception

    @staticmethod
    def NUMERICAL_MEMBER_ACCESS_TYPE(lhs: Ast, rhs: Ast, lhs_ty: "TypeAst") -> SemanticError:
        exception = SemanticError()
        exception.add_info(
            pos=lhs.pos,
            tag_message=f"Type inferred as '{lhs_ty}'")
        exception.add_error(
            pos=rhs.pos,
            error_type=SemanticErrorType.TYPE_ERROR,
            tag_message=f"Numeric member access found here",
            message="Numeric member access requires a tuple type",
            tip="Use a tuple type for numeric member access")
        return exception

    @staticmethod
    def NUMERICAL_MEMBER_ACCESS_OUT_OF_BOUNDS(lhs: Ast, rhs: Ast, lhs_ty: "TypeAst") -> SemanticError:
        exception = SemanticError()
        exception.add_info(
            pos=lhs.pos,
            tag_message=f"Type inferred as '{lhs_ty}' ({len(lhs_ty.types[-1].generic_arguments.arguments)} elements)")
        exception.add_error(
            pos=rhs.pos,
            error_type=SemanticErrorType.TYPE_ERROR,
            tag_message=f"Numeric member access found here to element {rhs}",
            message="Numeric member access out of bounds",
            tip=f"Use a valid index for numeric member access (< {len(lhs_ty.types[-1].generic_arguments.arguments)})")
        return exception

    @staticmethod
    def MEMBER_ACCESS_GENERIC_TYPE(lhs: Ast, rhs: Ast, lhs_ty: "TypeAst") -> SemanticError:
        exception = SemanticError()
        exception.add_info(
            pos=lhs.pos,
            tag_message=f"Type inferred as '{lhs_ty}' (generic)")
        exception.add_error(
            pos=rhs.pos,
            error_type=SemanticErrorType.TYPE_ERROR,
            tag_message=f"Attribute '{rhs}' accessed here",
            message="Cannot access attributes on unconstrained generic types",
            tip="Constrain the generic type to allow attribute access")
        return exception

    @staticmethod
    def MEMBER_ACCESS_NON_EXISTENT(lhs: Ast, rhs: Ast, lhs_ty: InferredType, what: str, item: str) -> SemanticError:
        exception = SemanticError()
        exception.add_info(
            pos=lhs.pos,
            tag_message=f"{what.title()} inferred as '{lhs_ty}'")
        exception.add_error(
            pos=rhs.pos,
            error_type=SemanticErrorType.TYPE_ERROR,
            tag_message=f"{item.title()} '{rhs}' accessed here",
            message=f"Undefined {item}",
            tip=f"Check for typos or define the {item}")
        return exception

    @staticmethod
    def INVALID_ASYNC_CALL(ast: Ast, rhs: Ast) -> SemanticError:
        exception = SemanticError()
        exception.add_error(
            pos=ast.pos,
            error_type=SemanticErrorType.ASYNC_ERROR,
            tag_message=f"'{rhs}' is not a function call.",
            message="Invalid 'async' usage",
            tip="Make sure that the 'async' keyword is used before a function call.")
        return exception

    @staticmethod
    def INVALID_WITH_EXPRESSION(ast: Ast, ty: Ast) -> SemanticError:
        exception = SemanticError()
        exception.add_error(
            pos=ast.pos,
            error_type=SemanticErrorType.TYPE_ERROR,
            tag_message=f"Type inferred as '{ty}'",
            message=f"Type does not superimpose 'CtxRef' or 'CtxMut'",
            tip=f"Superimpose 'CtxRef' or 'CtxMut' over the type.")
        return exception

    @staticmethod
    def INVALID_COROUTINE_RETURN_TYPE(ast: Ast, ret: Ast) -> SemanticError:
        exception = SemanticError()
        exception.add_info(
            pos=ast.pos,
            tag_message="Function defined as a coroutine here.")
        exception.add_error(
            pos=ret.pos,
            error_type=SemanticErrorType.TYPE_ERROR,
            tag_message=f"Return type declared as '{ret}'",
            message="Invalid return type for a coroutine",
            tip="Use a valid generator return type.")
        return exception

    @staticmethod
    def YIELD_OUTSIDE_COROUTINE(ast: Ast, fun: Ast) -> SemanticError:
        exception = SemanticError()
        exception.add_info(
            pos=fun.pos,
            tag_message=f"Subroutine defined here.")
        exception.add_error(
            pos=ast.pos,
            error_type=SemanticErrorType.TYPE_ERROR,
            tag_message=f"Gen expression found here.",
            message="Gen expressions can only occur inside a coroutine.",
            tip="Return values using the 'ret' statement instead.")
        return exception

    @staticmethod
    def RETURN_OUTSIDE_SUBROUTINE(ast: Ast, cor: Ast) -> SemanticError:
        exception = SemanticError()
        exception.add_info(
            pos=cor.pos,
            tag_message=f"Coroutine defined here.")
        exception.add_error(
            pos=ast.pos,
            error_type=SemanticErrorType.TYPE_ERROR,
            tag_message=f"Ret statement found here.",
            message="Ret statement can only occur inside a subroutine.",
            tip="Yield values using the 'gen' expression instead.")
        return exception

    @staticmethod
    def GENERIC_INFERRABLE(ast: Ast, orig: Ast, new: Ast) -> SemanticError:
        exception = SemanticError()
        exception.add_info(
            pos=orig.pos,
            tag_message=f"Generic argument '{ast}' explicitly given here")
        exception.add_error(
            pos=new.pos,
            error_type=SemanticErrorType.NAME_ERROR,
            tag_message=f"'{ast}' inferred by an argument here",
            message=f"Type redeclaration",
            tip="Remove the redefined type, or re-name it.")
        return exception

    @staticmethod
    def MISSING_GENERIC_ARGUMENT(ast: Ast, what: Ast) -> SemanticError:
        exception = SemanticError()
        exception.add_error(
            pos=ast.pos,
            error_type=SemanticErrorType.NAME_ERROR,
            tag_message=f"Missing generic argument for '{what}'",
            message="Generic argument missing",
            tip="Provide a generic argument for the type.")
        return exception

    @staticmethod
    def MULTIPLE_ARGUMENT_SKIPS(ast0: Ast, ast1: Ast) -> SemanticError:
        exception = SemanticError()
        exception.add_info(
            pos=ast0.pos, tag_message="Multiple skip arguments found.")
        exception.add_error(
            pos=ast1.pos, error_type=SemanticErrorType.ORDER_ERROR,
            tag_message="Multiple skip arguments in a tuple.",
            message="Only one skip argument is allowed in a tuple.",
            tip="Remove the additional skip argument.")
        return exception

    @staticmethod
    def UNPACKING_TOKEN_IN_DESTRUCTURE(ast: Ast) -> SemanticError:
        exception = SemanticError()
        exception.add_error(
            pos=ast.pos, error_type=SemanticErrorType.ORDER_ERROR,
            tag_message="Binding unpacking token in a destructure.",
            message="A binding uUnpacking token is not allowed in a destructure.",
            tip="Remove the binding identifier.")
        return exception

    @staticmethod
    def SKIPPING_ARGUMENTS_IN_STATELESS_TYPE(ast: Ast) -> SemanticError:
        exception = SemanticError()
        exception.add_error(
            pos=ast.pos, error_type=SemanticErrorType.ORDER_ERROR,
            tag_message="Skipping arguments in a 0-attribute type.",
            message="Skipping arguments is not allowed in a 0-attribute type.",
            tip="Remove the skip argument.")
        return exception

    @staticmethod
    def TUPLE_SIZE_MISMATCH(lhs: Ast, rhs: Ast, lhs_c: int, rhs_c: int) -> SemanticError:
        exception = SemanticError()
        exception.add_info(
            pos=lhs.pos, tag_message=f"Assignment tuple contains {lhs_c} items.")
        exception.add_error(
            pos=rhs.pos, error_type=SemanticErrorType.TYPE_ERROR,
            tag_message=f"Assignment value contains {rhs_c} items.",
            message="Tuple length mismatch.",
            tip="Ensure both tuples have contain an equal number of elements.")
        return exception

    @staticmethod
    def CONFLICTING_GENERIC_INFERENCE(what: Ast, existing: Ast, conflicting: Ast) -> SemanticError:
        exception = SemanticError()
        exception.add_info(
            pos=existing.pos, tag_message=f"'{what}' inferred here as '{existing}'.")
        exception.add_error(
            pos=conflicting.pos, error_type=SemanticErrorType.TYPE_ERROR,
            tag_message=f"'{what}' inferred here as '{conflicting}'.",
            message="Conflicting generic inference.",
            tip="Ensure that the inferred generic arguments are same.")
        return exception

    @staticmethod
    def VOID_USAGE(ast: Ast) -> SemanticError:
        exception = SemanticError()
        exception.add_error(
            pos=ast.pos, error_type=SemanticErrorType.TYPE_ERROR,
            tag_message="Type 'Void' used in assignment.",
            message="Cannot assign type 'Void' to a variable.",
            tip="Use a valid type for the assignment.")
        return exception

    @staticmethod
    def MODULE_NS(ast: Ast, ns: str) -> SemanticError:
        exception = SemanticError()
        exception.add_info(
            pos=-1, tag_message=f"In file '{ns.replace("::", "/")}'")
        exception.add_error(
            pos=ast.pos, error_type=SemanticErrorType.NAME_ERROR,
            tag_message=f"Module declared as '{ast}'.",
            message="Module namespace does not match the file path.",
            tip="Ensure that the module namespace matches the file path.")
        return exception

    @staticmethod
    def STATIC_MEMBER_TYPE_ACCESS(ast: Ast, op: Ast, what: str) -> SemanticError:
        from SPPCompiler.SemanticAnalysis.ASTs import TypeAst
        value = ast.types[-1] if isinstance(ast, TypeAst) else ast
        exception = SemanticError()
        exception.add_info(
            pos=value.pos, tag_message=f"'{ast}' is a {what}.")
        exception.add_error(
            pos=op.pos, error_type=SemanticErrorType.NAME_ERROR,
            tag_message=f"Runtime member access found here.",
            message=f"Runtime member access on {what}s is not allowed.",
            tip="Use '::' instead of '.' for static member access.")
        return exception

    @staticmethod
    def RUNTIME_MEMBER_TYPE_ACCESS(ast: Ast, op: Ast, what: str) -> SemanticError:
        from SPPCompiler.SemanticAnalysis.ASTs import TypeAst
        value = ast.types[-1] if isinstance(ast, TypeAst) else ast
        exception = SemanticError()
        exception.add_info(
            pos=value.pos, tag_message=f"'{ast}' is a {what}.")
        exception.add_error(
            pos=op.pos, error_type=SemanticErrorType.NAME_ERROR,
            tag_message=f"Static member access found here.",
            message=f"Static member access on {what}s is not allowed.",
            tip="Use '.' instead of '::' for runtime member access.")
        return exception

    @staticmethod
    def INVALID_ITERABLE_TYPE(ast: Ast, ty: InferredType) -> SemanticError:
        exception = SemanticError()
        exception.add_error(
            pos=ast.iterable.pos, error_type=SemanticErrorType.TYPE_ERROR,
            tag_message=f"Non-iterable type inferred as: '{ty}'.",
            message="Loop/in expressions require an iterable type.",
            tip="Ensure the type superimposes 'IterMov', 'IterMut' or 'IterRef'.")
        return exception

    @staticmethod
    def CONTROL_FLOW_TOO_MANY_LOOPS(ast: Ast, num_control: int, num_loop: int) -> SemanticError:
        exception = SemanticError()
        exception.add_error(
            pos=ast.pos, error_type=SemanticErrorType.ORDER_ERROR,
            tag_message=f"Too many control flow statements found.",
            message=f"{num_control} statements where found inside {num_loop} loops",
            tip=f"Remove {num_control - num_loop} control flow statements.")
        return exception

    @staticmethod
    def CONTROL_FLOW_TYPE_MISMATCH(ast_cf1: Ast, ast_cf2: Ast, ty1: Ast, ty2: Ast) -> SemanticError:
        exception = SemanticError()
        exception.add_info(
            pos=ast_cf1.pos,
            tag_message=f"Control flow statement inferred as '{ty1}'.")
        exception.add_error(
            pos=ast_cf2.pos,
            error_type=SemanticErrorType.TYPE_ERROR,
            tag_message=f"Control flow statement inferred as '{ty2}'.",
            message="Control flow statements must have the same type.",
            tip="Ensure all control flow statements have the same type.")
        return exception

    @staticmethod
    def TYPE_DESTRUCTURING_NON_UNION_TYPE(ty: Ast) -> SemanticError:
        exception = SemanticError()
        exception.add_error(
            pos=ty.pos,
            error_type=SemanticErrorType.TYPE_ERROR,
            tag_message="Type is not a union type.",
            message="Type destructuring requires a union type.",
            tip="Ensure the type is a union type.")
        return exception

    @staticmethod
    def TYPE_DESTRUCTURING_INVALID_TYPE(ty: Ast, var: Ast) -> SemanticError:
        exception = SemanticError()
        exception.add_info(
            pos=var.pos,
            tag_message=f"Variant type declared here.")
        exception.add_error(
            pos=ty.pos,
            error_type=SemanticErrorType.TYPE_ERROR,
            tag_message=f"Type '{ty}' does not belong to the variant.",
            message="Invalid type for type destructuring.",
            tip="Ensure the type belongs to the variant type.")
        return exception

    @staticmethod
    def CONFLICTING_FUNCTION_OVERLOADS(name: str, ast0: Ast, ast1: Ast) -> SemanticError:
        exception = SemanticError()
        exception.add_info(
            pos=ast0.pos,
            tag_message=f"1st overload of function '{name}' found here.")
        exception.add_error(
            pos=ast1.pos,
            error_type=SemanticErrorType.NAME_ERROR,
            tag_message=f"Conflicting overload of function '{name}' found here.",
            message="Conflicting function overloads.",
            tip="Ensure that the function overloads have different signatures.")
        return exception

    @staticmethod
    def CONFLICTING_ATTRIBUTES(old_symbol: VariableSymbol, new_symbol: VariableSymbol, old_type: TypeAst, new_type: TypeAst) -> SemanticError:
        exception = SemanticError()
        exception.add_info(
            pos=old_symbol.name.pos,
            tag_message=f"1st attribute '{old_symbol.name}' found here for '{old_type}'.")
        exception.add_error(
            pos=new_symbol.name.pos,
            error_type=SemanticErrorType.NAME_ERROR,
            tag_message=f"Conflicting attribute '{new_symbol.name}' found here for '{new_type}'.",
            message="Conflicting attribute names.",
            tip="Ensure that all attributes have unique names.")
        return exception

    @staticmethod
    def UNPACKING_NON_TUPLE_ARGUMENT(ast: Ast, ty: InferredType) -> SemanticError:
        exception = SemanticError()
        exception.add_error(
            pos=ast.pos,
            error_type=SemanticErrorType.TYPE_ERROR,
            tag_message=f"Type inferred as '{ty}'.",
            message="Unpacking requires a tuple type.",
            tip="Ensure the type is a tuple type.")
        return exception

    @staticmethod
    def INVALID_SUPERIMPOSITION_MEMBER(ast: Ast, superclass_type: "TypeAst") -> SemanticError:
        exception = SemanticError()
        exception.add_info(
            pos=superclass_type.pos,
            tag_message=f"Superclass declared as '{superclass_type}'.")
        exception.add_error(
            pos=ast.pos,
            error_type=SemanticErrorType.TYPE_ERROR,
            tag_message=f"Invalid member found here.",
            message=f"Member does not exist on superclass '{superclass_type.without_generics()}'.",
            tip="Ensure the member is defined in the superclass.\n- Note that 'Self'-type parameters change type depending on their enclosing class.")
        return exception

    @staticmethod
    def TOO_MANY_GENERIC_ARGUMENTS(ast: Ast, symbol: TypeSymbol) -> SemanticError:
        exception = SemanticError()
        exception.add_info(
            pos=symbol.type.identifier.pos,
            tag_message=f"'{symbol.name}' declared here.")
        exception.add_error(
            pos=ast.pos,
            error_type=SemanticErrorType.VALUE_ERROR,
            tag_message="Extra generic type arguments provided.",
            message="Too many generic type arguments provided",
            tip="Ensure the correct number of generic type arguments are provided.")
        return exception

    @staticmethod
    def UNCONSTRAINED_GENERIC_PARAMETER(ast: Ast, gen: Ast) -> SemanticError:
        exception = SemanticError()
        exception.add_info(
            pos=ast.pos,
            tag_message=f"Superimposition declared here.")
        exception.add_error(
            pos=gen.pos,
            error_type=SemanticErrorType.TYPE_ERROR,
            tag_message=f"Generic parameter '{gen}' is unconstrained.",
            message="Unconstrained generic parameter.",
            tip="Constrain the generic parameter to a valid type.")
        return exception

    @staticmethod
    def MISSING_OBJ_INIT_SUP_ARGUMENT(ast: Ast) -> SemanticError:
        exception = SemanticError()
        exception.add_info(
            pos=ast.pos,
            tag_message=f"Object initialization here.")
        exception.add_error(
            pos=ast.pos,
            error_type=SemanticErrorType.VALUE_ERROR,
            tag_message="Missing a 'sup' argument.",
            message="Missing a 'sup' argument in object initialization.",
            tip="Ensure the object is initialized with q 'sup' argument.")
        return exception

    @staticmethod
    def UNEXPECTED_OBJ_INIT_SUP_ARGUMENT(ast: Ast, sup: Ast) -> SemanticError:
        exception = SemanticError()
        exception.add_info(
            pos=ast.pos,
            tag_message=f"Expected no superclass arguments.")
        exception.add_error(
            pos=sup.pos,
            error_type=SemanticErrorType.VALUE_ERROR,
            tag_message="Unexpected 'sup' argument.",
            message="Unexpected 'sup' argument in object initialization.",
            tip="Remove the 'sup' argument.")
        return exception

    @staticmethod
    def MISSING_OBJ_INIT_SUPER_CLASS(ast: Ast, expected: TypeAst) -> SemanticError:
        exception = SemanticError()
        exception.add_error(
            pos=ast.pos,
            error_type=SemanticErrorType.VALUE_ERROR,
            tag_message=f"Missing superclass '{expected}'.",
            message="Missing superclass in object initialization.",
            tip="Ensure the object is initialized with a superclass.")
        return exception

    @staticmethod
    def UNEXPECTED_OBJ_INIT_SUPER_CLASS(ast: Ast, given: TypeAst) -> SemanticError:
        exception = SemanticError()
        exception.add_info(
            pos=ast.pos,
            tag_message=f"Object initialization here.")
        exception.add_error(
            pos=given.pos,
            error_type=SemanticErrorType.VALUE_ERROR,
            tag_message=f"Unexpected superclass '{given}'.",
            message="Unexpected superclass in object initialization.",
            tip="Remove the unexpected superclass.")
        return exception

    @staticmethod
    def INVALID_OPERAND_COMPOUND_ASSIGNMENT(ast: Ast, lhs: Ast) -> SemanticError:
        exception = SemanticError()
        exception.add_info(
            pos=ast.pos,
            tag_message=f"Compound assignment found here.")
        exception.add_error(
            pos=lhs.pos,
            error_type=SemanticErrorType.VALUE_ERROR,
            tag_message="Invalid operand.",
            message="Invalid operand for compound assignment.",
            tip="Ensure the operand is a variable or attribute.")
        return exception

    @staticmethod
    def INVALID_PIN_TARGET(ast: Ast, pin: Ast) -> SemanticError:
        exception = SemanticError()
        exception.add_info(
            pos=ast.pos,
            tag_message=f"Pin operation found here.")
        exception.add_error(
            pos=pin.pos,
            error_type=SemanticErrorType.VALUE_ERROR,
            tag_message="Invalid pin target.",
            message="Invalid pin target.",
            tip="Ensure the target is a variable or attribute.")
        return exception

    @staticmethod
    def PIN_OVERLAP_CONFLICT(old: Ast, new: Ast) -> SemanticError:
        exception = SemanticError()
        exception.add_info(
            pos=old.pos,
            tag_message=f"Expression pinned here.")
        exception.add_error(
            pos=new.pos,
            error_type=SemanticErrorType.VALUE_ERROR,
            tag_message="Overlapping pin created here.",
            message="Cannot define overlapping pins.",
            tip="Use more precise pins, or use one broad pin.")
        return exception

    @staticmethod
    def UNPINNING_NON_PINNED(ast: Ast, rel: Ast) -> SemanticError:
        exception = SemanticError()
        exception.add_info(
            pos=ast.pos,
            tag_message=f"Unpin operation found here.")
        exception.add_error(
            pos=rel.pos,
            error_type=SemanticErrorType.VALUE_ERROR,
            tag_message="Non-pinned expression found here.",
            message="Cannot unpin a non-pinned expression.",
            tip="Ensure the expression is pinned before unpinning.")
        return exception

    @staticmethod
    def UNPINNING_CONSTANT(ast: Ast, constant: Ast) -> SemanticError:
        exception = SemanticError()
        exception.add_info(
            pos=constant.pos,
            tag_message=f"Global constant '{ast}' found here.")
        exception.add_error(
            pos=ast.pos,
            error_type=SemanticErrorType.VALUE_ERROR,
            tag_message="Global constant unpinned here.",
            message="Cannot unpin a global constant.",
            tip="Ensure the expression is not a global constant.")
        return exception

    @staticmethod
    def MOVING_PINNED_VALUE(ast: Ast, pin: Ast) -> SemanticError:
        exception = SemanticError()
        exception.add_info(
            pos=pin.pos,
            tag_message=f"'{ast}' pinned here.")
        exception.add_error(
            pos=ast.pos,
            error_type=SemanticErrorType.MEMORY_ERROR,
            tag_message="Pinned value moved here.",
            message="Cannot move a pinned value.",
            tip="Unpin the value, or don't copy the pinned value.")
        return exception

    @staticmethod
    def UNPINNED_BORROW(ast: Ast, fun: Ast, is_async: bool) -> SemanticError:
        what1 = "Coroutine definition" if not is_async else "Asynchronous tag"
        what2 = "coroutine" if not is_async else "asynchronous function"
        exception = SemanticError()
        # exception.add_info(
        #     pos=fun.pos,
        #     tag_message=f"{what1} here.")
        exception.add_error(
            pos=ast.pos,
            error_type=SemanticErrorType.MEMORY_ERROR,
            tag_message="Unpinned borrow found here.",
            message=f"Cannot borrow an unpinned value into a {what2} call.",
            tip="Ensure the value is pinned before borrowing it.")
        return exception

    @staticmethod
    def INCONSISTENT_MEMORY_INIT_STATUS(ast: Ast) -> SemanticError:
        exception = SemanticError()
        exception.add_error(
            pos=ast.pos,
            error_type=SemanticErrorType.MEMORY_ERROR,
            tag_message="Value might not be initialized.",
            message="A conditional block hasn't consistently initialized or consumed this value in all branches.",
            tip="Ensure the memory is consistently initialized or consumed in each branch.")
        return exception

    @staticmethod
    def INCONSISTENT_MEMORY_PIN_STATUS(ast: Ast) -> SemanticError:
        exception = SemanticError()
        exception.add_error(
            pos=ast.pos,
            error_type=SemanticErrorType.MEMORY_ERROR,
            tag_message="Value might not be pinned.",
            message="A conditional block hasn't consistently pinned or unpinned this value in all branches.",
            tip="Ensure the memory is consistently pinned or unpinned in each branch.")
        return exception

    @staticmethod
    def MUTABLE_GLOBAL_CONSTANT(ast: Ast) -> SemanticError:
        exception = SemanticError()
        exception.add_error(
            pos=ast.pos,
            error_type=SemanticErrorType.MEMORY_ERROR,
            tag_message="Global constant marked mutable here.",
            message="Cannot declare a mutable global constant.",
            tip="Remove the 'mut' modifier.")
        return exception

    @staticmethod
    def REDEFINED_GLOBAL_CONSTANT(ast: Ast, new: Ast, old: Ast) -> SemanticError:
        exception = SemanticError()
        exception.add_info(
            pos=old.pos,
            tag_message=f"First declaration of '{ast}' found here.")
        exception.add_error(
            pos=new.pos,
            error_type=SemanticErrorType.NAME_ERROR,
            tag_message=f"Redeclaration of global constant {ast}.",
            message="Global constant redefinition.",
            tip="Ensure the global constant is only declared once.")
        return exception

    @staticmethod
    def CANNOT_USE_GENERIC_HERE(ast: Ast) -> SemanticError:
        exception = SemanticError()
        exception.add_error(
            pos=ast.pos,
            error_type=SemanticErrorType.TYPE_ERROR,
            tag_message=f"Generic identifier '{ast}' used here.",
            message="Cannot use a generic type here.",
            tip="Use a non-generic type here.")
        raise exception
