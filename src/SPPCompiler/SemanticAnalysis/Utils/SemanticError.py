from __future__ import annotations

import difflib
import inflection
from colorama import Fore, Style
from enum import Enum
from typing import List, Optional, Tuple

from SPPCompiler.SemanticAnalysis.ASTs.Meta.Ast import Ast
from SPPCompiler.SemanticAnalysis.Utils.Symbols import VariableSymbol, TypeSymbol
from SPPCompiler.SemanticAnalysis.ASTMixins.TypeInfer import InferredType
from SPPCompiler.Utils.Sequence import Seq


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
    def INVALID_LHS_EXPR(lhs: Ast) -> SemanticError:
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
        exception = SemanticError()
        if lhs_symbol: exception.add_info(
            pos=lhs_symbol.memory_info.ast_initialized.pos,
            tag_message=f"Variable '{lhs_symbol.name}' declared as '{lhs_type}'.")
        exception.add_error(
            pos=ast.pos, error_type=SemanticErrorType.TYPE_ERROR,
            tag_message=f"Expected type '{lhs_type}'{extra}, inferred '{rhs_type}'.",
            message="Type mismatch.",
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
            tag_message=f"{symbol.name} used here.",
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
            message=f"Cannot {conflicting_how} an object that's currently being {existing_how.rstrip("e")}ed.",
            tag_message=f"Overlapping object '{conflict}' {conflicting_how.rstrip("e")}ed here.",
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
    def SELF_PARAMETER_OUTSIDE_CLASS(ast: Ast) -> SemanticError:
        exception = SemanticError()
        exception.add_error(
            pos=ast.pos, error_type=SemanticErrorType.NAME_ERROR,
            message=f"Cannot use a 'self' parameter in module global space",
            tag_message=f"Parameter '{ast}' declared here",
            tip="Move the function into a 'sup' block")
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
        closest = difflib.get_close_matches(ast.value, similar, n=1, cutoff=0)
        closest = f" Did you mean '{closest[0]}'?" if closest else ""

        exception = SemanticError()
        exception.add_error(
            pos=ast.pos, error_type=SemanticErrorType.NAME_ERROR,
            tag_message=f"{what.title()} '{ast}' does not exist.",
            message=f"Undefined {what}.",
            tip=f"Define the {what} in this scope.{closest}")
        return exception

    @staticmethod
    def CONFLICTING_COMPARISON_OPERATORS(lhs: Ast, rhs: Ast) -> SemanticError:
        exception = SemanticError()
        exception.add_info(
            pos=lhs.pos,
            tag_message=f"Condition comparison operator '{lhs}' found here.")
        exception.add_error(
            pos=rhs.pos,
            error_type=SemanticErrorType.ORDER_ERROR,
            tag_message=f"Subsequent comparison operator '{rhs}' found here.",
            message=f"Cannot have a comparison operator in both the case-expression and pattern block.",
            tip=f"Remove one of the conflicting comparison operators.")
        return exception

    @staticmethod
    def NO_COMPARISON_OPERATOR(lhs: Ast, rhs: Ast) -> SemanticError:
        exception = SemanticError()
        exception.add_info(
            pos=lhs.pos,
            tag_message=f"No comparison operator found here.")
        exception.add_error(
            pos=rhs.pos,
            error_type=SemanticErrorType.ORDER_ERROR,
            tag_message=f"Branch's pattern block declared here with no operator.",
            message=f"No comparison operator found in case-expression or pattern block.",
            tip=f"Add a comparison operator to either the case-expression or pattern block.")
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
            tag_message=f"Object '{ast}' is not callable.",
            message=f"Uncallable object",
            tip="Only identifiers can be called.")
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
            tip=f"Ensure the function is called with the correct arguments. Available Signatures:\n{signatures}")
        return exception

    @staticmethod
    def NUMERICAL_MEMBER_ACCESS_TYPE(lhs: Ast, rhs: Ast, lhs_ty: InferredType) -> SemanticError:
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
    def NUMERICAL_MEMBER_ACCESS_OUT_OF_BOUNDS(lhs: Ast, rhs: Ast, lhs_ty: InferredType) -> SemanticError:
        exception = SemanticError()
        exception.add_info(
            pos=lhs.pos,
            tag_message=f"Type inferred as '{lhs_ty}' ({len(lhs_ty.type.parts[-1].generic_arguments.arguments)} elements)")
        exception.add_error(
            pos=rhs.pos,
            error_type=SemanticErrorType.TYPE_ERROR,
            tag_message=f"Numeric member access found here to element {rhs}",
            message="Numeric member access out of bounds",
            tip=f"Use a valid index for numeric member access (< {len(lhs_ty.type.parts[-1].generic_arguments.arguments)})")
        return exception

    @staticmethod
    def MEMBER_ACCESS_GENERIC_TYPE(lhs: Ast, rhs: Ast, lhs_ty: InferredType) -> SemanticError:
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
            pos=ret.pos,
            tag_message=f"Function type defined as '{ret}'")
        exception.add_error(
            pos=ast.pos,
            error_type=SemanticErrorType.TYPE_ERROR,
            tag_message=f"Gen expression found here",
            message="Gen expressions can only occur inside a generator",
            tip="Ensure the function returns a 'GenMov', 'GenRef' or 'GenMut' type.")
        return exception

    @staticmethod
    def GENERIC_INFERRABLE(ast: Ast, orig: Ast, new: Ast) -> SemanticError:
        exception = SemanticError()
        exception.add_info(
            pos=orig.pos,
            tag_message=f"{ast} defined here")
        exception.add_error(
            pos=new.pos,
            error_type=SemanticErrorType.NAME_ERROR,
            tag_message=f"{ast} has been inferred by an argument",
            message="Type redeclaration",
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
            tag_message="Unpacking token in a destructure.",
            message="Unpacking tokens are not allowed in a destructure.",
            tip="Remove the unpacking token.")
        return exception

    @staticmethod
    def TUPLE_SIZE_MISMATCH(lhs: Ast, rhs: Ast, lhs_c: int, rhs_c: int) -> SemanticError:
        exception = SemanticError()
        exception.add_info(
            pos=lhs.pos, tag_message=f"Assignment tuple contains {lhs_c} items.")
        exception.add_error(
            pos=rhs.pos, error_type=SemanticErrorType.TYPE_ERROR,
            tag_message=f"Assignment value contains {rhs_c} items.",
            message="The length of the tuple does not match the length of the other tuple.",
            tip="Ensure that the tuple has the same number of items as the other tuple.")
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
            tip="Ensure that the inferred generic arguments are unique.")
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
