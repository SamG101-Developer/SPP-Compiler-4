from __future__ import annotations

import difflib

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

    def add_error(
            self, pos: int, tag: str, msg: str, tip: str,
            format_: SemanticErrorStringFormatType = SemanticErrorStringFormatType.NORMAL) -> SemanticError:

        msg = f"\n{Style.BRIGHT}Semantic Error: {Style.NORMAL}{msg}\n{Fore.LIGHTCYAN_EX}{Style.BRIGHT}Tip: {Style.NORMAL}{tip}"
        self.additional_info.append((pos, msg, tag, format_))
        return self

    def add_info(self, pos: int, tag: str) -> SemanticError:
        self.add_error(pos, tag, "", "", SemanticErrorStringFormatType.MINIMAL)
        return self


class SemanticErrors:
    @staticmethod
    def INVALID_ASSIGNMENT_LHS_EXPR(lhs: Ast) -> SemanticError:
        exception = SemanticError()
        exception.add_error(
            pos=lhs.pos,
            tag=f"Non-identifier found.",
            msg="Only identifiers can be assigned to.",
            tip="Change the LHS to a variable or attribute.")
        return exception

    @staticmethod
    def CANNOT_MUTATE_IMMUTABLE_SYM(lhs: Ast, lhs_symbol: VariableSymbol) -> SemanticError:
        exception = SemanticError()
        exception.add_info(
            pos=lhs_symbol.memory_info.ast_initialized.pos,
            tag=f"Variable '{lhs_symbol.name}' is declared immutable here.")
        exception.add_error(
            pos=lhs.pos,
            tag=f"Assignment to immutable variable '{lhs_symbol.name}' here.",
            msg="Cannot assign to an immutable variable.",
            tip="ADd the 'mut' keyword to declare the variable as mutable.")
        return exception

    @staticmethod
    def TYPE_MISMATCH_2(lhs: Optional[Ast], rhs: Ast, lhs_type: InferredType, rhs_type: InferredType, scope_handler: ScopeHandler, extra: str = "") -> SemanticError:
        exception = SemanticError()
        exception.add_info(
            pos=scope_handler.current_scope.get_outermost_variable_symbol(lhs).memory_info.ast_initialized.pos if lhs else lhs_type.type.pos,
            tag=f"LHS type declared as '{lhs_type}'.")
        exception.add_error(
            pos=rhs.pos,
            tag=f"RHS type inferred as '{rhs_type}'.",
            msg=f"Type mismatch between '{lhs_type}' and '{rhs_type}'{extra}.",
            tip="Ensure the RHS type exactly matches the LHS.")
        return exception

    @staticmethod
    def INVALID_BINARY_FOLD_EXPR_TYPE(rhs: Ast, rhs_type: InferredType) -> SemanticError:
        exception = SemanticError()
        exception.add_error(pos=rhs.pos, tag=f"Inferred as '{rhs_type}' instead.",
                            msg="Binary fold operations can only be applied to tuples.",
                            tip="Ensure that the right-hand-side of the binary fold operation is a tuple.")
        return exception

    @staticmethod
    def INVALID_BINARY_FOLD_EXPR_ELEMENT_TYPE(rhs: Ast, rhs_type: InferredType) -> SemanticError:
        exception = SemanticError()
        exception.add_error(pos=rhs.pos, tag=f"Inferred as '{rhs_type}' instead.",
                            msg="Binary fold operations can only be applied to tuples with elements of the same type.",
                            tip="Ensure that all elements of the right-hand-side tuple are of the same type.")
        return exception

    @staticmethod
    def INVALID_BINARY_FOLD_EXPR_ELEMENT_COUNT(rhs: Ast, count: int) -> SemanticError:
        exception = SemanticError()
        exception.add_error(pos=rhs.pos, tag=f"The tuple has {count} elements.",
                            msg="Binary fold operations can only be applied to tuples with at least 2 elements.",
                            tip="Ensure that the right-hand-side tuple has at least 2 elements.")
        return exception

    @staticmethod
    def INVALID_CLASS_ATTRIBUTE_TYPE(ast: Ast) -> SemanticError:
        exception = SemanticError()
        exception.add_error(pos=ast.pos, tag="Attribute type inferred as 'Void'.",
                            msg="Attributes cannot have the type 'Void'.",
                            tip="Change the type of the attribute to a valid type.")
        return exception

    @staticmethod
    def DUPLICATE_ITEM(duplicates: list[Ast], what: str) -> SemanticError:
        exception = SemanticError()
        exception.add_info(pos=duplicates[0].pos, tag=f"First instance of '{duplicates[0]}' found here.")
        exception.add_error(pos=duplicates[1].pos, tag=f"Second instance of '{duplicates[1]}' found here.",
                            msg=f"Duplicate {what} found.", tip=f"Ensure all {what}s are unique.")
        return exception

    @staticmethod
    def INVALID_ORDER(
            difference: list[tuple[type, Ast]], classification_ordering: dict[type, str], what: str) -> SemanticError:
        exception = SemanticError()
        exception.add_info(pos=difference[-2][1].pos,
                           tag=f"{classification_ordering[difference[-2][0]]} {what} declared here.")
        exception.add_error(pos=difference[-1][1].pos,
                            tag=f"{classification_ordering[difference[-1][0]]} {what} declared here.",
                            msg=f"{classification_ordering[difference[-2][0]]} {what}s must follow {classification_ordering[difference[-1][0]].lower()} {what}s.",
                            tip=f"Ensure the ordering of {what}s are correct.")
        return exception

    @staticmethod
    def USING_NON_INITIALIZED_VALUE(ast: Ast, symbol: VariableSymbol) -> SemanticError:
        exception = SemanticError()
        exception.add_info(pos=symbol.memory_info.ast_consumed.pos,
                           tag=f"Variable '{ast}' is uninitialized/moved here.")
        exception.add_error(pos=ast.pos, tag=f"'{symbol.name}' used here.", msg="Using a non-initialized value.",
                            tip="Ensure that the value is fully initialized before being used.")
        return exception

    @staticmethod
    def USING_PARTIAL_MOVED_VALUE(ast: Ast, symbol: VariableSymbol) -> SemanticError:
        exception = SemanticError()
        exception.add_info(pos=symbol.memory_info.ast_partial_moves[0].pos,
                           tag=f"Attribute '{symbol.memory_info.ast_partial_moves[0]}' is moved here.")
        exception.add_error(pos=ast.pos, tag=f"'{ast}' is partially moved.", msg="Using a partially moved value.",
                            tip="Ensure that the value is fully initialized before being used.")
        return exception

    @staticmethod
    def MEMORY_OVERLAP_CONFLICT(existing: Ast, conflict: Ast, existing_how: str, conflicting_how: str) -> SemanticError:
        exception = SemanticError()
        exception.add_info(existing.pos, tag=f"Object '{existing}' {existing_how.rstrip("e")}ed here")
        exception.add_error(pos=conflict.pos,
                            tag=f"Overlapping object '{conflict}' {conflicting_how.rstrip("e")}ed here.",
                            msg=f"Cannot {conflicting_how} an object that's currently being {existing_how.rstrip("e")}ed.",
                            tip=f"TODO")
        return exception

    @staticmethod
    def MOVING_FROM_BORROWED_CONTEXT(ast: Ast, operation: Ast, symbol: VariableSymbol) -> SemanticError:
        exception = SemanticError()
        exception.add_info(pos=symbol.memory_info.ast_borrow.pos, tag=f"Value {symbol.name} declared as borrowed here.")
        exception.add_error(pos=operation.pos, tag=f"Partial move of '{ast}' from '{symbol.name}' here.",
                            msg="Cannot move from a borrowed context.",
                            tip="Ensure that the variable is not borrowed when moving attributes off it.")
        return exception

    @staticmethod
    def MUTABLE_BORROW_FROM_IMMUTABLE_SOURCE(ast: Ast, symbol: VariableSymbol) -> SemanticError:
        exception = SemanticError()
        exception.add_info(pos=symbol.memory_info.ast_initialized.pos,
                           tag=f"Value {symbol.name} declared as immutable here.")
        exception.add_error(pos=ast.pos, tag=f"Mutable borrow of '{ast}' taken here.",
                            msg="Cannot take a mutable borrow from an immutable source.",
                            tip="Declare the variable as mutable, using the 'mut' keyword.")
        return exception

    @staticmethod
    def MULTIPLE_VARIADIC_PARAMETERS(first: Ast, second: Ast) -> SemanticError:
        exception = SemanticError()
        exception.add_info(pos=first.pos, tag=f"1st variadic parameter '{first}' declared here")
        exception.add_error(pos=second.pos, tag=f"2nd variadic parameter '{second}' declared here",
                            msg=f"Cannot have more than 1 variadic parameter in function prototype",
                            tip="Remove the extra variadic parameter, or make it non-variadic")
        return exception

    @staticmethod
    def OPTIONAL_PARAM_REQUIRES_MOV_CONVENTION(ast: Ast) -> SemanticError:
        exception = SemanticError()
        exception.add_error(pos=ast.pos, tag=f"Convention '{ast}' used here.",
                            msg="Optional parameters must use the by-mov convention",
                            tip=f"Remove the '{ast}' convention from the parameter declaration")
        return exception

    @staticmethod
    def MISSING_RETURN_STATEMENT(ret: Ast, end: Ast) -> SemanticError:
        exception = SemanticError()
        exception.add_info(pos=ret.pos, tag=f"Function return type defined as '{ret}'")
        exception.add_error(pos=end.pos, tag="Return statement expected here.",
                            msg="Missing return statement at the end of the function.",
                            tip="Ensure that the function returns a value.")
        return exception

    @staticmethod
    def UNKNOWN_IDENTIFIER(ast: Ast, similar: list, what: str) -> SemanticError:
        from SPPCompiler.SemanticAnalysis.ASTs import TypeAst
        value = ast.types[-1].value if isinstance(ast, TypeAst) else ast.value
        closest = difflib.get_close_matches(value, similar, n=1, cutoff=0)
        closest = f" Did you mean '{closest[0]}'?" if closest else ""

        exception = SemanticError()
        exception.add_error(pos=ast.pos, tag=f"{what.title()} '{ast}' does not exist.", msg=f"Undefined {what}.",
                            tip=f"Define the {what} in this scope.{closest}")
        return exception

    @staticmethod
    def CONFLICTING_IF_BRANCH_TYPES(t1: InferredType, t2: InferredType) -> SemanticError:
        exception = SemanticError()
        exception.add_info(pos=t1.type.pos, tag=f"Type '{t1}' inferred here.")
        exception.add_error(pos=t2.type.pos, tag=f"Type '{t2}' inferred here.",
                            msg=f"Conflicting types found in if-expression being used for assignment.",
                            tip=f"Ensure all branches return the same type.")
        return exception

    @staticmethod
    def ELSE_BRANCH_WRONG_POSITION(ast: Ast) -> SemanticError:
        exception = SemanticError()
        exception.add_error(pos=ast.pos, tag="Else branch found here.",
                            msg="The else branch must be the final branch in an if-expression.",
                            tip="Ensure the else branch is the final branch.")
        return exception

    @staticmethod
    def NO_ELSE_BRANCH(ast: Ast) -> SemanticError:
        exception = SemanticError()
        exception.add_error(pos=ast.pos, tag="Non-else branch found here.",
                            msg="The final branch in an if-expression being used for assignment must be an else branch.",
                            tip="Ensure the final branch is an else branch.")
        return exception

    @staticmethod
    def UNREACHABLE_CODE(ret: Ast, end: Ast) -> SemanticError:
        exception = SemanticError()
        exception.add_info(pos=ret.pos, tag="Return statement found.")
        exception.add_error(pos=end.pos, tag="Unreachable code detected.",
                            msg="Code after a return statement is unreachable.",
                            tip="Ensure that no code comes after a return statement.")
        return exception

    @staticmethod
    def NON_INSTANTIABLE_TYPE(ast: Ast, symbol: TypeSymbol) -> SemanticError:
        exception = SemanticError()
        exception.add_info(pos=symbol.name.pos, tag=f"Generic type '{symbol.name}' defined here.")
        exception.add_error(pos=ast.pos, tag=f"Type '{ast}' instantiated here.",
                            msg="Cannot instantiate a generic type.", tip="Ensure the type is not a generic type.")
        return exception

    @staticmethod
    def MISSING_ARGUMENT(ast: Ast, arg: Ast, what: str, item: str) -> SemanticError:
        exception = SemanticError()
        exception.add_error(pos=ast.pos, tag=f"Missing {item} '{arg}' in {what}", msg=f"{what.title()} declared here",
                            tip=f"Give the missing {item} '{arg}' a value")
        return exception

    @staticmethod
    def UNCALLABLE_TYPE(ast: Ast) -> SemanticError:
        exception = SemanticError()
        exception.add_error(pos=ast.pos, tag=f"'{ast}' is not callable as a function.",
                            msg=f"Non-function type is being called.", tip="Call (postfix-) identifiers.")
        return exception

    @staticmethod
    def TOO_MANY_ARGUMENTS(ast: Ast) -> SemanticError:
        exception = SemanticError()
        exception.add_error(pos=ast.pos, tag="Extra arguments provided.", msg="Too many arguments provided",
                            tip="Ensure the correct number of arguments are provided.")
        return exception

    @staticmethod
    def NO_VALID_OVERLOADS(ast: Ast, signatures: str) -> SemanticError:
        exception = SemanticError()
        exception.add_error(pos=ast.pos, tag="No valid overloads found.",
                            msg="No valid overloads found for function with signatures.",
                            tip=f"Ensure the function is called with the correct arguments:\n{signatures}")
        return exception

    @staticmethod
    def AMBIGUOUS_FUNCTION_CALL(ast: Ast, overloads: str) -> SemanticError:
        exception = SemanticError()
        exception.add_error(pos=ast.pos, tag="Ambiguous function call.",
                            msg="Multiple overloads found for function call after generic substitution.",
                            tip=f"Ensure the function call is unambiguous:\n{overloads}")
        return exception

    @staticmethod
    def NUMERICAL_MEMBER_ACCESS_TYPE(lhs: Ast, rhs: Ast, lhs_ty: "TypeAst") -> SemanticError:
        exception = SemanticError()
        exception.add_info(pos=lhs.pos, tag=f"Type inferred as '{lhs_ty}'")
        exception.add_error(pos=rhs.pos, tag=f"Numeric member access found here",
                            msg="Numeric member access requires a tuple type",
                            tip="Use a tuple type for numeric member access")
        return exception

    @staticmethod
    def NUMERICAL_MEMBER_ACCESS_OUT_OF_BOUNDS(lhs: Ast, rhs: Ast, lhs_ty: "TypeAst") -> SemanticError:
        exception = SemanticError()
        exception.add_info(pos=lhs.pos,
                           tag=f"Type inferred as '{lhs_ty}' ({len(lhs_ty.types[-1].generic_arguments.arguments)} elements)")
        exception.add_error(pos=rhs.pos, tag=f"Numeric member access found here to element {rhs}",
                            msg="Numeric member access out of bounds",
                            tip=f"Use a valid index for numeric member access (< {len(lhs_ty.types[-1].generic_arguments.arguments)})")
        return exception

    @staticmethod
    def MEMBER_ACCESS_GENERIC_TYPE(lhs: Ast, rhs: Ast, lhs_ty: "TypeAst") -> SemanticError:
        exception = SemanticError()
        exception.add_info(pos=lhs.pos, tag=f"Type inferred as '{lhs_ty}' (generic)")
        exception.add_error(pos=rhs.pos, tag=f"Attribute '{rhs}' accessed here",
                            msg="Cannot access attributes on unconstrained generic types",
                            tip="Constrain the generic type to allow attribute access")
        return exception

    @staticmethod
    def MEMBER_ACCESS_NON_EXISTENT(lhs: Ast, rhs: Ast, lhs_ty: InferredType, what: str, item: str) -> SemanticError:
        exception = SemanticError()
        exception.add_info(pos=lhs.pos, tag=f"{what.title()} inferred as '{lhs_ty}'")
        exception.add_error(pos=rhs.pos, tag=f"{item.title()} '{rhs}' accessed here", msg=f"Undefined {item}",
                            tip=f"Check for typos or define the {item}")
        return exception

    @staticmethod
    def INVALID_ASYNC_CALL(ast: Ast, rhs: Ast) -> SemanticError:
        exception = SemanticError()
        exception.add_error(pos=ast.pos, tag=f"'{rhs}' is not a function call.", msg="Invalid 'async' usage",
                            tip="Make sure that the 'async' keyword is used before a function call.")
        return exception

    @staticmethod
    def INVALID_WITH_EXPRESSION(ast: Ast, ty: Ast) -> SemanticError:
        exception = SemanticError()
        exception.add_error(pos=ast.pos, tag=f"Type inferred as '{ty}'",
                            msg=f"Type does not superimpose 'CtxRef' or 'CtxMut'",
                            tip=f"Superimpose 'CtxRef' or 'CtxMut' over the type.")
        return exception

    @staticmethod
    def INVALID_COROUTINE_RETURN_TYPE(ast: Ast, ret: Ast) -> SemanticError:
        exception = SemanticError()
        exception.add_info(pos=ast.pos, tag="Function defined as a coroutine here.")
        exception.add_error(pos=ret.pos, tag=f"Return type declared as '{ret}'",
                            msg="Invalid return type for a coroutine", tip="Use a valid generator return type.")
        return exception

    @staticmethod
    def YIELD_OUTSIDE_COROUTINE(ast: Ast, fun: Ast) -> SemanticError:
        exception = SemanticError()
        exception.add_info(pos=fun.pos, tag=f"Subroutine defined here.")
        exception.add_error(pos=ast.pos, tag=f"Gen expression found here.",
                            msg="Gen expressions can only occur inside a coroutine.",
                            tip="Return values using the 'ret' statement instead.")
        return exception

    @staticmethod
    def RETURN_OUTSIDE_SUBROUTINE(ast: Ast, cor: Ast) -> SemanticError:
        exception = SemanticError()
        exception.add_info(pos=cor.pos, tag=f"Coroutine defined here.")
        exception.add_error(pos=ast.pos, tag=f"Ret statement found here.",
                            msg="Ret statement can only occur inside a subroutine.",
                            tip="Yield values using the 'gen' expression instead.")
        return exception

    @staticmethod
    def GENERIC_INFERRABLE(ast: Ast, orig: Ast, new: Ast) -> SemanticError:
        exception = SemanticError()
        exception.add_info(pos=orig.pos, tag=f"Generic argument '{ast}' explicitly given here")
        exception.add_error(pos=new.pos, tag=f"'{ast}' inferred by an argument here", msg=f"Type redeclaration",
                            tip="Remove the redefined type, or re-name it.")
        return exception

    @staticmethod
    def MISSING_GENERIC_ARGUMENT(ast: Ast, what: Ast) -> SemanticError:
        exception = SemanticError()
        exception.add_error(pos=ast.pos, tag=f"Missing generic argument for '{what}'", msg="Generic argument missing",
                            tip="Provide a generic argument for the type.")
        return exception

    @staticmethod
    def MULTIPLE_ARGUMENT_SKIPS(ast0: Ast, ast1: Ast) -> SemanticError:
        exception = SemanticError()
        exception.add_info(pos=ast0.pos, tag="Multiple skip arguments found.")
        exception.add_error(pos=ast1.pos, tag="Multiple skip arguments in a tuple.",
                            msg="Only one skip argument is allowed in a tuple.",
                            tip="Remove the additional skip argument.")
        return exception

    @staticmethod
    def UNPACKING_TOKEN_IN_DESTRUCTURE(ast: Ast) -> SemanticError:
        exception = SemanticError()
        exception.add_error(pos=ast.pos, tag="Binding unpacking token in a destructure.",
                            msg="A binding uUnpacking token is not allowed in a destructure.",
                            tip="Remove the binding identifier.")
        return exception

    @staticmethod
    def SKIPPING_ARGUMENTS_IN_STATELESS_TYPE(ast: Ast) -> SemanticError:
        exception = SemanticError()
        exception.add_error(pos=ast.pos, tag="Skipping arguments in a 0-attribute type.",
                            msg="Skipping arguments is not allowed in a 0-attribute type.",
                            tip="Remove the skip argument.")
        return exception

    @staticmethod
    def TUPLE_SIZE_MISMATCH(lhs: Ast, rhs: Ast, lhs_c: int, rhs_c: int) -> SemanticError:
        exception = SemanticError()
        exception.add_info(pos=lhs.pos, tag=f"Assignment tuple contains {lhs_c} items.")
        exception.add_error(pos=rhs.pos, tag=f"Assignment value contains {rhs_c} items.", msg="Tuple length mismatch.",
                            tip="Ensure both tuples have contain an equal number of elements.")
        return exception

    @staticmethod
    def CONFLICTING_GENERIC_INFERENCE(what: Ast, existing: Ast, conflicting: Ast) -> SemanticError:
        exception = SemanticError()
        exception.add_info(pos=existing.pos, tag=f"'{what}' inferred here as '{existing}'.")
        exception.add_error(pos=conflicting.pos, tag=f"'{what}' inferred here as '{conflicting}'.",
                            msg="Conflicting generic inference.",
                            tip="Ensure that the inferred generic arguments are same.")
        return exception

    @staticmethod
    def VOID_USAGE(ast: Ast) -> SemanticError:
        exception = SemanticError()
        exception.add_error(pos=ast.pos, tag="Type 'Void' used in assignment.",
                            msg="Cannot assign type 'Void' to a variable.", tip="Use a valid type for the assignment.")
        return exception

    @staticmethod
    def MODULE_NS(ast: Ast, ns: str) -> SemanticError:
        exception = SemanticError()
        exception.add_info(pos=-1, tag=f"In file '{ns.replace("::", "/")}'")
        exception.add_error(pos=ast.pos, tag=f"Module declared as '{ast}'.",
                            msg="Module namespace does not match the file path.",
                            tip="Ensure that the module namespace matches the file path.")
        return exception

    @staticmethod
    def STATIC_MEMBER_TYPE_ACCESS(ast: Ast, op: Ast, what: str) -> SemanticError:
        from SPPCompiler.SemanticAnalysis.ASTs import TypeAst
        value = ast.types[-1] if isinstance(ast, TypeAst) else ast
        exception = SemanticError()
        exception.add_info(pos=value.pos, tag=f"'{ast}' is a {what}.")
        exception.add_error(pos=op.pos, tag=f"Runtime member access found here.",
                            msg=f"Runtime member access on {what}s is not allowed.",
                            tip="Use '::' instead of '.' for static member access.")
        return exception

    @staticmethod
    def RUNTIME_MEMBER_TYPE_ACCESS(ast: Ast, op: Ast, what: str) -> SemanticError:
        from SPPCompiler.SemanticAnalysis.ASTs import TypeAst
        value = ast.types[-1] if isinstance(ast, TypeAst) else ast
        exception = SemanticError()
        exception.add_info(pos=value.pos, tag=f"'{ast}' is a {what}.")
        exception.add_error(pos=op.pos, tag=f"Static member access found here.",
                            msg=f"Static member access on {what}s is not allowed.",
                            tip="Use '.' instead of '::' for runtime member access.")
        return exception

    @staticmethod
    def INVALID_ITERABLE_TYPE(ast: Ast, ty: InferredType) -> SemanticError:
        exception = SemanticError()
        exception.add_error(pos=ast.iterable.pos, tag=f"Non-iterable type inferred as: '{ty}'.",
                            msg="Loop/in expressions require an iterable type.",
                            tip="Ensure the type superimposes 'IterMov', 'IterMut' or 'IterRef'.")
        return exception

    @staticmethod
    def CONTROL_FLOW_TOO_MANY_LOOPS(ast: Ast, num_control: int, num_loop: int) -> SemanticError:
        exception = SemanticError()
        exception.add_error(pos=ast.pos, tag=f"Too many control flow statements found.",
                            msg=f"{num_control} statements where found inside {num_loop} loops",
                            tip=f"Remove {num_control - num_loop} control flow statements.")
        return exception

    @staticmethod
    def CONTROL_FLOW_TYPE_MISMATCH(ast_cf1: Ast, ast_cf2: Ast, ty1: Ast, ty2: Ast) -> SemanticError:
        exception = SemanticError()
        exception.add_info(pos=ast_cf1.pos, tag=f"Control flow statement inferred as '{ty1}'.")
        exception.add_error(pos=ast_cf2.pos, tag=f"Control flow statement inferred as '{ty2}'.",
                            msg="Control flow statements must have the same type.",
                            tip="Ensure all control flow statements have the same type.")
        return exception

    @staticmethod
    def TYPE_DESTRUCTURING_NON_UNION_TYPE(ty: Ast) -> SemanticError:
        exception = SemanticError()
        exception.add_error(pos=ty.pos, tag="Type is not a union type.",
                            msg="Type destructuring requires a union type.", tip="Ensure the type is a union type.")
        return exception

    @staticmethod
    def TYPE_DESTRUCTURING_INVALID_TYPE(ty: Ast, var: Ast) -> SemanticError:
        exception = SemanticError()
        exception.add_info(pos=var.pos, tag=f"Variant type declared here.")
        exception.add_error(pos=ty.pos, tag=f"Type '{ty}' does not belong to the variant.",
                            msg="Invalid type for type destructuring.",
                            tip="Ensure the type belongs to the variant type.")
        return exception

    @staticmethod
    def CONFLICTING_FUNCTION_OVERLOADS(name: str, ast0: Ast, ast1: Ast) -> SemanticError:
        exception = SemanticError()
        exception.add_info(pos=ast0.pos, tag=f"1st overload of function '{name}' found here.")
        exception.add_error(pos=ast1.pos, tag=f"Conflicting overload of function '{name}' found here.",
                            msg="Conflicting function overloads.",
                            tip="Ensure that the function overloads have different signatures.")
        return exception

    @staticmethod
    def CONFLICTING_ATTRIBUTES(
            old_symbol: VariableSymbol, new_symbol: VariableSymbol, old_type: TypeAst,
            new_type: TypeAst) -> SemanticError:
        exception = SemanticError()
        exception.add_info(pos=old_symbol.name.pos,
                           tag=f"1st attribute '{old_symbol.name}' found here for '{old_type}'.")
        exception.add_error(pos=new_symbol.name.pos,
                            tag=f"Conflicting attribute '{new_symbol.name}' found here for '{new_type}'.",
                            msg="Conflicting attribute names.", tip="Ensure that all attributes have unique names.")
        return exception

    @staticmethod
    def UNPACKING_NON_TUPLE_ARGUMENT(ast: Ast, ty: InferredType) -> SemanticError:
        exception = SemanticError()
        exception.add_error(pos=ast.pos, tag=f"Type inferred as '{ty}'.", msg="Unpacking requires a tuple type.",
                            tip="Ensure the type is a tuple type.")
        return exception

    @staticmethod
    def INVALID_SUPERIMPOSITION_MEMBER(ast: Ast, superclass_type: "TypeAst") -> SemanticError:
        exception = SemanticError()
        exception.add_info(pos=superclass_type.pos, tag=f"Superclass declared as '{superclass_type}'.")
        exception.add_error(pos=ast.pos, tag=f"Invalid member found here.",
                            msg=f"Member does not exist on superclass '{superclass_type.without_generics()}'.",
                            tip="Ensure the member is defined in the superclass.\n- Note that 'Self'-type parameters change type depending on their enclosing class.")
        return exception

    @staticmethod
    def TOO_MANY_GENERIC_ARGUMENTS(ast: Ast, symbol: TypeSymbol) -> SemanticError:
        exception = SemanticError()
        exception.add_info(pos=symbol.type.identifier.pos, tag=f"'{symbol.name}' declared here.")
        exception.add_error(pos=ast.pos, tag="Extra generic type arguments provided.",
                            msg="Too many generic type arguments provided",
                            tip="Ensure the correct number of generic type arguments are provided.")
        return exception

    @staticmethod
    def UNCONSTRAINED_GENERIC_PARAMETER(ast: Ast, gen: Ast) -> SemanticError:
        exception = SemanticError()
        exception.add_info(pos=ast.pos, tag=f"Superimposition declared here.")
        exception.add_error(pos=gen.pos, tag=f"Generic parameter '{gen}' is unconstrained.",
                            msg="Unconstrained generic parameter.",
                            tip="Constrain the generic parameter to a valid type.")
        return exception

    @staticmethod
    def MISSING_OBJ_INIT_SUP_ARGUMENT(ast: Ast) -> SemanticError:
        exception = SemanticError()
        exception.add_info(pos=ast.pos, tag=f"Object initialization here.")
        exception.add_error(pos=ast.pos, tag="Missing a 'sup' argument.",
                            msg="Missing a 'sup' argument in object initialization.",
                            tip="Ensure the object is initialized with q 'sup' argument.")
        return exception

    @staticmethod
    def UNEXPECTED_OBJ_INIT_SUP_ARGUMENT(ast: Ast, sup: Ast) -> SemanticError:
        exception = SemanticError()
        exception.add_info(pos=ast.pos, tag=f"Expected no superclass arguments.")
        exception.add_error(pos=sup.pos, tag="Unexpected 'sup' argument.",
                            msg="Unexpected 'sup' argument in object initialization.", tip="Remove the 'sup' argument.")
        return exception

    @staticmethod
    def MISSING_OBJ_INIT_SUPER_CLASS(ast: Ast, expected: TypeAst) -> SemanticError:
        exception = SemanticError()
        exception.add_error(pos=ast.pos, tag=f"Missing superclass '{expected}'.",
                            msg="Missing superclass in object initialization.",
                            tip="Ensure the object is initialized with a superclass.")
        return exception

    @staticmethod
    def UNEXPECTED_OBJ_INIT_SUPER_CLASS(ast: Ast, given: TypeAst) -> SemanticError:
        exception = SemanticError()
        exception.add_info(pos=ast.pos, tag=f"Object initialization here.")
        exception.add_error(pos=given.pos, tag=f"Unexpected superclass '{given}'.",
                            msg="Unexpected superclass in object initialization.",
                            tip="Remove the unexpected superclass.")
        return exception

    @staticmethod
    def OBJ_INIT_SUP_ARGUMENT_NON_TUPLE(ast: Ast, sup: Ast,  ty: InferredType) -> SemanticError:
        exception = SemanticError()
        exception.add_info(pos=ast.pos, tag=f"Object initialization here.")
        exception.add_error(pos=sup.pos, tag=f"Type inferred as '{ty}'.", msg="Superclass argument must be a tuple.",
                            tip="Ensure the superclass argument is a tuple.")
        return exception

    @staticmethod
    def INVALID_OPERAND_COMPOUND_ASSIGNMENT(ast: Ast, lhs: Ast) -> SemanticError:
        exception = SemanticError()
        exception.add_info(pos=ast.pos, tag=f"Compound assignment found here.")
        exception.add_error(pos=lhs.pos, tag="Invalid operand.", msg="Invalid operand for compound assignment.",
                            tip="Ensure the operand is a variable or attribute.")
        return exception

    @staticmethod
    def INVALID_PIN_TARGET(ast: Ast, pin: Ast) -> SemanticError:
        exception = SemanticError()
        exception.add_info(pos=ast.pos, tag=f"Pin operation found here.")
        exception.add_error(pos=pin.pos, tag="Invalid pin target.", msg="Invalid pin target.",
                            tip="Ensure the target is a variable or attribute.")
        return exception

    @staticmethod
    def PIN_OVERLAP_CONFLICT(old: Ast, new: Ast) -> SemanticError:
        exception = SemanticError()
        exception.add_info(pos=old.pos, tag=f"Expression pinned here.")
        exception.add_error(pos=new.pos, tag="Overlapping pin created here.", msg="Cannot define overlapping pins.",
                            tip="Use more precise pins, or use one broad pin.")
        return exception

    @staticmethod
    def UNPINNING_NON_PINNED(ast: Ast, rel: Ast) -> SemanticError:
        exception = SemanticError()
        exception.add_info(pos=ast.pos, tag=f"Unpin operation found here.")
        exception.add_error(pos=rel.pos, tag=f"Non-pinned expression '{rel}' found here.",
                            msg="Cannot unpin a non-pinned expression.",
                            tip="Ensure the expression is pinned before unpinning.")
        return exception

    @staticmethod
    def UNPINNING_CONSTANT(ast: Ast, constant: Ast) -> SemanticError:
        exception = SemanticError()
        exception.add_info(pos=constant.pos, tag=f"Global constant '{ast}' found here.")
        exception.add_error(pos=ast.pos, tag="Global constant unpinned here.", msg="Cannot unpin a global constant.",
                            tip="Ensure the expression is not a global constant.")
        return exception

    @staticmethod
    def MOVING_PINNED_VALUE(ast: Ast, pin: Ast) -> SemanticError:
        exception = SemanticError()
        exception.add_info(pos=pin.pos, tag=f"'{ast}' pinned here.")
        exception.add_error(pos=ast.pos, tag="Pinned value moved here.", msg="Cannot move a pinned value.",
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
        exception.add_error(pos=ast.pos, tag="Unpinned borrow found here.",
                            msg=f"Cannot borrow an unpinned value into a {what2} call.",
                            tip="Ensure the value is pinned before borrowing it.")
        return exception

    @staticmethod
    def INCONSISTENT_MEMORY_INIT_STATUS(ast: Ast) -> SemanticError:
        exception = SemanticError()
        exception.add_error(pos=ast.pos, tag="Value might not be initialized.",
                            msg="A conditional block hasn't consistently initialized or consumed this value in all branches.",
                            tip="Ensure the memory is consistently initialized or consumed in each branch.")
        return exception

    @staticmethod
    def INCONSISTENT_MEMORY_PIN_STATUS(ast: Ast) -> SemanticError:
        exception = SemanticError()
        exception.add_error(pos=ast.pos, tag="Value might not be pinned.",
                            msg="A conditional block hasn't consistently pinned or unpinned this value in all branches.",
                            tip="Ensure the memory is consistently pinned or unpinned in each branch.")
        return exception

    @staticmethod
    def MUTABLE_GLOBAL_CONSTANT(ast: Ast) -> SemanticError:
        exception = SemanticError()
        exception.add_error(pos=ast.pos, tag="Global constant marked mutable here.",
                            msg="Cannot declare a mutable global constant.", tip="Remove the 'mut' modifier.")
        return exception

    @staticmethod
    def REDEFINED_GLOBAL_CONSTANT(ast: Ast, new: Ast, old: Ast) -> SemanticError:
        exception = SemanticError()
        exception.add_info(pos=old.pos, tag=f"First declaration of '{ast}' found here.")
        exception.add_error(pos=new.pos, tag=f"Redeclaration of global constant {ast}.",
                            msg="Global constant redefinition.",
                            tip="Ensure the global constant is only declared once.")
        return exception

    @staticmethod
    def CONDITION_NON_BOOLEAN(block: Ast, condition: Ast, condition_type: InferredType, what: str) -> SemanticError:
        exception = SemanticError()
        exception.add_info(
            pos=block.pos,
            tag=f"{what.title()} block defined here.")
        exception.add_error(
            pos=condition.pos,
            tag=f"Condition inferred as '{condition_type}'.",
            msg="Condition must be a boolean.",
            tip="Ensure the condition is a boolean expression.")
        return exception

    @staticmethod
    def VARIADIC_ARGUMENT_MULTIPLE_TYPES(ast: Ast, ty1: "TypeAst", ty2: "TypeAst") -> SemanticError:
        exception = SemanticError()
        exception.add_info(
            pos=ty1.pos,
            tag=f"1st type '{ty1}' inferred here.")
        exception.add_error(
            pos=ty2.pos,
            tag=f"2nd type '{ty2}' inferred here.",
            msg="Variadic arguments must have the same type.",
            tip="Ensure all variadic arguments have the same type.")
        return exception

    @staticmethod
    def SUPERIMPOSITION_ONTO_GENERIC(identifier: Ast, generic: Ast) -> SemanticError:
        exception = SemanticError()
        exception.add_info(
            pos=generic.pos,
            tag=f"Generic type '{generic}' defined here.")
        exception.add_error(
            pos=identifier.pos,
            tag=f"Superimposition found here.",
            msg="Cannot superimpose with a generic type.",
            tip="Remove the superimposition, or superimpose a concrete type.")
        return exception

    @staticmethod
    def REDEFINED_TYPE(old: Ast, new: Ast) -> SemanticError:
        exception = SemanticError()
        exception.add_info(
            pos=old.pos,
            tag=f"First definition of '{old}' found here.")
        exception.add_error(
            pos=new.pos,
            tag=f"Redefined type '{new}' found here.",
            msg="Type redefinition.",
            tip="Ensure the type is only defined once.")
        return exception
