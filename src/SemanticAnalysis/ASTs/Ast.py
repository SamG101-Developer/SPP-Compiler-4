from __future__ import annotations

import copy
import dataclasses
import hashlib
import difflib

from abc import ABC, abstractmethod
from dataclasses import dataclass
from ordered_set import OrderedSet
from typing import List, Optional, Self, Tuple, Type

from src.SemanticAnalysis.Symbols.Scopes import ScopeHandler, Scope
from src.SemanticAnalysis.Symbols.Symbols import TypeSymbol, VariableSymbol, MemoryStatus
from src.SemanticAnalysis.Symbols.SymbolGeneration import SymbolGenerator
from src.SemanticAnalysis.Analysis.SemanticAnalysis import SemanticAnalysis, BIN_OP_FUNCS, OP_PREC
from src.SemanticAnalysis.Analysis.SemanticError import SemanticError
from src.SemanticAnalysis.ASTs.AstPrinter import AstPrinter, ast_printer_method, ast_printer_method_indent
from src.SemanticAnalysis.TypeInfer import TypeInfer
from src.SemanticAnalysis.PreProcessor import PreProcessor
from src.SemanticAnalysis.CommonTypes import CommonTypes
# from src.SemanticAnalysis.LLVMGeneration import LLVMGeneration

from src.LexicalAnalysis.Tokens import Token, TokenType
from src.Utils.Sequence import Seq

# import llvmlite.ir as llvm_ir
# import llvmlite.binding as llvm_binding


@dataclass
class Ast(ABC):
    pos: int
    
    @ast_printer_method
    @abstractmethod
    def print(self, printer: AstPrinter) -> str:
        ...
    
    def __eq__(self, other):
        return isinstance(other, Ast)

    def __str__(self):
        printer = AstPrinter()
        return self.print(printer)


@dataclass
class TokenAst(Ast):
    token: Token

    @staticmethod
    def dummy(token_type: TokenType, info=None) -> Self:
        return TokenAst(-1, Token(info or token_type.value, token_type))

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        return self.token.token_metadata + (" " if self.token.token_type.name.startswith("Kw") else "")

    def __eq__(self, other):
        c1 = isinstance(other, TokenAst) and self.token.token_type == other.token.token_type
        c2 = self.token.token_metadata == other.token.token_metadata if self.token.token_type.name.startswith("Lx") else True
        return c1 and c2


@dataclass
class AnnotationAst(Ast):
    at_token: TokenAst
    identifier: ModuleIdentifierAst
    generic_arguments: GenericArgumentGroupAst
    arguments: FunctionArgumentGroupAst

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        return f"{self.at_token.print(printer)}{self.identifier.print(printer)}{self.generic_arguments.print(printer)}{self.arguments.print(printer)}"


@dataclass
class AssignmentStatementAst(Ast, SemanticAnalysis, TypeInfer):
    lhs: List[ExpressionAst]
    op: TokenAst
    rhs: ExpressionAst

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        return f"{Seq(self.lhs).print(printer, ", ")} {self.op.print(printer)} {self.rhs.print(printer)}"

    def do_semantic_analysis(self, scope_handler: ScopeHandler, **kwargs) -> None:
        ensure_memory_integrity_of_expression(self.rhs, scope_handler, **kwargs)

        lhs_symbols = []

        # Check the LHS is a valid assignment target. Valid assignment targets are identifiers or postfix member access
        # expressions (assigning to a class attribute for example).
        for lhs in self.lhs:

            # Ensure the LHS is valid (ie the identifier or attribute exists etc), before performing AST specific
            # instructions.
            lhs.do_semantic_analysis(scope_handler, **kwargs)
            match lhs:

                # If assigning to an identifier then append the symbol to the list of symbols to be assigned to. If
                # assigning to an attribute: append the symbol of the class owner of the attribute to the list of
                # symbols being assigned to. Otherwise, the assignment target is invalid, so raise an exception.
                case IdentifierAst():
                    sym = scope_handler.current_scope.get_symbol(lhs)
                    lhs_symbols.append(sym)

                case PostfixExpressionAst() if isinstance(lhs.op, PostfixExpressionOperatorMemberAccessAst):
                    sym = scope_handler.current_scope.get_symbol(lhs.lhs)
                    lhs_symbols.append(sym)

                case _:
                    exception = SemanticError(f"Invalid assignment target (must be an identifier):")
                    exception.add_traceback(lhs.pos, f"Assignment target '{lhs}' invalid.")
                    raise exception

        # Regular assignment with the "=" operator. Compound assignment, ie "+=" is not supported for semantic analysis
        # yet.
        if self.op.token.token_type == TokenType.TkAssign:
            for i, lhs_symbol in enumerate(lhs_symbols):

                # If the symbol isn't muutable or is the "&" borrow type, then this symbol cannot be mutated.
                # TODO: this is slightly wrong: it won't allow "&" variables to be re-assigned but it should.
                # TODO: it  should prevent "&" from being used in "&mut" i think? maybe just remove the "or ..."?
                if (not lhs_symbol.is_mutable or lhs_symbol.memory_info.is_borrow_ref) and lhs_symbol.memory_info.ast_initialized:
                    exception = SemanticError(f"Cannot assign to an immutable variable:")
                    exception.add_traceback(lhs_symbol.memory_info.ast_initialized.pos, f"Variable '{self.lhs[i]}' declared here immutably.")
                    exception.add_traceback(self.lhs[i].pos, f"Variable '{lhs_symbol.name}' assigned to here.")
                    raise exception

                # Resolve any (partial-) moves from the memory status information in the symbols acquired earlier. For
                # identifiers, mark the symbol as initialized and not consumed. For attributes, remove the attribute
                # from the list of partial moves (if it was a partial move)
                match self.lhs[i]:
                    case IdentifierAst() if not lhs_symbol.memory_info.ast_initialized:
                        lhs_symbol.memory_info.ast_initialized = self
                        lhs_symbol.memory_info.ast_consumed = None

                    case PostfixExpressionAst():
                        lhs_symbol.memory_info.ast_partial_moves = Seq(lhs_symbol.memory_info.ast_partial_moves).filter(lambda arg: arg != self.lhs[i]).value

            # Perform a type check between the LHS and RHS, to ensure that the types are the same. There is no implicit
            # casting due to the strong type system, all that's needed is a symbol eq check between the 2 types.
            if len(self.lhs) == 1:
                lhs_type = self.lhs[0].infer_type(scope_handler, **kwargs)
                rhs_type = self.rhs.infer_type(scope_handler, **kwargs)

                # If the conventions are the same, or the LHS is uninitialized (being resolved), and the types are the
                # same, then the match is valid.
                if (lhs_type[0] == rhs_type[0] or lhs_type[0] == ConventionNonInitAst) and lhs_type[1].symbolic_eq(rhs_type[1], scope_handler.current_scope):
                    pass

                # Otherwise, there is a type mismatch, so raise an exception.
                else:
                    exception = SemanticError(f"Type mismatch in assignment:")
                    exception.add_traceback(lhs_symbols[0].memory_info.ast_initialized.pos, f"Assignment target '{self.lhs[0]}' declared here with type '{lhs_type[0].default()}{lhs_type[1]}'.")  # TODO : should be symbol's initialization AST
                    exception.add_traceback(self.rhs.pos, f"Assignment value '{self.rhs}' inferred here with type '{rhs_type[0].default()}{rhs_type[1]}'.")
                    raise exception

            else:
                raise NotImplementedError()

        else:
            raise NotImplementedError()

    def infer_type(self, scope_handler: ScopeHandler, **kwargs) -> Tuple[Type[ConventionAst], TypeAst]:
        # Assignment never returns anything, so return the Void type. This is so that the memory rules of the language
        # can be adhered to.
        return ConventionMovAst, CommonTypes.void()


@dataclass
class BinaryExpressionAst(Ast, SemanticAnalysis, TypeInfer):
    lhs: ExpressionAst
    op: TokenAst
    rhs: ExpressionAst
    _as_func: Optional[PostfixExpressionAst] = dataclasses.field(default=None)

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        return f"({self.lhs.print(printer)} {self.op.print(printer)} {self.rhs.print(printer)})"

    def do_semantic_analysis(self, scope_handler: ScopeHandler, **kwargs) -> None:
        # TODO : special cases for ?? and "is".
        # TODO : special case for ".." as an operand

        # 3 stage mutation operation
        #   1. Re-arrange the arguments in the binary expression. To mitigate left-hand parsing issues, right hand parsing was used.
        #   2. Chain any comparison operators together, so that "a < b < c" becomes "a < b && b < c".
        #   3. Transform the binary expression to a function call.

        def fix_associativity(ast: BinaryExpressionAst) -> BinaryExpressionAst:
            # To mitigate the issue of left-hand recursive parsing, the parser uses right-hand recursive parsing. This
            # puts equal precedence operators on the right hand side of the tree, ie 1 + 2 + 3 would be 1 + (2 + 3), not
            # (1 + 2) + 3. This function fixes the associativity of the tree, so that the left hand side is always the
            # first operand, and the right hand side is always the last operand. TODO : this is bugged
            if not isinstance(ast.rhs, BinaryExpressionAst):
                return ast

            if OP_PREC[ast.op.token.token_type] >= OP_PREC[ast.rhs.op.token.token_type]:
                rhs = ast.rhs
                ast.rhs = rhs.rhs
                rhs.rhs = rhs.lhs
                rhs.lhs = ast.lhs
                rhs.op, ast.op = ast.op, rhs.op
                ast.lhs = rhs
                return fix_associativity(ast)

            return ast
        
        def combine_comparison_operators(ast: BinaryExpressionAst) -> BinaryExpressionAst:
            # A Python-borrowed feature is the combination of comparison operators, such as "a < b < c". This function
            # recursively combines comparison operators into a single binary expression, so that "a < b < c" becomes
            # "a < b && b < c".

            def is_comparison_operator(token):
                return token in {TokenType.TkEq, TokenType.TkNe, TokenType.TkLt, TokenType.TkGt, TokenType.TkLe, TokenType.TkGe}

            # If the ast isn't a binary expression, or the binary operator is not comparable-combinable, then return the
            # current AST, whatever it is.
            if not isinstance(ast.lhs, BinaryExpressionAst) or not is_comparison_operator(ast.op.token.token_type):
                return ast

            # Combine the comparison into separate binary expressions, so that "a < b < c" becomes "a < b && b < c".
            lhs = ast.lhs
            rhs = ast.rhs
            lhs = lhs.rhs
            ast.rhs = BinaryExpressionAst(ast.pos, lhs, ast.op, rhs)
            ast.op  = TokenAst.dummy(TokenType.TkLogicalAnd)
            combine_comparison_operators(ast.lhs)
                
            return ast

        def convert_to_function(ast: BinaryExpressionAst) -> PostfixExpressionAst:
            # Transform the binary expression to a function call. This doesn't have to go in pre-processing, because the
            # transformation is only temporary, and doesn't affect the tree at all. The original binary expression is
            # still used for the rest of the semantic analysis.

            # For "a + b", this would be "a.add"
            mock_function = PostfixExpressionAst(
                pos=ast.pos,
                lhs=ast.lhs,
                op=PostfixExpressionOperatorMemberAccessAst(
                    pos=ast.op.pos,
                    dot_token=TokenAst.dummy(TokenType.TkDot),
                    identifier=IdentifierAst(ast.op.pos, BIN_OP_FUNCS[ast.op.token.token_type])))

            # For "a + b", this would be "b"
            mock_function_call_argument = FunctionArgumentNormalAst(
                pos=ast.op.pos,
                convention=ConventionMovAst(ast.rhs.pos),
                unpack_token=None,
                value=ast.rhs)

            # For "a + b", this would be "a.add(b)"
            mock_function_call = PostfixExpressionAst(
                pos=ast.pos,
                lhs=mock_function,
                op=PostfixExpressionOperatorFunctionCallAst(
                    pos=ast.op.pos,
                    arguments=FunctionArgumentGroupAst(
                        pos=ast.op.pos,
                        paren_l_token=TokenAst.dummy(TokenType.TkParenL),
                        arguments=[mock_function_call_argument],
                        paren_r_token=TokenAst.dummy(TokenType.TkParenR)),
                    generic_arguments=None,
                    fold_token=None))
            
            return mock_function_call
        
        def convert_all_to_function(ast: BinaryExpressionAst) -> PostfixExpressionAst:
            if not isinstance(ast, BinaryExpressionAst):
                return ast
            
            ast.lhs = convert_all_to_function(ast.lhs)
            ast.rhs = convert_all_to_function(ast.rhs)
            return convert_to_function(ast)

        ast = fix_associativity(self)
        ast = combine_comparison_operators(ast)
        ast = convert_all_to_function(ast)



        self._as_func = ast
        self._as_func.do_semantic_analysis(scope_handler, **kwargs)
        return self._as_func

    def infer_type(self, scope_handler, **kwargs) -> Tuple[Type[ConventionAst], TypeAst]:
        # TODO : special case for ".." as an operand
        return self._as_func.infer_type(scope_handler, **kwargs)


@dataclass
class ClassAttributeAst(Ast, SemanticAnalysis):
    annotations: List[AnnotationAst]
    identifier: IdentifierAst
    colon_token: TokenAst
    type_declaration: TypeAst

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        return f"{Seq(self.annotations).print(printer, "\n")}{self.identifier.print(printer)}{self.colon_token.print(printer)} {self.type_declaration.print(printer)}"

    def do_semantic_analysis(self, scope_handler, **kwargs) -> None:
        # Check the annotations and that the type is valid.
        Seq(self.annotations).for_each(lambda a: a.do_semantic_analysis(scope_handler, **kwargs))
        self.type_declaration.do_semantic_analysis(scope_handler, **kwargs)


@dataclass
class ClassPrototypeAst(Ast, PreProcessor, SymbolGenerator, SemanticAnalysis):
    annotations: List[AnnotationAst]
    class_token: TokenAst
    identifier: TypeAst
    generic_parameters: Optional[GenericParameterGroupAst]
    where_block: Optional[WhereBlockAst]
    body: InnerScopeAst[ClassAttributeAst]
    _mod: ModuleIdentifierAst = dataclasses.field(default=None)

    def __post_init__(self):
        # Fill the generic parameters and where block with empty objects if they are None.
        self.generic_parameters = self.generic_parameters or GenericParameterGroupAst(-1, TokenAst.dummy(TokenType.TkBrackL), [], TokenAst.dummy(TokenType.TkBrackR))
        self.where_block = self.where_block or WhereBlockAst(-1, TokenAst.dummy(TokenType.KwWhere), WhereConstraintsGroupAst(-1, TokenAst.dummy(TokenType.TkBrackL), [], TokenAst.dummy(TokenType.TkBrackR)))

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        s = ""
        s += f"{Seq(self.annotations).print(printer, "\n")}{self.class_token.print(printer)}{self.identifier.print(printer)}"
        s += f"{self.generic_parameters.print(printer)}" if self.generic_parameters else ""
        s += f" {self.where_block.print(printer)}" if self.where_block else ""
        s += f"{self.body.print(printer)}"
        return s

    def pre_process(self, context: ModulePrototypeAst) -> None:
        # Replace "Self" in the generic parameters and attribute types so that they refer to the current class.
        Seq(self.body.members).for_each(lambda m: m.type_declaration.substitute_generics(CommonTypes.self(), self.identifier))
        Seq(self.generic_parameters.get_opt()).for_each(lambda p: p.default_value.substitute_generics(CommonTypes.self(), self.identifier))
        self._mod = context.identifier

    def generate(self, s: ScopeHandler) -> None:
        # Add a new TypeSymbol to the current scope, representing this class being generated. Move into the new scope
        # (representing the new type symbol). Associate the scope with the symbol.
        sym = TypeSymbol(self.identifier, self)
        s.current_scope.add_symbol(sym)
        s.into_new_scope(self.identifier)
        sym.associated_scope = s.current_scope

        # Add new TypeSymbols for each generic parameter to the scope, representing "None". This is because the
        # attributes may rely on these generic types. Build VariableSymbols for each attribute of the class. Add "Self"
        # as a TypeSymbol pointing to the current class.
        s.current_scope.add_symbol(TypeSymbol(CommonTypes.self(), self))
        Seq(self.generic_parameters.parameters).for_each(lambda p: s.current_scope.add_symbol(TypeSymbol(p.identifier, None)))
        Seq(self.body.members).for_each(lambda m: s.current_scope.add_symbol(VariableSymbol(m.identifier, m.type_declaration)))

        # Move back into the parent scope.
        s.exit_cur_scope()

    def do_semantic_analysis(self, scope_handler, **kwargs) -> None:
        # Move into the class scope to have access to types defined on the class (generics parameters)
        scope_handler.move_to_next_scope()

        # Analyse the annotations, generic parameters, where block and the body of the class, to ensure everything being
        # contained is valid.
        Seq(self.annotations).for_each(lambda a: a.do_semantic_analysis(scope_handler, **kwargs))
        self.generic_parameters.do_semantic_analysis(scope_handler, **kwargs)
        # self.where_block.do_semantic_analysis(s, scope_handler, **kwargs, **kwargs)
        self.body.do_semantic_analysis(scope_handler, inline_block=True, **kwargs)

        # Check that no attributes have the same name as each other. Raise an exception if they do.
        if Seq(self.body.members).map(lambda m: m.identifier).contains_duplicates():
            duplicate_attributes = Seq(self.body.members).map(lambda m: m.identifier).non_unique_items()[0]
            exception = SemanticError(f"Duplicate attributes '{duplicate_attributes[0]}' found on type '{self.identifier}':")
            exception.add_traceback(duplicate_attributes[0].pos, f"Attribute '{duplicate_attributes[0]}' declared here.")
            exception.add_traceback(duplicate_attributes[1].pos, f"Attribute '{duplicate_attributes[1]}' re-declared here.")
            raise exception

        # Move back into the parent scope.
        scope_handler.exit_cur_scope()

    def __json__(self):
        return self.identifier


@dataclass
class ConventionMovAst(Ast):
    def print(self, printer: AstPrinter) -> str:
        return f""

    @staticmethod
    def default() -> str:
        return f""

    def __eq__(self, other):
        return isinstance(other, ConventionMovAst)


@dataclass
class ConventionRefAst(Ast):
    ampersand_token: TokenAst

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        return f"{self.ampersand_token.print(printer)}"

    @staticmethod
    def default() -> str:
        return f"&"

    def __eq__(self, other):
        return isinstance(other, ConventionRefAst)


@dataclass
class ConventionMutAst(Ast):
    ampersand_token: TokenAst
    mut_token: TokenAst

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        return f"{self.ampersand_token.print(printer)}{self.mut_token.print(printer)}"

    @staticmethod
    def default() -> str:
        return f"&mut "

    def __eq__(self, other):
        return isinstance(other, ConventionMutAst)


class ConventionNonInitAst:
    @staticmethod
    def default() -> str:
        return f"Uninitialized: "

    def __eq__(self, other):
        return isinstance(other, ConventionNonInitAst)


class ConventionPartInitAst:
    @staticmethod
    def default() -> str:
        return f"Partially-Initialized: "

    def __eq__(self, other):
        return isinstance(other, ConventionPartInitAst)


type ConventionAst = (
        ConventionMovAst |
        ConventionRefAst |
        ConventionMutAst)


@dataclass
class FunctionArgumentNormalAst(Ast, SemanticAnalysis, TypeInfer):
    convention: ConventionAst
    unpack_token: Optional[TokenAst]
    value: ExpressionAst

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        s = ""
        s += f"{self.convention.print(printer)}"
        s += f"{self.unpack_token.print(printer)}" if self.unpack_token else ""
        s += f"{self.value.print(printer)}"
        return s

    def do_semantic_analysis(self, scope_handler: ScopeHandler, **kwargs) -> None:
        # Pass analysis onto the value of the argument.
        self.value.do_semantic_analysis(scope_handler, **kwargs)

    def infer_type(self, scope_handler: ScopeHandler, **kwargs) -> Tuple[Type[ConventionAst], TypeAst]:
        match self.convention, self.value.infer_type(scope_handler, **kwargs)[0]:
            case ConventionMovAst(), that_convention: convention = that_convention
            case self_convention, _: convention = type(self_convention)
        return convention, self.value.infer_type(scope_handler, **kwargs)[1]


@dataclass
class FunctionArgumentNamedAst(Ast, SemanticAnalysis, TypeInfer):
    identifier: IdentifierAst
    assignment_token: TokenAst
    convention: ConventionAst
    value: ExpressionAst

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        return f"{self.identifier.print(printer)}{self.assignment_token.print(printer)}{self.convention.print(printer)}{self.value.print(printer)}"

    def do_semantic_analysis(self, scope_handler: ScopeHandler, **kwargs) -> None:
        # Pass analysis onto the value of the argument.
        self.value.do_semantic_analysis(scope_handler, **kwargs)

    def infer_type(self, scope_handler: ScopeHandler, **kwargs) -> Tuple[Type[ConventionAst], TypeAst]:
        match self.convention, self.value.infer_type(scope_handler, **kwargs)[0]:
            case ConventionMovAst(), that_convention: convention = that_convention
            case self_convention, _: convention = type(self_convention)
        return convention, self.value.infer_type(scope_handler, **kwargs)[1]


type FunctionArgumentAst = (
        FunctionArgumentNormalAst |
        FunctionArgumentNamedAst)


@dataclass
class FunctionArgumentGroupAst(Ast, SemanticAnalysis):
    paren_l_token: TokenAst
    arguments: List[FunctionArgumentAst]
    paren_r_token: TokenAst

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        return f"{self.paren_l_token.print(printer)}{Seq(self.arguments).print(printer, ", ")}{self.paren_r_token.print(printer)}"

    def do_semantic_analysis(self, scope_handler: ScopeHandler, **kwargs) -> None:
        # Check no named arguments have the same name
        named_arguments = Seq(self.arguments).filter(lambda a: isinstance(a, GenericArgumentNamedAst)).map(lambda a: a.identifier)
        if named_arguments.contains_duplicates():
            duplicate_named_arguments = named_arguments.non_unique_items()[0]
            exception = SemanticError(f"Duplicate argument names '{duplicate_named_arguments[0]}' found in function prototype:")
            exception.add_traceback(duplicate_named_arguments[0].pos, f"Parameter '{duplicate_named_arguments[0]}' declared here.")
            exception.add_traceback(duplicate_named_arguments[1].pos, f"Parameter '{duplicate_named_arguments[1]}' re-declared here.")
            raise exception

        # Check argument order is Normal -> Named
        ordering = {FunctionArgumentNormalAst: "Normal", FunctionArgumentNamedAst: "Named"}
        current_classifications = Seq(self.arguments).map(lambda p: (type(p), p))
        sorted_classifications = current_classifications.sort(key=lambda t: list(ordering.keys()).index(t[0]))
        if current_classifications != sorted_classifications:
            difference = sorted_classifications.ordered_difference(current_classifications)
            exception = SemanticError(f"Invalid parameter order in function prototype:")
            exception.add_traceback(difference[-2][1].pos, f"{ordering[difference[-2][0]]} parameter '{difference[-2][1]}' declared here.")
            exception.add_traceback(difference[-1][1].pos, f"{ordering[difference[-1][0]]} parameter '{difference[-1][1]}' declared here.")
            raise exception

        # TODO : Expand tuple into multiple arguments, so that each part is analysed.
        # TODO : Check memory status of symbols too, not just their convention.

        # Begin memory checks here to prevent overlaps of borrows.
        borrows_ref = OrderedSet()
        borrows_mut = OrderedSet()

        for argument in self.arguments:
            argument.do_semantic_analysis(scope_handler, **kwargs)

            match argument.value:
                case IdentifierAst(): sym = scope_handler.current_scope.get_symbol(argument.value)
                case PostfixExpressionAst() if isinstance(argument.value.op, PostfixExpressionOperatorMemberAccessAst):
                    temp = argument.value
                    while isinstance(temp, PostfixExpressionAst):
                        temp = temp.lhs
                    sym = scope_handler.current_scope.get_symbol(temp) if isinstance(temp, IdentifierAst) else None
                case _: sym = None

            # Check that an argument is initialized before being used: applies to (postfix) identifier only.
            if sym and sym.memory_info.ast_consumed:
                match sym.memory_info.ast_consumed:
                    case LetStatementUninitializedAst(): error_message = "declared as uninitialized here"
                    case _: error_message = "moved here"

                exception = SemanticError(f"Variable '{argument.value}' used before being initialized:")
                exception.add_traceback(sym.memory_info.ast_consumed.pos, f"Variable '{argument.value}' {error_message}.")
                exception.add_traceback(argument.value.pos, f"Variable '{argument.value}' used here.")
                raise exception

            # Check that an argument is not partially moved before being used: applies to (postfix) identifier only.
            # Non-overlapping partial moves are ok, for example, if "a.b" is moved, "a.c" is fine to use, but "a" isn't.
            if sym and sym.memory_info.ast_partial_moves:
                for existing_partial_move in sym.memory_info.ast_partial_moves:
                    if existing_partial_move == argument.value:
                        exception = SemanticError(f"Variable '{argument.value}' used after being moved:")
                        exception.add_traceback(existing_partial_move.pos, f"Variable '{argument.value}' moved here.")
                        exception.add_traceback(argument.value.pos, f"Variable '{argument.value}' used here.")
                        raise exception

                for existing_partial_move in sym.memory_info.ast_partial_moves:
                    existing_partial_move = str(existing_partial_move)
                    if existing_partial_move.startswith(str(argument.value)):
                        exception = SemanticError(f"Variable '{argument.value}' used after being partially moved:")

                        # TODO : if any partial moves are in the same place then consolidate them into one trace
                        for partial_move in sym.memory_info.ast_partial_moves:
                            exception.add_traceback(partial_move.pos, f"Variable '{partial_move}' partially moved here.")
                        exception.add_traceback(argument.value.pos, f"Variable '{argument.value}' used here.")
                        raise exception

            # Check conventions of arguments to enforce the law of exclusivity. Note that "&mut a", "&mut a.b" is an
            # overlap, but "&mut a.b", "&mut a.c" is not.
            match argument.convention:
                case ConventionMovAst() if sym:
                    # Mark the symbol as consumed, if the argument is a single identifier.
                    # TODO: remove initialization AST?
                    if isinstance(argument.value, IdentifierAst):
                        sym.memory_info.ast_consumed = argument.value

                    # Cannot move an identifier if is borrowed as a previous argument.
                    for i, existing_borrow in borrows_mut | borrows_ref:
                        existing_borrow_check = str(existing_borrow)
                        if existing_borrow_check.startswith(str(argument.value)):
                            exception = SemanticError(f"Cannot move an object '{argument.value}' that's already borrowed:")
                            exception.add_traceback(i, f"Object '{argument.value}' already borrowed here.")
                            exception.add_traceback(argument.convention.pos, f"Object '{argument.value}' attempted to be borrowed here.")
                            raise exception

                    # Cannot move from a borrowed context, so enforce this here too.
                    if sym.memory_info.is_borrow:
                        exception = SemanticError(f"Cannot move from a borrowed context:")
                        exception.add_traceback(sym.memory_info.ast_borrow.pos, f"Variable '{argument.value}' borrowed here.")
                        exception.add_traceback(argument.pos, f"Partial move '{argument}' attempted here.")
                        raise exception

                    # Otherwise, mark the left most identifier as partially moved.
                    else:
                        sym.memory_info.ast_partial_moves.append(argument.value)

                case ConventionMutAst():
                    # Can only take a borrow from a (postfix) identifier. TODO : remove this soon
                    if not sym:
                        exception = SemanticError(f"Cannot take an borrow from a non-identifier:")
                        exception.add_traceback(argument.convention.pos, f"Borrow '{argument.convention}' taken here.")
                        raise exception

                    # Can only take a mutable borrow from a mutable symbol
                    if not (sym.is_mutable or sym.memory_info.is_borrow_mut):
                        exception = SemanticError(f"Cannot take a mutable borrow from an immutable variable:")
                        exception.add_traceback(sym.memory_info.ast_initialized.pos, f"Variable '{argument.value}' declared immutably here.")
                        exception.add_traceback(argument.convention.pos, f"Mutable borrow '{argument.value}' taken here.")
                        raise exception

                    # Cannot take a mutable borrow from an immutable borrow
                    if sym.memory_info.is_borrow_ref:
                        exception = SemanticError(f"Cannot take a mutable borrow from an immutable borrow:")
                        exception.add_traceback(sym.memory_info.ast_borrow.pos, f"Variable '{argument.value}' borrowed immutably here.")
                        exception.add_traceback(argument.convention.pos, f"Mutable borrow '{argument.value}' taken here.")
                        raise exception

                    # For a mutable borrow to take place, ensure that no other overlapping part of the variable is
                    # already borrowed immutably or mutably.
                    for i, existing_borrow in borrows_ref | borrows_mut:
                        if existing_borrow.startswith(str(argument.value)):
                            exception = SemanticError(f"Cannot take a mutable borrow to an object '{argument.value}' that's already borrowed:")
                            exception.add_traceback(i, f"Object '{argument.value}' already borrowed here.")
                            exception.add_traceback(argument.convention.pos, f"Object '{argument.value}' attempted to ebe borrowed mutably here.")
                            raise exception

                    borrows_mut.add((argument.value.pos, str(argument.value)))

                case ConventionRefAst():
                    # Can only take a borrow from a (postfix) identifier. TODO : remove this soon
                    # if not sym:
                    #     exception = SemanticError(f"Cannot take a borrow from a non-identifier:")
                    #     exception.add_traceback(argument.convention.pos, f"Borrow '{argument.convention}' taken here.")
                    #     raise exception

                    # For an immutable borrow to take place, ensure that no other overlapping part of the variable is
                    # already borrowed mutably.
                    for i, existing_borrow in borrows_mut:
                        existing_borrow_check = str(existing_borrow)
                        if existing_borrow_check.startswith(str(argument.value)):
                            exception = SemanticError(f"Cannot take a immutable borrow to an object '{argument.value}' that's already mutably borrowed:")
                            exception.add_traceback(i, f"Object '{argument.value}' already mutably borrowed here.")
                            exception.add_traceback(argument.convention.pos, f"Object '{argument.value}' attempted to be borrowed immutably here.")
                            raise exception

                    borrows_ref.add((argument.value.pos, str(argument.value)))


@dataclass
class FunctionParameterSelfAst(Ast, SemanticAnalysis):
    is_mutable: Optional[TokenAst]
    convention: ConventionAst
    identifier: IdentifierAst
    type_declaration: TypeAst = dataclasses.field(default=None)

    def __post_init__(self):
        self.type_declaration = CommonTypes.self()

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        s = ""
        s += f"{self.is_mutable.print(printer)}" if self.is_mutable else ""
        s += f"{self.convention.print(printer)} {self.identifier.print(printer)}"
        return s

    def do_semantic_analysis(self, scope_handler, **kwargs) -> None:
        # Add the "self" symbol to the current scope. The memory status depends on the calling-convention used on the
        # "self" parameter, ie {"&" => immutable borrow, "&mut" => mutable borrow}.
        symbol = VariableSymbol(self.identifier, CommonTypes.self(), is_mutable=self.is_mutable is not None, memory_info=MemoryStatus(
            ast_initialized=self.identifier,
            is_borrow_ref=isinstance(self.convention, ConventionRefAst),
            is_borrow_mut=isinstance(self.convention, ConventionMutAst),
            ast_borrow=self.convention))
        scope_handler.current_scope.add_symbol(symbol)

    def __eq__(self, other):
        # Because all "self" parameters have the same name ("self"), the only check is that the 2 nodes are the same
        # type (FunctionParameterSelfAst).
        return isinstance(other, FunctionParameterSelfAst)


@dataclass
class FunctionParameterRequiredAst(Ast, SemanticAnalysis):
    is_mutable: Optional[TokenAst]
    identifier: IdentifierAst
    colon_token: TokenAst
    convention: ConventionAst
    type_declaration: TypeAst

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        s = ""
        s += f"{self.is_mutable.print(printer)}" if self.is_mutable else ""
        s += f"{self.identifier.print(printer)}{self.colon_token.print(printer)} {self.convention.print(printer)}{self.type_declaration.print(printer)}"
        return s

    def do_semantic_analysis(self, scope_handler, **kwargs) -> None:
        # Analyse the parameter type.
        self.type_declaration.do_semantic_analysis(scope_handler, **kwargs)

        # Add the symbol for the parameter. The memory status depends on the calling-convention used.
        symbol = VariableSymbol(self.identifier, self.type_declaration, is_mutable=self.is_mutable is not None, memory_info=MemoryStatus(
            ast_initialized=self.identifier,
            is_borrow_ref=isinstance(self.convention, ConventionRefAst),
            is_borrow_mut=isinstance(self.convention, ConventionMutAst),
            ast_borrow=self.convention))
        scope_handler.current_scope.add_symbol(symbol)

    def __eq__(self, other):
        # Equality is determined by the parameter's identifier.
        return isinstance(other, FunctionParameterRequiredAst) and self.identifier == other.identifier


@dataclass
class FunctionParameterOptionalAst(Ast, SemanticAnalysis):
    is_mutable: Optional[TokenAst]
    identifier: IdentifierAst
    colon_token: TokenAst
    convention: ConventionAst
    type_declaration: TypeAst
    assignment_token: TokenAst
    default_value: ExpressionAst

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        s = ""
        s += f"{self.is_mutable.print(printer)}" if self.is_mutable else ""
        s += f"{self.identifier.print(printer)}{self.colon_token.print(printer)} {self.convention.print(printer)}{self.type_declaration.print(printer)} {self.assignment_token.print(printer)} {self.default_value.print(printer)}"
        return s

    def do_semantic_analysis(self, scope_handler, **kwargs) -> None:
        # Analyse the parameter type.
        self.type_declaration.do_semantic_analysis(scope_handler, **kwargs)

        # Add the symbol for the parameter. The memory status depends on the calling-convention used.
        symbol = VariableSymbol(self.identifier, self.type_declaration, is_mutable=self.is_mutable is not None, memory_info=MemoryStatus(
            ast_initialized=self.identifier,
            is_borrow_ref=isinstance(self.convention, ConventionRefAst),
            is_borrow_mut=isinstance(self.convention, ConventionMutAst),
            ast_borrow=self.convention))
        scope_handler.current_scope.add_symbol(symbol)

        # Check the convention of the parameter is by-mov, because default values will always be "owned", so the type
        # must use the "move" convention.
        if not isinstance(self.convention, ConventionMovAst):
            exception = SemanticError(f"Optional parameters must use the by-val convention:")
            exception.add_traceback(self.convention.pos, f"Convention '{self.convention}' used here.")
            raise exception

        # Analyse the default value to ensure it is valid. This happens before the type-check against the parameter#
        # type, as otherwise there would be an error in detecting the type of an invalid expression.
        self.default_value.do_semantic_analysis(scope_handler, **kwargs)

        # Check the default value's type matches the parameter's type.
        if not self.type_declaration.symbolic_eq((default_value_type := self.default_value.infer_type(scope_handler, **kwargs))[1], scope_handler.current_scope):
            exception = SemanticError(f"Optional parameter type does not match default value type:")
            exception.add_traceback(self.type_declaration.pos, f"Parameter type '{self.type_declaration}' declared here.")
            exception.add_traceback(self.default_value.pos, f"Default value type '{default_value_type[1]}' inferred here.")
            raise exception

    def __eq__(self, other):
        # Equality is determined by the parameter's identifier.
        return isinstance(other, FunctionParameterOptionalAst) and self.identifier == other.identifier


@dataclass
class FunctionParameterVariadicAst(Ast, SemanticAnalysis):
    is_mutable: Optional[TokenAst]
    variadic_token: TokenAst
    identifier: IdentifierAst
    colon_token: TokenAst
    convention: ConventionAst
    type_declaration: TypeAst

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        s = ""
        s += f"{self.is_mutable.print(printer)}" if self.is_mutable else ""
        s += f"{self.variadic_token.print(printer)}{self.identifier.print(printer)}{self.colon_token.print(printer)} {self.convention.print(printer)}{self.type_declaration.print(printer)}"
        return s

    def do_semantic_analysis(self, scope_handler, **kwargs) -> None:
        # TODO : type declaration for variadics will need to be checked later: tuple?
        # TODO

        self.type_declaration.do_semantic_analysis(scope_handler, **kwargs)
        symbol = VariableSymbol(self.identifier, self.type_declaration, is_mutable=self.is_mutable is not None, memory_info=MemoryStatus(
            ast_initialized=self.identifier,
            is_borrow_ref=isinstance(self.convention, ConventionRefAst),
            is_borrow_mut=isinstance(self.convention, ConventionMutAst),
            ast_borrow=self.convention))
        scope_handler.current_scope.add_symbol(symbol)

    def __eq__(self, other):
        # Equality is determined by the parameter's identifier.
        return isinstance(other, FunctionParameterVariadicAst) and self.identifier == other.identifier


type FunctionParameterAst = (
        FunctionParameterSelfAst |
        FunctionParameterRequiredAst |
        FunctionParameterOptionalAst |
        FunctionParameterVariadicAst)


@dataclass
class FunctionParameterGroupAst(Ast, SemanticAnalysis):
    paren_l_token: TokenAst
    parameters: List[FunctionParameterAst]
    paren_r_token: TokenAst

    def print(self, printer: AstPrinter) -> str:
        return f"{self.paren_l_token.print(printer)}{Seq(self.parameters).print(printer, ", ")}{self.paren_r_token.print(printer)}"

    def do_semantic_analysis(self, scope_handler: ScopeHandler, **kwargs) -> None:
        # Check no parameters have the same name
        if Seq(self.parameters).map(lambda p: p.identifier).contains_duplicates():
            duplicate_parameters = Seq(self.parameters).map(lambda p: p.identifier).non_unique_items()[0]
            exception = SemanticError(f"Duplicate parameters '{duplicate_parameters[0]}' found in function prototype:")
            exception.add_traceback(duplicate_parameters[0].pos, f"Parameter '{duplicate_parameters[0]}' declared here.")
            exception.add_traceback(duplicate_parameters[1].pos, f"Parameter '{duplicate_parameters[1]}' re-declared here.")
            raise exception

        # Check parameter order is Self -> Required -> Optional -> Variadic
        ordering = {FunctionParameterSelfAst: "Self", FunctionParameterRequiredAst: "Required", FunctionParameterOptionalAst: "Optional", FunctionParameterVariadicAst: "Variadic"}
        current_classifications = Seq(self.parameters).map(lambda p: (type(p), p))
        sorted_classifications = current_classifications.sort(key=lambda t: list(ordering.keys()).index(t[0]))
        if current_classifications != sorted_classifications:
            difference = sorted_classifications.ordered_difference(current_classifications)
            exception = SemanticError(f"Invalid parameter order in function prototype:")
            exception.add_traceback(difference[-2][1].pos, f"{ordering[difference[-2][0]]} parameter '{difference[-2][1]}' declared here.")
            exception.add_traceback(difference[-1][1].pos, f"{ordering[difference[-1][0]]} parameter '{difference[-1][1]}' declared here.")
            raise exception

        # Check that the function is class method, not in the module global space, if there is a "self" parameter
        if self.parameters and isinstance(self.parameters[0], FunctionParameterSelfAst) and scope_handler.at_global_scope(parent_level=2):
            exception = SemanticError(f"Can only use the 'self' parameter within a class:")
            exception.add_traceback(self.parameters[0].pos, f"Parameter '{self.parameters[0]}' declared here.")
            raise exception

        # Check that there is a maximum of 1 variadic parameter
        variadic_parameters = Seq(self.parameters).filter(lambda p: isinstance(p, FunctionParameterVariadicAst))
        if variadic_parameters.length > 1:
            exception = SemanticError(f"Invalid parameter order in function prototype:")
            exception.add_traceback(variadic_parameters[0].pos, f"1st variadic parameter '{variadic_parameters[0]}' declared here.")
            exception.add_traceback(variadic_parameters[1].pos, f"2nd variadic parameter '{variadic_parameters[1]}' declared here.")
            raise exception

        Seq(self.parameters).for_each(lambda p: p.do_semantic_analysis(scope_handler, **kwargs))

    def get_self(self) -> Optional[FunctionParameterSelfAst]:
        # Convenience method to get the "self" function parameter (if it exists).
        return Seq(self.parameters).filter(lambda p: isinstance(p, FunctionParameterSelfAst)).first(None)

    def get_req(self) -> List[FunctionParameterRequiredAst]:
        # Convenience method to get all the required function parameters.
        return Seq(self.parameters).filter(lambda p: isinstance(p, FunctionParameterRequiredAst)).value

    def get_opt(self) -> List[FunctionParameterOptionalAst]:
        # Convenience method to get all the optional function parameters.
        return Seq(self.parameters).filter(lambda p: isinstance(p, FunctionParameterOptionalAst)).value

    def get_var(self) -> Optional[FunctionParameterVariadicAst]:
        # Convenience method to get the variadic function parameter (if it exists).
        return Seq(self.parameters).filter(lambda p: isinstance(p, FunctionParameterVariadicAst)).first(None)


@dataclass
class FunctionPrototypeAst(Ast, PreProcessor, SymbolGenerator, SemanticAnalysis):
    annotations: List[AnnotationAst]
    fun_token: TokenAst
    identifier: IdentifierAst
    generic_parameters: Optional[GenericParameterGroupAst]
    parameters: FunctionParameterGroupAst
    arrow_token: TokenAst
    return_type: TypeAst
    where_block: Optional[WhereBlockAst]
    body: InnerScopeAst[StatementAst]
    _fn_type: TypeAst = dataclasses.field(default=None)
    _orig: IdentifierAst = dataclasses.field(default=None)
    _ctx: ModulePrototypeAst | SupPrototypeAst = dataclasses.field(default=None)
    _specializations: List[FunctionPrototypeAst] = dataclasses.field(default_factory=list)

    def __post_init__(self):
        self.generic_parameters = self.generic_parameters or GenericParameterGroupAst(-1, TokenAst.dummy(TokenType.TkBrackL), [], TokenAst.dummy(TokenType.TkBrackR))
        self.where_block = self.where_block or WhereBlockAst(-1, TokenAst.dummy(TokenType.KwWhere), WhereConstraintsGroupAst(-1, TokenAst.dummy(TokenType.TkBrackL), [], TokenAst.dummy(TokenType.TkBrackR)))

    def __eq__(self, other):
        return self.identifier == other.identifier and self.generic_parameters == other.generic_parameters and self.parameters == other.parameters and self.return_type == other.return_type and self.where_block == other.where_block

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        s = ""
        s += f"{Seq(self.annotations).print(printer, "\n")}{self.fun_token.print(printer)}{self.identifier.print(printer)}"
        s += f"{self.generic_parameters.print(printer)}" if self.generic_parameters else ""
        s += f"{self.parameters.print(printer)} {self.arrow_token.print(printer)} {self.return_type.print(printer)}"
        s += f" {self.where_block.print(printer)}" if self.where_block else ""
        s += f"{self.body.print(printer)}"
        return s

    def pre_process(self, context: ModulePrototypeAst | SupPrototypeAst) -> None:
        self._ctx = context

        # For functions that are methods (ie inside a "sup" block), substitute the "Self" type from generic parameters,
        # function parameters, and the return type.
        if not isinstance(context, ModulePrototypeAst):
            Seq(self.generic_parameters.get_opt()).for_each(lambda p: p.default_value.substitute_generics(CommonTypes.self(), context.identifier))
            Seq(self.parameters.parameters).for_each(lambda p: p.type_declaration.substitute_generics(CommonTypes.self(), context.identifier))
            self.return_type.substitute_generics(CommonTypes.self(), context.identifier)

        # Convert the "fun ..." to a "Fun___" superimposition over a type representing the function class. This allows
        # for the first class nature of functions. The mock object for "fun function" will be "MOCK_function".
        mock_class_name = IdentifierAst(-1, f"MOCK_{self.identifier.value}")
        mock_class_name = TypeSingleAst(-1, [GenericIdentifierAst(-1, mock_class_name.value, None)])

        # Determine the class type and call name. This will be "FunRef/call_ref", "FunMut/call_mut" or "FunMov/call_mov"
        function_class_type = self._deduce_function_class_type(context)
        function_call_name  = self._deduce_function_call_name(function_class_type)

        # If the mock class name ("MOCK_function") doesn't exist as a class, then this if the first instance of a
        # function with this name seen. Therefore, the class needs to be added into the module prototype.
        if Seq(context.body.members).filter(lambda m: isinstance(m, ClassPrototypeAst) and m.identifier == mock_class_name).empty():

            # Create the mock class prototype.
            # cls MOCK_function {}
            mock_class_ast = ClassPrototypeAst(
                pos=-1,
                annotations=[],
                class_token=TokenAst.dummy(TokenType.KwCls),
                identifier=mock_class_name,
                generic_parameters=None,
                where_block=None,
                body=InnerScopeAst(
                    pos=-1,
                    brace_l_token=TokenAst.dummy(TokenType.TkBraceL),
                    members=[],
                    brace_r_token=TokenAst.dummy(TokenType.TkBraceR)))

            # Create the let statement, which brings an instance of this class into scope.
            # let function = MOCK_function()
            mock_let_statement = LetStatementInitializedAst(
                pos=-1,
                let_keyword=TokenAst.dummy(TokenType.KwLet),
                assign_to=LocalVariableSingleAst(
                    pos=-1,
                    is_mutable=None,
                    unpack_token=None,
                    identifier=copy.deepcopy(self.identifier)),
                assign_token=TokenAst.dummy(TokenType.TkAssign),
                value=ObjectInitializerAst(
                    pos=-1,
                    class_type=copy.deepcopy(mock_class_name),
                    arguments=ObjectInitializerArgumentGroupAst(
                        pos=-1,
                        paren_l_token=TokenAst.dummy(TokenType.TkParenL),
                        arguments=[],
                        paren_r_token=TokenAst.dummy(TokenType.TkParenR))),
                _sup_let_type=function_class_type)

            # Append both of these ASTs to the module or sup prototype ("context" will be either one).
            context.body.members.append(mock_class_ast)
            context.body.members.append(mock_let_statement)

        # At this point, either the class existed, or it exists now, so super-impose the "Fun___" type onto it. Create
        # the call function, like "call_ref", and carry through the generic parameters, function parameters,
        # return type, etc.
        call_method_ast = SupMethodPrototypeAst(
            pos=self.pos,
            annotations=[],
            fun_token=TokenAst.dummy(TokenType.KwFun),
            identifier=function_call_name,
            generic_parameters=self.generic_parameters,
            parameters=self.parameters,
            arrow_token=TokenAst.dummy(TokenType.TkArrowR),
            return_type=self.return_type,
            where_block=None,
            body=self.body,
            _orig=self.identifier,
            _ctx=self._ctx)

        # Create the superimposition block over the class type, which includes the "call_ref" function as a member. This
        # will allow for the type to now be callable with the parameter types and return type specified.
        sup_block_ast = SupPrototypeInheritanceAst(
            pos=-1,
            sup_keyword=TokenAst.dummy(TokenType.KwSup),
            generic_parameters=self.generic_parameters,
            super_class=copy.deepcopy(function_class_type),
            on_keyword=TokenAst.dummy(TokenType.KwOn),
            identifier=mock_class_name,
            where_block=self.where_block,
            body=InnerScopeAst(
                pos=-1,
                brace_l_token=TokenAst.dummy(TokenType.TkBraceL),
                members=[call_method_ast],
                brace_r_token=TokenAst.dummy(TokenType.TkBraceR)))

        # Append the "sup" block to the module or sup prototype ("context" will be either one).
        context.body.members.append(sup_block_ast)
        self._fn_type = function_class_type

    def _deduce_function_class_type(self, context: ModulePrototypeAst | SupPrototypeAst) -> TypeAst:
        # Deducing the function call type requires knowledge of the "self" parameter. If there is a "self" parameter,
        # then use the convention to determine the function class type. If there isn't, then the function is either a
        # free function (module scope), or a static class method. In either case, it will be "FunRef".
        is_method = not isinstance(context, ModulePrototypeAst)
        has_self_parameter = self.parameters.parameters and isinstance(self.parameters.parameters[0], FunctionParameterSelfAst)

        # Get the parameter types and return type, to move into the function class type being created.
        parameter_types = Seq(self.parameters.parameters).map(lambda p: p.type_declaration).value
        return_type = self.return_type

        # If the method is a non-static class method, then base the function type off the "self" parameter's convention.
        if is_method and has_self_parameter:
            convention = self.parameters.get_self().convention
            match convention:
                case ConventionRefAst(): return CommonTypes.fun_ref(return_type, parameter_types, pos=self.pos)
                case ConventionMutAst(): return CommonTypes.fun_mut(return_type, parameter_types, pos=self.pos)
                case ConventionMovAst(): return CommonTypes.fun_mov(return_type, parameter_types, pos=self.pos)
                case _: raise SystemExit(f"Unknown convention '{convention}' being deduced. Report as bug.")

        # Otherwise, the function is either free or a static method, so the function class type will be "FunRef".
        else:
            return CommonTypes.fun_ref(return_type, parameter_types, pos=self.pos)

    def _deduce_function_call_name(self, function_class_type: TypeAst) -> IdentifierAst:
        # Map the function class type to a function call name with a simple match-case statement.
        match function_class_type.parts[-1].value:
            case "FunRef": return IdentifierAst(self.identifier.pos, "call_ref")
            case "FunMut": return IdentifierAst(self.identifier.pos, "call_mut")
            case "FunMov": return IdentifierAst(self.identifier.pos, "call_mov")
            case _: raise SystemExit(f"Unknown function class type '{function_class_type}' being deduced. Report as bug.")

    def generate(self, s: ScopeHandler) -> None:
        # Create and move into a new scope for the function prototype's scope. Within this scope, generate type symbols
        # for each generic parameter. Exit the newly created function scope.
        s.into_new_scope(self.identifier)
        Seq(self.generic_parameters.parameters).for_each(lambda p: s.current_scope.add_symbol(TypeSymbol(p.identifier, None)))
        s.exit_cur_scope()

    def do_semantic_analysis(self, scope_handler, override_scope: bool = False, **kwargs) -> None:
        # Move into the function scope.
        if not override_scope:
            scope_handler.move_to_next_scope()

        # Analyse each part of the function: the annotations, generic parameters, parameters, return type, where block,
        # and body.
        Seq(self.annotations).for_each(lambda a: a.do_semantic_analysis(scope_handler, **kwargs))
        self.generic_parameters.do_semantic_analysis(scope_handler, **kwargs)
        self.parameters.do_semantic_analysis(scope_handler, **kwargs)
        self.return_type.do_semantic_analysis(scope_handler, **kwargs)
        # self.where_block.do_semantic_analysis(s)

        # Add the "target-return-type" to the function block, so that nested return statements can be type-checked. Set
        # "inline-block" to True, because a new scope has already been created for the function scope, so the InnerScope
        # analyser doesn't need to create a new scope automatically.
        kwargs |= {"target-return-type": self.return_type}
        self.body.do_semantic_analysis(scope_handler, inline_block=True, **kwargs)
        kwargs.pop("target-return-type")

        # Check that there a return statement at the end for non-Void subroutines.
        if ("coroutine" not in kwargs
                and not self.return_type.symbolic_eq(CommonTypes.void(), scope_handler.current_scope)
                and self.body.members
                and not isinstance(self.body.members[-1], ReturnStatementAst)):
            exception = SemanticError(f"Missing return statement in non-Void function:")
            exception.add_traceback(self.pos, f"Function '{self.identifier}' declared here.")
            exception.add_traceback(self.body.members[-1].pos, f"Last statement '{self.body.members[-1]}' found here.")
            raise exception

        # Check if the function is a coroutine (contains 1 yield statement, can be nested in some inner scope too)
        if "coroutine" in kwargs:
            ...

        # Exit the function scope.
        if not override_scope:
            scope_handler.exit_cur_scope()


@dataclass
class GenericArgumentNormalAst(Ast, SemanticAnalysis):
    type: TypeAst

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        return f"{self.type.print(printer)}"

    def do_semantic_analysis(self, scope_handler: ScopeHandler, **kwargs) -> None:
        # Analyse the type of the generic argument.
        self.type.do_semantic_analysis(scope_handler, **kwargs)

    def __eq__(self, other):
        return isinstance(other, GenericArgumentNormalAst) and self.type == other.type


@dataclass
class GenericArgumentNamedAst(GenericArgumentNormalAst, SemanticAnalysis):
    identifier: TypeAst
    assignment_token: TokenAst

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        return f"{self.identifier.print(printer)}{self.assignment_token.print(printer)}{self.type.print(printer)}"

    def do_semantic_analysis(self, scope_handler: ScopeHandler, **kwargs) -> None:
        # Analyse the type of the generic argument.
        super().do_semantic_analysis(scope_handler, **kwargs)
        self.type.do_semantic_analysis(scope_handler, **kwargs)

    def __eq__(self, other):
        return isinstance(other, GenericArgumentNamedAst) and self.identifier == other.identifier and self.type == other.type


type GenericArgumentAst = (
        GenericArgumentNormalAst |
        GenericArgumentNamedAst)


@dataclass
class GenericArgumentGroupAst(Ast, SemanticAnalysis):
    bracket_l_token: TokenAst
    arguments: List[GenericArgumentAst]
    bracket_r_token: TokenAst

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        s = ""
        s += f"{self.bracket_l_token.print(printer)}{Seq(self.arguments).print(printer, ", ")}{self.bracket_r_token.print(printer)}" if self.arguments else ""
        return s

    def __eq__(self, other):
        return isinstance(other, GenericArgumentGroupAst) and self.arguments == other.arguments

    def do_semantic_analysis(self, scope_handler: ScopeHandler, **kwargs) -> None:
        # Check no named arguments have the same name
        named_arguments = Seq(self.arguments).filter(lambda a: isinstance(a, GenericArgumentNamedAst)).map(lambda a: a.identifier)
        if named_arguments.contains_duplicates():
            duplicate_named_arguments = named_arguments.non_unique_items()[0]
            exception = SemanticError(f"Duplicate generic argument names '{duplicate_named_arguments[0]}' found:")
            exception.add_traceback(duplicate_named_arguments[0].pos, f"Argument <{duplicate_named_arguments[0]}> declared here.")
            exception.add_traceback(duplicate_named_arguments[1].pos, f"Argument <{duplicate_named_arguments[1]}> re-declared here.")
            raise exception

        # Check argument order is Normal -> Named
        ordering = {GenericArgumentNormalAst: "Normal", GenericArgumentNamedAst: "Named"}
        current_classifications = Seq(self.arguments).map(lambda p: (type(p), p))
        sorted_classifications = current_classifications.sort(key=lambda t: list(ordering.keys()).index(t[0]))
        if current_classifications != sorted_classifications:
            difference = sorted_classifications.ordered_difference(current_classifications)
            exception = SemanticError(f"Invalid generic argument order:")
            exception.add_traceback(difference[-2][1].pos, f"{ordering[difference[-2][0]]} argument '{difference[-2][1]}' declared here.")
            exception.add_traceback(difference[-1][1].pos, f"{ordering[difference[-1][0]]} argument '{difference[-1][1]}' declared here.")
            raise exception

        # Analyse each argument.
        Seq(self.arguments).for_each(lambda p: p.do_semantic_analysis(scope_handler, **kwargs))


@dataclass
class GenericIdentifierAst(Ast):
    value: str
    generic_arguments: Optional[GenericArgumentGroupAst]

    def __post_init__(self):
        self.generic_arguments = self.generic_arguments or GenericArgumentGroupAst(-1, TokenAst.dummy(TokenType.TkBrackL), [], TokenAst.dummy(TokenType.TkBrackR))

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        s = ""
        s += f"{self.value}"
        s += f"{self.generic_arguments.print(printer)}" if self.generic_arguments else ""
        return s

    def __eq__(self, other):
        return isinstance(other, GenericIdentifierAst) and self.value == other.value and self.generic_arguments == other.generic_arguments


@dataclass
class GenericParameterRequiredAst(Ast, SemanticAnalysis):
    identifier: TypeAst
    inline_constraints: Optional[GenericParameterInlineConstraintAst]

    def __post_init__(self):
        self.identifier = TypeSingleAst(self.identifier.pos, [GenericIdentifierAst(self.identifier.pos, self.identifier.value, None)])

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        s = ""
        s += f"{self.identifier.print(printer)}"
        s += f"{self.inline_constraints.print(printer)}" if self.inline_constraints else ""
        return s

    def do_semantic_analysis(self, scope_handler: ScopeHandler, **kwargs) -> None:
        ...


@dataclass
class GenericParameterOptionalAst(GenericParameterRequiredAst):
    assignment_token: TokenAst
    default_value: TypeAst

    def __post_init__(self):
        pass

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        return f"{super().print(printer)} {self.assignment_token.print(printer)} {self.default_value.print(printer)}"


@dataclass
class GenericParameterVariadicAst(GenericParameterRequiredAst):
    variadic_token: TokenAst

    def __post_init__(self):
        pass

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        return f"{self.variadic_token.print(printer)}{super().print(printer)}"


type GenericParameterAst = (
        GenericParameterRequiredAst |
        GenericParameterOptionalAst |
        GenericParameterVariadicAst)


@dataclass
class GenericParameterGroupAst(Ast, SemanticAnalysis):
    bracket_l_token: TokenAst
    parameters: List[GenericParameterAst]
    bracket_r_token: TokenAst

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        s = ""
        s += f"{self.bracket_l_token.print(printer)}{Seq(self.parameters).print(printer, ", ")}{self.bracket_r_token.print(printer)}" if self.parameters else ""
        return s

    def get_req(self) -> List[GenericParameterRequiredAst]:
        return [p for p in self.parameters if isinstance(p, GenericParameterRequiredAst)]

    def get_opt(self) -> List[GenericParameterOptionalAst]:
        return [p for p in self.parameters if isinstance(p, GenericParameterOptionalAst)]

    def get_var(self) -> List[GenericParameterVariadicAst]:
        return [p for p in self.parameters if isinstance(p, GenericParameterVariadicAst)]

    def do_semantic_analysis(self, scope_handler: ScopeHandler, **kwargs) -> None:
        # print(f"analysing generic parameter group: {self}")

        # Check no parameters have the same name
        if Seq(self.parameters).map(lambda p: p.identifier).contains_duplicates():
            duplicate_parameters = Seq(self.parameters).map(lambda p: p.identifier).non_unique_items()[0]
            exception = SemanticError(f"Duplicate parameters '{duplicate_parameters[0]}' found in function prototype:")
            exception.add_traceback(duplicate_parameters[0].pos, f"Parameter '{duplicate_parameters[0]}' declared here.")
            exception.add_traceback(duplicate_parameters[1].pos, f"Parameter '{duplicate_parameters[1]}' re-declared here.")
            raise exception

        # Add each parameter to the scope
        for generic_parameter in self.parameters:
            scope_handler.current_scope.add_symbol(TypeSymbol(generic_parameter.identifier, None))

        # Check parameter order is Self -> Required -> Optional -> Variadic
        ordering = {GenericParameterRequiredAst: "Required", GenericParameterOptionalAst: "Optional", GenericParameterVariadicAst: "Variadic"}
        current_classifications = Seq(self.parameters).map(lambda p: (type(p), p))
        sorted_classifications = current_classifications.sort(key=lambda t: list(ordering.keys()).index(t[0]))
        if current_classifications != sorted_classifications:
            difference = sorted_classifications.ordered_difference(current_classifications)
            exception = SemanticError(f"Invalid generic parameter order:")
            exception.add_traceback(difference[-2][1].pos, f"{ordering[difference[-2][0]]} generic parameter '{difference[-2][1]}' declared here.")
            exception.add_traceback(difference[-1][1].pos, f"{ordering[difference[-1][0]]} generic parameter '{difference[-1][1]}' declared here.")
            raise exception

        Seq(self.parameters).for_each(lambda p: p.do_semantic_analysis(scope_handler, **kwargs))


@dataclass
class GenericParameterInlineConstraintAst(Ast):
    colon_token: TokenAst
    constraints: List[TypeAst]

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        return f"{self.colon_token.print(printer)} {Seq(self.constraints).print(printer, ", ")}"


@dataclass
class IdentifierAst(Ast, SemanticAnalysis, TypeInfer):
    value: str

    def print(self, printer: AstPrinter) -> str:
        return f"{self.value}"

    def do_semantic_analysis(self, scope_handler: ScopeHandler, **kwargs) -> None:
        if not scope_handler.current_scope.has_symbol(self):
            all_symbols = Seq(scope_handler.current_scope.all_symbols()).filter(lambda s: isinstance(s, VariableSymbol))
            closest_match = difflib.get_close_matches(self.value, all_symbols.map(lambda s: s.name.value).value, n=1)
            closest_match = f" Did you mean '{closest_match[0]}'?" if closest_match else ""

            exception = SemanticError(f"Undefined identifier '{self.value}':")
            exception.add_traceback(self.pos, f"Identifier '{self.value}' used here.{closest_match}")
            raise exception

        scope_handler.current_scope.get_symbol(self).type.do_semantic_analysis(scope_handler, **kwargs)  # ?

    def infer_type(self, scope_handler: ScopeHandler, **kwargs) -> Tuple[Type[ConventionAst], TypeAst]:
        sym = scope_handler.current_scope.get_symbol(self)

        if sym.memory_info.ast_consumed:
            convention = ConventionNonInitAst
        elif sym.memory_info.ast_partial_moves:
            convention = ConventionPartInitAst
        elif sym.memory_info.is_borrow_mut:
            convention = ConventionMutAst
        elif sym.memory_info.is_borrow_ref:
            convention = ConventionRefAst
        else:
            convention = ConventionMovAst

        return convention, sym.type

    def __eq__(self, other):
        return isinstance(other, IdentifierAst) and self.value == other.value

    def __hash__(self):
        return int.from_bytes(hashlib.md5(self.value.encode()).digest())

    def __radd__(self, other):
        if isinstance(other, str):
            return IdentifierAst(pos=self.pos, value=other + self.value)

    def __json__(self) -> str:
        return self.value


@dataclass
class IfExpressionAst(Ast, SemanticAnalysis, TypeInfer):
    if_keyword: TokenAst
    condition: ExpressionAst
    comp_operator: TokenAst
    then_keyword: TokenAst
    branches: List[PatternBlockAst]

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        s = ""
        s += f"{self.if_keyword.print(printer)}{self.condition.print(printer)}"
        s += f" {self.comp_operator.print(printer)}" if self.comp_operator else ""
        s += f" {self.then_keyword.print(printer)}\n{Seq(self.branches).print(printer, "\n")}"
        return s

    def do_semantic_analysis(self, scope_handler: ScopeHandler, **kwargs) -> None:
        # Move into a new scope for the IfExpressionAst. Whilst no symbols will be stored in this scope specifically,
        # this scope holds each scope created for the pattern blocks. This keeps it cleaner rather than having all the
        # pattern blocks in the parent scope to this if expression.
        scope_handler.into_new_scope(f"<if-expression:{self.condition}>")

        # Analyse the condition and then each pattern branch
        self.condition.do_semantic_analysis(scope_handler, **kwargs)

        # Check that the branches don't have comparison operators if the if-expression does. This is because the if
        # expression's partial fragment cannot conflict with the branches partial fragment.
        for branch in self.branches:
            branch.do_semantic_analysis(scope_handler, if_condition=self.condition, **kwargs)

            if self.comp_operator and branch.comp_operator:
                exception = SemanticError(f"Comparison operators [{self.comp_operator}, {branch.comp_operator}] found in case-expression and pattern block:")
                exception.add_traceback(self.comp_operator.pos, f"1st Comparison operator '{self.comp_operator}' found here.")
                exception.add_traceback(branch.comp_operator.pos, f"2nd Comparison operator '{branch.comp_operator}' found here.")
                raise exception

            if not self.comp_operator and not branch.comp_operator and not isinstance(branch.patterns[0], PatternVariantElseAst):
                exception = SemanticError(f"No comparison operator found in case-expression or pattern block:")
                exception.add_traceback(self.condition.pos, f"Case-expression declared here with no operator")
                exception.add_traceback(branch.body.brace_l_token.pos, f"Branch's pattern block declared here with no operator")
                raise exception

        # Check that the combination of the case expression and the branch expressions can be combined successfully ie
        # does the complete function exist?
        # for branch in self.branches:
        #     for pattern in branch.patterns:
        #         if isinstance(pattern, PatternVariantElseAst): continue

                # complete_pattern = BinaryExpressionAst(
                #     pos=(self.comp_operator or branch.comp_operator).pos,
                #     lhs=self.condition,
                #     op=(self.comp_operator or branch.comp_operator),
                #     rhs=pattern)
                # TODO: complete_pattern.do_semantic_analysis(scope_handler, **kwargs)
                # TODO: the "rhs" argument is wrong. maybe make a dummy variable of that type?
                # TODO: beneath check too

                # Overriding the comparison classes forces a Bool return type. This check is for the future when member
                # access is implemented too.
                # if (pattern_type := complete_pattern.infer_type(scope_handler, **kwargs))[1] != CommonTypes.bool():
                #     exception = SemanticError(f"Comparisons must evaluate to a boolean expression")
                #     exception.add_traceback(complete_pattern.pos, f"Comparison evaluated here with type '{pattern_type[0].default()}{pattern_type[1]}'")
                #     raise exception

        # If this "if" expression is being used for assignment, then check that all branches have the same returning
        # type (final statement of the pattern block).
        if kwargs.get("assignment", False):
            if Seq(self.branches).map(lambda b: b.body.members[-1].infer_type(scope_handler, **kwargs)).unique_items().length > 1:
                conflicting_types = Seq(self.branches).map(lambda b: b.body.members[-1].infer_type(scope_handler, **kwargs)).unique_items()
                exception = SemanticError(f"Conflicting types found in if-expression being used for assignment:")
                exception.add_traceback(conflicting_types[0][1].pos, f"Type '{conflicting_types[0][0].default()}{conflicting_types[0][1]}' inferred here.")  # TODO : wrong.pos
                exception.add_traceback(conflicting_types[1][1].pos, f"Type '{conflicting_types[1][0].default()}{conflicting_types[1][1]}' inferred here.")  # TODO : wrong.pos
                raise exception

            # Assignment from an "if" expression require an else branch to be present.
            if not self.branches[-1].is_else_branch():
                exception = SemanticError(f"Missing else branch in assign-if-expression:")
                exception.add_traceback(self.pos, f"If-expression declared here.")
                exception.add_traceback(self.branches[-1].pos, f"Last pattern block found here.")
                raise exception

        scope_handler.exit_cur_scope()

    def infer_type(self, scope_handler, **kwargs) -> Tuple[Type[ConventionAst], TypeAst]:
        if self.branches and self.branches[0].body.members:
            return self.branches[0].body.members[-1].infer_type(scope_handler, **kwargs)
        return ConventionMovAst, CommonTypes.void()


@dataclass
class InnerScopeAst[T](Ast, SemanticAnalysis, TypeInfer):
    brace_l_token: TokenAst
    members: List[T]
    brace_r_token: TokenAst

    @ast_printer_method_indent
    def print(self, printer: AstPrinter) -> str:
        s = ""
        s += f"{self.brace_l_token.print(printer)}"
        s += f"\n{Seq(self.members).print(printer, "\n")}\n" if self.members else ""
        s += f"{self.brace_r_token.print(printer)}"
        return s

    def do_semantic_analysis(self, scope_handler, inline_block: bool = False, **kwargs) -> None:
        # Case where a new scope is not wanted, ie keep inline with the current scope. This is NOT the default.
        if inline_block:
            Seq(self.members).for_each(lambda m: m.do_semantic_analysis(scope_handler, **kwargs))

        # Case where a new scope is wanted, ie create a new scope and move into it. This IS the default.
        else:
            scope_handler.into_new_scope("<inner-scope>")
            Seq(self.members).for_each(lambda m: m.do_semantic_analysis(scope_handler, **kwargs))
            scope_handler.exit_cur_scope()

    def infer_type(self, scope_handler: ScopeHandler, **kwargs) -> Tuple[Type[ConventionAst], TypeAst]:
        return self.members[-1].infer_type(scope_handler, **kwargs) if self.members else (ConventionMovAst, CommonTypes.void())


@dataclass
class LambdaCaptureBlockAst(Ast):
    with_keyword: TokenAst
    bracket_l_token: TokenAst
    items: List[LambdaCaptureItemAst]
    bracket_r_token: TokenAst

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        return f"{self.with_keyword.print(printer)}{self.bracket_l_token.print(printer)}{Seq(self.items).print(printer, ", ")}{self.bracket_r_token.print(printer)}"


@dataclass
class LambdaCaptureItemNormalAst(Ast):
    convention: ConventionAst
    value: IdentifierAst

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        return f"{self.convention.print(printer)}{self.value.print(printer)}"


@dataclass
class LambdaCaptureItemNamedAst(Ast):
    identifier: IdentifierAst
    assignment_token: TokenAst
    convention: ConventionAst
    value: IdentifierAst

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        return f"{self.identifier.print(printer)}{self.assignment_token.print(printer)}{self.convention.print(printer)}{self.value.print(printer)}"


type LambdaCaptureItemAst = (
        LambdaCaptureItemNormalAst |
        LambdaCaptureItemNamedAst)


@dataclass
class LambdaPrototypeAst(Ast):
    annotations: List[AnnotationAst]
    fun_token: TokenAst
    generic_parameters: GenericParameterGroupAst
    parameters: FunctionParameterGroupAst
    arrow_token: TokenAst
    return_type: TypeAst
    where_block: Optional[WhereBlockAst]
    lambda_capture_block: Optional[LambdaCaptureBlockAst]
    body: InnerScopeAst[StatementAst]

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        s = ""
        s += f"{Seq(self.annotations).print(printer, "\n")}\n{self.fun_token.print(printer)}{self.generic_parameters.print(printer)}{self.parameters.print(printer)} {self.arrow_token.print(printer)} {self.return_type.print(printer)}"
        s += f" {self.where_block.print(printer)}" if self.where_block else ""
        s += f"{self.lambda_capture_block.print(printer)}" if self.lambda_capture_block else ""
        s += f"{self.body.print(printer)}"
        return s


@dataclass
class LetStatementInitializedAst(Ast, PreProcessor, SymbolGenerator, SemanticAnalysis):
    let_keyword: TokenAst
    assign_to: LocalVariableAst
    assign_token: TokenAst
    value: ExpressionAst
    _sup_let_type: TypeAst = dataclasses.field(default=None)

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        s = ""
        s += f"{self.let_keyword.print(printer)}{self.assign_to.print(printer)} {self.assign_token.print(printer)} {self.value.print(printer)}"
        return s

    def pre_process(self, context) -> None:
        pass

    def generate(self, s: ScopeHandler) -> None:
        s.current_scope.add_symbol(VariableSymbol(self.assign_to.identifier, self._sup_let_type))

    def do_semantic_analysis(self, scope_handler, **kwargs) -> None:
        match self.assign_to:
            case LocalVariableSingleAst():
                # Add the symbol to the current scope
                # Defer the type for postfix function calls that need analysing before return type from overload is known
                # This is fine because a symbol can never refer to itself in its own definition (just not possible)
                sym = VariableSymbol(
                    name=self.assign_to.identifier,
                    type=None,
                    is_mutable=self.assign_to.is_mutable is not None,
                    memory_info=MemoryStatus(ast_initialized=self.assign_to.identifier))
                scope_handler.current_scope.add_symbol(sym)

                kwargs |= {"assignment": True}
                if not self._sup_let_type:
                    ensure_memory_integrity_of_expression(self.value, scope_handler, keep_consume=True, **kwargs)
                    sym.type = self.value.infer_type(scope_handler, **kwargs)[1]
                else:
                    self.value.do_semantic_analysis(scope_handler, **kwargs)
                    sym.type = self.value.infer_type(scope_handler, **kwargs)[1]
                kwargs.pop("assignment")

            case LocalVariableTupleAst():
                # Check there are the same number of elements on the LHS as the RHS
                # TODO: move into LocalVariableTupleAst
                rhs_tuple_type_elements = self.value.infer_type(scope_handler, **kwargs)[1].parts[-1].generic_arguments.arguments
                if len(self.assign_to.items) != len(rhs_tuple_type_elements):
                    exception = SemanticError(f"Invalid tuple assignment:")
                    exception.add_traceback(self.assign_to.pos, f"Assignment target tuple contains {len(self.assign_to.items)} elements.")
                    exception.add_traceback(self.value.pos, f"Assignment value tuple contains {len(rhs_tuple_type_elements)} elements.")
                    raise exception

                for i, current_let_statement in enumerate(self.assign_to.items):
                    if isinstance(current_let_statement, LocalVariableSkipArgumentAst):
                        pass

                    new_rhs = PostfixExpressionAst(
                        pos=self.pos,
                        lhs=self.value,
                        op=PostfixExpressionOperatorMemberAccessAst(
                            pos=self.pos,
                            dot_token=TokenAst.dummy(TokenType.TkDot),
                            identifier=TokenAst.dummy(TokenType.LxDecDigits, info=str(i))))

                    new_let_statement = LetStatementInitializedAst(
                        pos=self.pos,
                        let_keyword=self.let_keyword,
                        assign_to=current_let_statement,
                        assign_token=self.assign_token,
                        value=new_rhs,
                        _sup_let_type=self._sup_let_type)

                    new_let_statement.do_semantic_analysis(scope_handler, **kwargs)

            case LocalVariableDestructureAst():
                # Check that all the arguments given are attributes on the class type
                self.assign_to.do_semantic_analysis(scope_handler, **kwargs)

                new_let_statements = []
                for current_let_statement in self.assign_to.items:
                    if isinstance(current_let_statement, LocalVariableSkipArgumentAst):
                        continue

                    new_rhs = PostfixExpressionAst(
                        pos=self.pos,
                        lhs=self.value,
                        op=PostfixExpressionOperatorMemberAccessAst(
                            pos=self.pos,
                            dot_token=TokenAst.dummy(TokenType.TkDot),
                            identifier=current_let_statement.identifier))

                    new_let_statement = LetStatementInitializedAst(
                        pos=self.pos,
                        let_keyword=self.let_keyword,
                        assign_to=current_let_statement,
                        assign_token=self.assign_token,
                        value=new_rhs,
                        _sup_let_type=self._sup_let_type)

                    new_let_statements.append(new_let_statement)

                for new_let_statement in new_let_statements:
                    new_let_statement.do_semantic_analysis(scope_handler, **kwargs)


@dataclass
class LetStatementUninitializedAst(Ast, SemanticAnalysis):
    let_keyword: TokenAst
    assign_to: LocalVariableAst
    colon_token: TokenAst
    type_declaration: TypeAst

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        return f"{self.let_keyword.print(printer)}{self.assign_to.print(printer)}{self.colon_token.print(printer)} {self.type_declaration.print(printer)}"

    def do_semantic_analysis(self, scope_handler: ScopeHandler, **kwargs) -> None:
        sym = VariableSymbol(
            self.assign_to.identifier,
            self.type_declaration,
            is_mutable=self.assign_to.is_mutable is not None,
            memory_info=MemoryStatus(ast_consumed=self))

        scope_handler.current_scope.add_symbol(sym)


type LetStatementAst = (
        LetStatementInitializedAst |
        LetStatementUninitializedAst)


@dataclass
class LiteralNumberBase10Ast(Ast, SemanticAnalysis, TypeInfer):
    sign: Optional[TokenAst]
    number: TokenAst
    raw_type: Optional[IdentifierAst]
    type: Optional[TypeAst] = dataclasses.field(default=None, init=False)

    def __post_init__(self) -> None:
        if self.raw_type:
            corrected_raw_type = GenericIdentifierAst(self.raw_type.pos, self.raw_type.value.title(), None)
            std_namespace = IdentifierAst(self.raw_type.pos, "std")
            self.type = TypeSingleAst(self.raw_type.pos, [std_namespace, corrected_raw_type])

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        s = ""
        s += f"{self.sign.print(printer)}" if self.sign else ""
        s += f"{self.number.print(printer)}"
        s += f"_{self.raw_type.print(printer)}" if self.raw_type else ""
        return s

    def do_semantic_analysis(self, scope_handler: ScopeHandler, **kwargs) -> None:
        # TODO: make sure that value < bound of type is its given (don't allow narrowing)
        # TODO: don't allow [float -> int] conversion
        ...

    def infer_type(self, scope_handler, **kwargs) -> Tuple[Type[ConventionAst], TypeAst]:
        if self.type:
            return ConventionMovAst, self.type
        if self.number.token.token_type == TokenType.LxDecFloat:
            return ConventionMovAst, CommonTypes.big_dec(self.pos)
        return ConventionMovAst, CommonTypes.big_num(self.pos)

    def __eq__(self, other):
        return isinstance(other, LiteralNumberBase10Ast) and self.sign == other.sign and self.number == other.number and self.type == other.type


@dataclass
class LiteralNumberBase02Ast(Ast):
    integer: TokenAst
    primitive_type: Optional[IdentifierAst]

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        s = ""
        s += f"{self.integer.print(printer)}"
        s += f"{self.primitive_type.print(printer)}" if self.primitive_type else ""
        return s


@dataclass
class LiteralNumberBase16Ast(Ast):
    integer: TokenAst
    primitive_type: Optional[IdentifierAst]

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        s = ""
        s += f"{self.integer.print(printer)}"
        s += f"{self.primitive_type.print(printer)}" if self.primitive_type else ""
        return s


type LiteralNumberAst = (
        LiteralNumberBase10Ast |
        LiteralNumberBase02Ast |
        LiteralNumberBase16Ast)


@dataclass
class LiteralStringAst(Ast, SemanticAnalysis, TypeInfer):
    string: TokenAst

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        return f"{self.string.print(printer)}"

    def do_semantic_analysis(self, scope_handler: ScopeHandler, **kwargs) -> None:
        pass

    def infer_type(self, scope_handler, **kwargs) -> Tuple[Type[ConventionAst], TypeAst]:
        return ConventionMovAst, CommonTypes.str(pos=self.pos)


@dataclass
class LiteralArrayNonEmptyAst(Ast, SemanticAnalysis, TypeInfer):
    bracket_l_token: TokenAst
    elements: List[ExpressionAst]
    bracket_r_token: TokenAst

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        return f"{self.bracket_l_token.print(printer)}{Seq(self.elements).print(printer, ", ")}{self.bracket_r_token.print(printer)}"

    def do_semantic_analysis(self, scope_handler: ScopeHandler, **kwargs) -> None:
        Seq(self.elements).for_each(lambda i: i.do_semantic_analysis(scope_handler, **kwargs))
        non_matching_types = Seq(self.elements).map(lambda i: i.infer_type(scope_handler, **kwargs)).unique_items()

        if non_matching_types.length > 1:
            exception = SemanticError(f"Array items must have the same type:")
            exception.add_traceback(non_matching_types[0][1].pos, f"Item '{non_matching_types[0][0].default()}{non_matching_types[0][1]}' found here.")
            exception.add_traceback(non_matching_types[1][1].pos, f"Item '{non_matching_types[1][0].default()}{non_matching_types[1][1]}' has a different type.")
            raise exception

    def infer_type(self, scope_handler: ScopeHandler, **kwargs) -> Tuple[Type[ConventionAst], TypeAst]:
        return ConventionMovAst, CommonTypes.arr(self.elements[0].infer_type(scope_handler, **kwargs)[1], pos=self.pos)


@dataclass
class LiteralArrayEmptyAst(Ast, SemanticAnalysis, TypeInfer):
    bracket_l_token: TokenAst
    element_type: TypeAst
    bracket_r_token: TokenAst

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        return f"{self.bracket_l_token.print(printer)}{self.element_type.print(printer)}{self.bracket_r_token.print(printer)}"

    def do_semantic_analysis(self, scope_handler: ScopeHandler, **kwargs) -> None:
        self.element_type.do_semantic_analysis(scope_handler, **kwargs)

    def infer_type(self, scope_handler: ScopeHandler, **kwargs) -> Tuple[Type[ConventionAst], TypeAst]:
        return ConventionMovAst, CommonTypes.arr(self.element_type, pos=self.pos)


type LiteralArrayAst = (
        LiteralArrayNonEmptyAst |
        LiteralArrayEmptyAst)


@dataclass
class LiteralTupleAst(Ast, SemanticAnalysis, TypeInfer):
    paren_l_token: TokenAst
    items: List[ExpressionAst]
    paren_r_token: TokenAst

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        return f"{self.paren_l_token.print(printer)}{Seq(self.items).print(printer, ", ")}{self.paren_r_token.print(printer)}"

    def do_semantic_analysis(self, scope_handler: ScopeHandler, **kwargs) -> None:
        Seq(self.items).for_each(lambda i: i.do_semantic_analysis(scope_handler, **kwargs))

    def infer_type(self, scope_handler: ScopeHandler, **kwargs) -> Tuple[Type[ConventionAst], TypeAst]:
        tuple_type = CommonTypes.tuple(Seq(self.items).map(lambda i: i.infer_type(scope_handler, **kwargs)[1]).value, pos=self.pos)
        tuple_type.do_semantic_analysis(scope_handler, **kwargs)
        return ConventionMovAst, tuple_type


@dataclass
class LiteralBooleanAst(Ast, SemanticAnalysis, TypeInfer):
    boolean: bool

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        return f"{self.boolean}"

    def do_semantic_analysis(self, scope_handler: ScopeHandler, **kwargs) -> None:
        pass

    def infer_type(self, scope_handler, **kwargs) -> Tuple[Type[ConventionAst], TypeAst]:
        return ConventionMovAst, CommonTypes.bool(self.pos)


@dataclass
class LiteralRegexAst(Ast):
    regex: TokenAst

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        return f"{self.regex.print(printer)}"


type LiteralAst = (
        LiteralNumberAst |
        LiteralStringAst |
        LiteralArrayAst |
        LiteralTupleAst |
        LiteralBooleanAst |
        LiteralRegexAst)


@dataclass
class LocalVariableSingleAst(Ast):
    is_mutable: Optional[TokenAst]
    unpack_token: Optional[TokenAst]
    identifier: IdentifierAst

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        s = ""
        s += f"{self.is_mutable.print(printer)}" if self.is_mutable else ""
        s += f"{self.unpack_token.print(printer)}" if self.unpack_token else ""
        s += f"{self.identifier.print(printer)}"
        return s


@dataclass
class LocalVariableTupleAst(Ast):
    paren_l_token: TokenAst
    items: List[LocalVariableSingleAst]
    paren_r_token: TokenAst

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        return f"{self.paren_l_token.print(printer)}{Seq(self.items).print(printer, ", ")}{self.paren_r_token.print(printer)}"


@dataclass
class LocalVariableDestructureAst(Ast, SemanticAnalysis, TypeInfer):
    class_type: TypeAst
    bracket_l_token: TokenAst
    items: List[LocalVariableSingleAst]
    bracket_r_token: TokenAst

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        return f"{self.class_type.print(printer)}{self.bracket_l_token.print(printer)}{Seq(self.items).print(printer, ", ")}{self.bracket_r_token.print(printer)}"

    def do_semantic_analysis(self, scope_handler: ScopeHandler, **kwargs) -> None:
        self.class_type.do_semantic_analysis(scope_handler, **kwargs)
        class_type_sym = scope_handler.current_scope.get_symbol(self.class_type)
        attributes = Seq(class_type_sym.type.body.members)

        has_skipped_args = None
        for argument in self.items:
            if isinstance(argument, LocalVariableSkipArgumentAst):
                if has_skipped_args:
                    exception = SemanticError(f"Multiple '..' given to pattern:")
                    exception.add_traceback(has_skipped_args.pos, f"1st variadic argument given here.")
                    exception.add_traceback_minimal(argument.variadic_token.pos, f"2nd variadic argument given here.")
                    raise exception
                has_skipped_args = argument
                continue

            unpack = argument.unpack_token if isinstance(argument, LocalVariableSingleAst) else None
            if unpack:
                exception = SemanticError(f"Cannot use the unpack token '..' in a destructure pattern:")
                exception.add_traceback(unpack.pos, f"Unpack token '..' found here.")
                raise exception

            if not attributes.map(lambda a: a.identifier).contains(argument.identifier):
                exception = SemanticError(f"Invalid destructure assignment:")
                exception.add_traceback(self.class_type.pos, f"Class '{self.class_type}' declared here with attributes: {attributes}")
                exception.add_traceback(argument.pos, f"Attribute '{argument.identifier}' not found on class '{self.class_type}'")
                raise exception

            if isinstance(argument, LocalVariableAssignmentAst):
                value = argument.value
                value.do_semantic_analysis(scope_handler, **kwargs)

                corresponding_attribute = attributes.find(lambda attribute: attribute.identifier == argument.identifier)
                value_type = value.infer_type(scope_handler, **kwargs)

                if value_type[0] != ConventionMovAst or not value_type[1].symbolic_eq(corresponding_attribute.type_declaration, scope_handler.current_scope):
                    exception = SemanticError(f"Invalid type '{value_type[0].default()}{value_type[1]}' given to attribute '{argument.identifier}':")
                    exception.add_traceback(corresponding_attribute.identifier.pos, f"Attribute '{corresponding_attribute.identifier}' declared here with type '{corresponding_attribute.type_declaration}'.")
                    exception.add_traceback(value.pos, f"Attribute '{argument.identifier}' given value here with type '{value_type[0].default()}{value_type[1]}'.")
                    raise exception

        attributes_assigned_to = Seq(self.items).filter(lambda a: not isinstance(a, LocalVariableSkipArgumentAst))
        if attributes_assigned_to.length < attributes.length and not has_skipped_args:
            exception = SemanticError(f"Missing attribute(s) in pattern for type '{self.class_type}':")
            exception.add_traceback(class_type_sym.type.identifier.pos, f"Class '{self.class_type}' declared here with attributes '{attributes}'.")
            exception.add_traceback(self.pos, f"Initialization missing attributes '{attributes.map(lambda a: a.identifier) - attributes_assigned_to.map(lambda v: v.identifier)}'. Consider adding a '..' to mark variables as deliberately excluded.")
            raise exception

    def infer_type(self, scope_handler: ScopeHandler, **kwargs) -> Tuple[Type[ConventionAst], TypeAst]:
        return ConventionMovAst, self.class_type


@dataclass
class LocalVariableAssignmentAst(Ast):
    identifier: IdentifierAst
    assign_token: TokenAst
    value: LocalVariableNestedAst

    def print(self, printer: AstPrinter) -> str:
        return f"{self.identifier.print(printer)}{self.assign_token.print(printer)}{self.value.print(printer)}"


@dataclass
class LocalVariableSkipArgumentAst(Ast):
    variadic_token: TokenAst

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        return f"{self.variadic_token.print(printer)}"


type LocalVariableAst = (
        LocalVariableSingleAst |
        LocalVariableTupleAst |
        LocalVariableDestructureAst)


type LocalVariableNestedAst = (
        LocalVariableSingleAst |
        LocalVariableTupleAst |
        LocalVariableDestructureAst |
        LocalVariableAssignmentAst |
        LocalVariableSkipArgumentAst
)


@dataclass
class ModuleIdentifierAst(Ast):
    parts: List[IdentifierAst]

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        return f"{Seq(self.parts).print(printer, ".")}"


@dataclass
class ModulePrototypeAst(Ast, SemanticAnalysis):
    annotations: List[AnnotationAst]
    module_keyword: TokenAst
    identifier: ModuleIdentifierAst
    body: ModuleImplementationAst

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        return f"{Seq(self.annotations).print(printer, "\n")}\n{self.module_keyword.print(printer)}{self.identifier.print(printer)}\n{self.body.print(printer)}"

    def do_semantic_analysis(self, scope_handler, **kwargs) -> None:
        Seq(self.annotations).for_each(lambda x: x.do_semantic_analysis(scope_handler, **kwargs))
        # self.identifier.do_semantic_analysis(s)
        self.body.do_semantic_analysis(scope_handler, **kwargs)


@dataclass
class ModuleImplementationAst(Ast, SemanticAnalysis):
    members: List[ModuleMemberAst]

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        return f"{Seq(self.members).print(printer, "\n\n")}"

    def do_semantic_analysis(self, scope_handler, **kwargs) -> None:
        Seq(self.members).for_each(lambda x: x.do_semantic_analysis(scope_handler, **kwargs))


@dataclass
class ObjectInitializerArgumentNormalAst(Ast, SemanticAnalysis):
    identifier: IdentifierAst

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        return f"{self.identifier.print(printer)}"

    def do_semantic_analysis(self, scope_handler: ScopeHandler, **kwargs) -> None:
        self.identifier.do_semantic_analysis(scope_handler, **kwargs)


@dataclass
class ObjectInitializerArgumentNamedAst(Ast, SemanticAnalysis):
    identifier: IdentifierAst | TokenAst
    assignment_token: TokenAst
    value: ExpressionAst

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        return f"{self.identifier.print(printer)}{self.assignment_token.print(printer)}{self.value.print(printer)}"

    def do_semantic_analysis(self, scope_handler: ScopeHandler, **kwargs) -> None:
        self.value.do_semantic_analysis(scope_handler, **kwargs)


type ObjectInitializerArgumentAst = (
        ObjectInitializerArgumentNormalAst |
        ObjectInitializerArgumentNamedAst)


@dataclass
class ObjectInitializerArgumentGroupAst(Ast, SemanticAnalysis):
    paren_l_token: TokenAst
    arguments: List[ObjectInitializerArgumentAst]
    paren_r_token: TokenAst

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        s = ""
        s += f"{self.paren_l_token.print(printer)}"
        s += f"{Seq(self.arguments).print(printer, ", ")}" if self.arguments else ""
        s += f"{self.paren_r_token.print(printer)}"
        return s

    def do_semantic_analysis(self, scope_handler: ScopeHandler, **kwargs) -> None:
        Seq(self.arguments).for_each(lambda a: a.do_semantic_analysis(scope_handler, **kwargs))


@dataclass
class ObjectInitializerAst(Ast, SemanticAnalysis, TypeInfer):
    class_type: TypeAst
    arguments: ObjectInitializerArgumentGroupAst
    _modified_type: Optional[TypeAst] = dataclasses.field(default=None)

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        return f"{self.class_type.print(printer)}{self.arguments.print(printer)}"

    def do_semantic_analysis(self, scope_handler: ScopeHandler, **kwargs) -> None:
        self.class_type.do_semantic_analysis(scope_handler, **kwargs)

        type_sym = scope_handler.current_scope.get_symbol(self.class_type)
        type_scope = type_sym.associated_scope
        attributes = Seq(type_sym.type.body.members)
        attribute_names = attributes.map(lambda s: s.identifier)
        sup_classes = type_scope.exclusive_sup_scopes

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
            ensure_memory_integrity_of_expression(default_value_given[0].value, scope_handler, **kwargs)

        # Check that the default value is of the correct type:
        if default_value_given:
            default_value_given_type = default_value_given[0].value.infer_type(scope_handler, **kwargs)
            if ConventionMovAst == default_value_given_type[0] and self.class_type.symbolic_eq(default_value_given_type[1], scope_handler.current_scope):
                exception = SemanticError(f"Invalid type default value type:")
                exception.add_traceback(type_sym.type.identifier.pos, f"Object initializer declared here with type '{self.class_type}'.")
                exception.add_traceback_minimal(default_value_given[0].value.pos, f"Object initializer given value here with type '{default_value_given_type[0].default()}{default_value_given_type[1]}'.")
                raise exception

        # Mark the symbol for the default value as consumed:
        if default_value_given:
            ensure_memory_integrity_of_expression(default_value_given[0].value, scope_handler, keep_consume=True, **kwargs)

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

        inferred_generic_arguments = infer_generic_argument_values(
            scope_handler=scope_handler,
            generic_parameters=Seq(type_sym.type.generic_parameters.parameters),
            infer_from=attributes.map(lambda p: p.type_declaration),
            replace_with=Seq(self.arguments.arguments).map(lambda a: a.value.infer_type(scope_handler, **kwargs)[1]),
            obj_definition=type_sym.type)

        all_generic_arguments = verify_generic_arguments(
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
            ensure_memory_integrity_of_expression(given_argument, scope_handler, **kwargs)

            # Type check
            given_argument_type = given_argument.infer_type(scope_handler, **kwargs)
            if given_argument_type[0] != ConventionMovAst or not given_argument_type[1].symbolic_eq(attribute.type_declaration, type_scope, class_scope):
                exception = SemanticError(f"Invalid type '{given_argument_type[0].default()}{given_argument_type[1]}' given to attribute '{attribute.identifier}':")
                exception.add_traceback(attribute.identifier.pos, f"Attribute '{attribute.identifier}' declared here with type '{attribute.type_declaration}'.")
                exception.add_traceback(given_argument.pos, f"Attribute '{attribute.identifier}' given value here with type '{given_argument_type[0].default()}{given_argument_type[1]}'.")
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
        return ConventionMovAst, self._modified_type


@dataclass
class ParenthesizedExpressionAst(Ast, SemanticAnalysis, TypeInfer):
    paren_l_token: TokenAst
    expression: ExpressionAst
    paren_r_token: TokenAst

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        return f"{self.paren_l_token.print(printer)}{self.expression.print(printer)}{self.paren_r_token.print(printer)}"

    def do_semantic_analysis(self, scope_handler: ScopeHandler, **kwargs) -> None:
        self.expression.do_semantic_analysis(scope_handler, **kwargs)

    def infer_type(self, scope_handler: ScopeHandler, if_condition: ExpressionAst = None, **kwargs) -> Tuple[Type[ConventionAst], TypeAst]:
        return self.expression.infer_type(scope_handler, **kwargs)


@dataclass
class PatternVariantTupleAst(Ast, SemanticAnalysis, TypeInfer):
    paren_l_token: TokenAst
    items: List[PatternVariantNestedAst]
    paren_r_token: TokenAst

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        s = ""
        s += f"{self.paren_l_token.print(printer)}"
        s += f"{Seq(self.items).print(printer, ", ")}" if self.items else ""
        s += f"{self.paren_r_token.print(printer)}"
        return s

    def do_semantic_analysis(self, scope_handler: ScopeHandler, if_condition: ExpressionAst = None, **kwargs) -> None:
        Seq(self.items).for_each(lambda x: x.do_semantic_analysis(scope_handler, if_condition=if_condition, **kwargs))

        has_skipped_args = None
        for argument in self.items:
            if isinstance(argument, PatternVariantSkipArgumentAst):
                if has_skipped_args:
                    exception = SemanticError(f"Multiple '..' given to pattern:")
                    exception.add_traceback(has_skipped_args.pos, f"1st variadic argument given here.")
                    exception.add_traceback_minimal(argument.variadic_token.pos, f"2nd variadic argument given here.")
                    raise exception
                has_skipped_args = argument
                continue

        lhs_tuple_type_elements = if_condition.infer_type(scope_handler, **kwargs)[1].parts[-1].generic_arguments.arguments
        rhs_tuple_type_elements = self.items
        if len(lhs_tuple_type_elements) != len(rhs_tuple_type_elements) and not has_skipped_args:
            exception = SemanticError(f"Invalid tuple assignment:")
            exception.add_traceback(self.pos, f"Assignment target tuple contains {len(rhs_tuple_type_elements)} elements.")
            exception.add_traceback(if_condition.pos, f"Assignment value tuple contains {len(lhs_tuple_type_elements)} elements.")
            raise exception

    def infer_type(self, scope_handler: ScopeHandler, if_condition: ExpressionAst = None, **kwargs) -> Tuple[Type[ConventionAst], TypeAst]:
        return ConventionMovAst, if_condition.infer_type(scope_handler, **kwargs)[1]


@dataclass
class PatternVariantDestructureAst(Ast, SemanticAnalysis, TypeInfer):
    class_type: TypeAst
    bracket_l_token: TokenAst
    items: List[PatternVariantNestedAst]
    bracket_r_token: TokenAst

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        s = ""
        s += f"{self.class_type.print(printer)}{self.bracket_l_token.print(printer)}"
        s += f"{Seq(self.items).print(printer, ", ")}" if self.items else ""
        s += f"{self.bracket_r_token.print(printer)}"
        return s

    def convert_to_variable(self) -> LocalVariableDestructureAst:
        converted_items = Seq(self.items).filter_to_type(PatternVariantVariableAssignmentAst, PatternVariantSkipArgumentAst, PatternVariantVariableAst).map(lambda i: i.convert_to_variable())

        return LocalVariableDestructureAst(
            pos=self.pos,
            class_type=self.class_type,
            bracket_l_token=self.bracket_l_token,
            items=converted_items.value,
            bracket_r_token=self.bracket_r_token)

    def do_semantic_analysis(self, scope_handler: ScopeHandler, if_condition: ExpressionAst = None, **kwargs) -> None:
        conversion = self.convert_to_variable()
        conversion.do_semantic_analysis(scope_handler, **kwargs)

    def infer_type(self, scope_handler: ScopeHandler, if_condition: ExpressionAst = None, **kwargs) -> Tuple[Type[ConventionAst], TypeAst]:
        return ConventionMovAst, self.class_type


@dataclass
class PatternVariantVariableAst(Ast, SemanticAnalysis, TypeInfer):
    is_mutable: Optional[TokenAst]
    unpack_token: Optional[TokenAst]
    identifier: IdentifierAst

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        s = ""
        s += f"{self.is_mutable.print(printer)}" if self.is_mutable else ""
        s += f"{self.unpack_token.print(printer)}" if self.unpack_token else ""
        s += f"{self.identifier.print(printer)}"
        return s

    def convert_to_variable(self) -> LocalVariableSingleAst:
        return LocalVariableSingleAst(
            pos=self.pos,
            is_mutable=self.is_mutable,
            unpack_token=self.unpack_token,
            identifier=self.identifier)

    def do_semantic_analysis(self, scope_handler: ScopeHandler, if_condition: ExpressionAst = None, **kwargs) -> None:
        conversion = self.convert_to_variable()

    def infer_type(self, scope_handler: ScopeHandler, if_condition: ExpressionAst = None, **kwargs) -> Tuple[Type[ConventionAst], TypeAst]:
        ...


@dataclass
class PatternVariantLiteralAst(Ast, SemanticAnalysis, TypeInfer):
    literal: LiteralAst

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        return f"{self.literal.print(printer)}"

    def convert_to_variable(self) -> LocalVariableAst:
        return self.literal

    def do_semantic_analysis(self, scope_handler: ScopeHandler, if_condition: ExpressionAst = None, **kwargs) -> None:
        self.literal.do_semantic_analysis(scope_handler, **kwargs)

    def infer_type(self, scope_handler: ScopeHandler, if_condition: ExpressionAst = None, **kwargs) -> Tuple[Type[ConventionAst], TypeAst]:
        return self.literal.infer_type(scope_handler, **kwargs)


@dataclass
class PatternVariantVariableAssignmentAst(Ast, SemanticAnalysis, TypeInfer):
    identifier: IdentifierAst
    assign_token: TokenAst
    value: PatternVariantAst

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        return f"{self.identifier.print(printer)}{self.assign_token.print(printer)}{self.value.print(printer)}"

    def convert_to_variable(self) -> LocalVariableAssignmentAst:
        return LocalVariableAssignmentAst(
            pos=self.pos,
            identifier=self.identifier,
            assign_token=self.assign_token,
            value=self.value.convert_to_variable())

    def do_semantic_analysis(self, scope_handler: ScopeHandler, if_condition: ExpressionAst = None, **kwargs) -> None:
        conversion = self.convert_to_variable()
        conversion.do_semantic_analysis(scope_handler, **kwargs)

    def infer_type(self, scope_handler: ScopeHandler, if_condition: ExpressionAst = None, **kwargs) -> Tuple[Type[ConventionAst], TypeAst]:
        return ConventionMovAst, CommonTypes.void(self.pos)


@dataclass
class PatternVariantBoolMemberAst(Ast):
    expression: PostfixExpressionAst

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        return f"{self.expression.print(printer)}"


@dataclass
class PatternVariantElseAst(Ast, SemanticAnalysis):
    else_token: TokenAst

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        return f"{self.else_token.print(printer)}"

    def do_semantic_analysis(self, scope_handler: ScopeHandler, if_condition: ExpressionAst = None, **kwargs) -> None:
        ...


@dataclass
class PatternVariantSkipArgumentAst(Ast, SemanticAnalysis):
    variadic_token: TokenAst

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        return f"{self.variadic_token.print(printer)}"

    def convert_to_variable(self) -> LocalVariableSkipArgumentAst:
        return LocalVariableSkipArgumentAst(
            pos=self.pos,
            variadic_token=self.variadic_token)

    def do_semantic_analysis(self, scope_handler: ScopeHandler, if_condition: ExpressionAst = None, **kwargs) -> None:
        conversion = self.convert_to_variable()
        conversion.do_semantic_analysis(scope_handler, **kwargs)


type PatternVariantAst = (
        PatternVariantTupleAst |
        PatternVariantDestructureAst |
        PatternVariantVariableAst |
        PatternVariantLiteralAst |
        PatternVariantBoolMemberAst |
        PatternVariantElseAst)

type PatternVariantNestedAst = (
        PatternVariantVariableAssignmentAst |
        PatternVariantTupleAst |
        PatternVariantDestructureAst |
        PatternVariantVariableAst |
        PatternVariantLiteralAst |
        PatternVariantSkipArgumentAst
)


@dataclass
class PatternBlockAst(Ast, SemanticAnalysis):
    comp_operator: Optional[TokenAst]
    patterns: List[PatternVariantAst]
    guard: Optional[PatternGuardAst]
    body: InnerScopeAst[StatementAst]

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        s = ""
        s += f"{self.comp_operator.print(printer)} " if self.comp_operator else ""
        s += f"{Seq(self.patterns).print(printer, ", ")}"
        s += f"{self.guard.print(printer)}" if self.guard else ""
        s += f" {self.body.print(printer)}"
        return s

    def is_else_branch(self) -> bool:
        return isinstance(self.patterns[0], PatternVariantElseAst)

    def do_semantic_analysis(self, scope_handler: ScopeHandler, if_condition: ExpressionAst = None, **kwargs) -> None:
        scope_handler.into_new_scope(f"<pattern-block:{Seq(self.patterns)}>")

        Seq(self.patterns).for_each(lambda p: p.do_semantic_analysis(scope_handler, if_condition, **kwargs))
        self.guard.do_semantic_analysis(scope_handler, **kwargs) if self.guard else None
        self.body.do_semantic_analysis(scope_handler, **kwargs)
        scope_handler.exit_cur_scope()


@dataclass
class PatternGuardAst(Ast, SemanticAnalysis):
    guard_token: TokenAst
    expression: ExpressionAst

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        return f"{self.guard_token.print(printer)}{self.expression.print(printer)}"

    def do_semantic_analysis(self, scope_handler: ScopeHandler, **kwargs) -> None:
        self.expression.do_semantic_analysis(scope_handler, **kwargs)

        expression_type = self.expression.infer_type(scope_handler, **kwargs)[1]
        if not expression_type.symbolic_eq(CommonTypes.bool(), scope_handler.current_scope):
            exception = SemanticError(f"Guard expression must be of type 'Bool':")
            exception.add_traceback(self.expression.pos, f"Expression '{self.expression}' has type '{expression_type}'.")
            raise exception


@dataclass
class PostfixExpressionAst(Ast, SemanticAnalysis, TypeInfer):
    lhs: ExpressionAst
    op: PostfixExpressionOperatorAst

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        return f"{self.lhs.print(printer)}{self.op.print(printer)}"

    def do_semantic_analysis(self, scope_handler: ScopeHandler, **kwargs) -> None:
        self.lhs.do_semantic_analysis(scope_handler, **kwargs)
        self.op.do_semantic_analysis(scope_handler, postfix_lhs=self.lhs, **kwargs)

    def infer_type(self, scope_handler: ScopeHandler, **kwargs) -> Tuple[Type[ConventionAst], TypeAst]:
        return self.op.infer_type(scope_handler, postfix_lhs=self.lhs, **kwargs)

    def __eq__(self, other):
        return isinstance(other, PostfixExpressionAst) and self.lhs == other.lhs and self.op == other.op


@dataclass
class PostfixExpressionOperatorFunctionCallAst(Ast, SemanticAnalysis, TypeInfer):
    generic_arguments: Optional[GenericArgumentGroupAst]
    arguments: FunctionArgumentGroupAst
    fold_token: Optional[TokenAst]

    def __post_init__(self):
        self.generic_arguments = self.generic_arguments or GenericArgumentGroupAst(-1, TokenAst.dummy(TokenType.TkBrackL), [], TokenAst.dummy(TokenType.TkBrackR))

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        s = ""
        s += f"{self.generic_arguments.print(printer)}" if self.generic_arguments else ""
        s += f"{self.arguments.print(printer)}"
        s += f"{self.fold_token.print(printer)}" if self.fold_token else ""
        return s

    def __get_matching_overload(self, scope_handler: ScopeHandler, function_name: ExpressionAst, **kwargs) -> tuple[FunctionPrototypeAst, Scope, Optional[FunctionArgumentNamedAst]]:
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
            try:
                inferred_generic_arguments = infer_generic_argument_values(
                    scope_handler=scope_handler,
                    generic_parameters=Seq(function_overload.generic_parameters.parameters),
                    infer_from=Seq(function_overload.parameters.parameters).map(lambda p: p.type_declaration),
                    replace_with=Seq(self.arguments.arguments).map(lambda a: a.value.infer_type(scope_handler, **kwargs)[1]),
                    obj_definition=function_overload)

                all_generic_arguments = verify_generic_arguments(
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
                    value=function_name.lhs
                ))

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
                    exception.add_traceback_minimal(argument.pos, f"Argument <{argument}> found here with type '{argument_type[0].default()}{argument_type[1]}', instead of '{corresponding_parameter.convention}{corresponding_parameter.type_declaration}'.")
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

    def do_semantic_analysis(self, scope_handler: ScopeHandler, postfix_lhs: ExpressionAst = None, **kwargs) -> None:
        # Check that a matching overload exists for the function call. Also get the "self" argument (for analysis)
        _, _, self_arg = self.__get_matching_overload(scope_handler, postfix_lhs, **kwargs)
        Seq(self.generic_arguments.arguments).for_each(lambda x: x.type.do_semantic_analysis(scope_handler, **kwargs))

        # Analyse the arguments (including the "self" argument, to check for conflicting borrows)
        if self_arg:
            self.arguments.arguments.append(self_arg)
            self.arguments.do_semantic_analysis(scope_handler, **kwargs)
            self.arguments.arguments.remove(self_arg)
        else:
            self.arguments.do_semantic_analysis(scope_handler, **kwargs)

    def infer_type(self, scope_handler: ScopeHandler, postfix_lhs: ExpressionAst = None, **kwargs) -> Tuple[Type[ConventionAst], TypeAst]:
        # Get the matching overload and return its return-type. 2nd class borrows mean the object returned is always
        # owned, ie ConventionMovAst.
        function_proto, function_scope, _ = self.__get_matching_overload(scope_handler, postfix_lhs, **kwargs)
        function_return_type = copy.deepcopy(function_proto.return_type)
        function_return_type = function_scope.get_symbol(function_return_type).type.identifier  # TODO: this is a hack (namespaced types won't work here)
        return ConventionMovAst, function_return_type


@dataclass
class PostfixExpressionOperatorMemberAccessAst(Ast, SemanticAnalysis):
    dot_token: TokenAst
    identifier: PostfixMemberPartAst

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        return f"{self.dot_token.print(printer)}{self.identifier.print(printer)}"

    def do_semantic_analysis(self, scope_handler: ScopeHandler, postfix_lhs: ExpressionAst = None, **kwargs) -> None:
        lhs = postfix_lhs

        # Check that, for numeric access, the LHS is a tuple type with enough elements in it.
        if isinstance(self.identifier, TokenAst):
            if lhs.infer_type(scope_handler, **kwargs)[1].without_generics() != CommonTypes.tuple([]):
                exception = SemanticError(f"Numeric member access requires a tuple type:")
                exception.add_traceback(lhs.pos, f"Type '{lhs.infer_type(scope_handler, **kwargs)}' found here.")
                exception.add_traceback(self.identifier.pos, f"Numeric member access found here.")
                raise exception

            if int(self.identifier.token.token_metadata) >= len(lhs.infer_type(scope_handler, **kwargs)[1].parts[-1].generic_arguments.arguments):
                lhs_type = lhs.infer_type(scope_handler, **kwargs)
                exception = SemanticError(f"Numeric member access out of bounds:")
                exception.add_traceback(lhs.pos, f"Type '{lhs_type[0].default()}{lhs_type[1]}' found here, with {len(lhs.infer_type(scope_handler, **kwargs)[1].parts[-1].generic_arguments.arguments)} elements.")
                exception.add_traceback(self.identifier.pos, f"Numeric member access found here to element {self.identifier.token.token_metadata}.")
                raise exception

        # Check that, for attribute access, the attribute exists on the type being accessed.
        elif isinstance(self.identifier, IdentifierAst):
            lhs_type_scope = scope_handler.current_scope.get_symbol(lhs.infer_type(scope_handler, **kwargs)[1]).associated_scope
            if not lhs_type_scope.has_symbol(self.identifier):
                lhs_type = lhs.infer_type(scope_handler, **kwargs)
                exception = SemanticError(f"Undefined attribute '{self.identifier.value}' on type '{lhs_type[1]}':")
                exception.add_traceback(lhs.pos, f"Type '{lhs_type[0].default()}{lhs_type[1]}' inferred here.")
                exception.add_traceback(self.identifier.pos, f"Attribute '{self.identifier.value}' accessed here.")
                raise exception

        else:
            raise NotImplementedError

    def infer_type(self, scope_handler: ScopeHandler, postfix_lhs: ExpressionAst = None, **kwargs) -> Tuple[Type[ConventionAst], TypeAst]:
        lhs = postfix_lhs

        # The identifier access needs to get the type of the left side, then inspect the correct attribute for the
        # correct type
        if isinstance(self.identifier, IdentifierAst):
            lhs_type_scope = scope_handler.current_scope.get_symbol(lhs.infer_type(scope_handler, **kwargs)[1]).associated_scope
            return ConventionMovAst, lhs_type_scope.get_symbol(self.identifier).type

        # The numeric access needs to get the generic arguments of the left side (tuple), then get the type of the
        # correct element.
        elif isinstance(self.identifier, TokenAst):
            lhs_type = lhs.infer_type(scope_handler, **kwargs)
            return ConventionMovAst, lhs_type[1].parts[-1].generic_arguments.arguments[int(self.identifier.token.token_metadata)].type

    def __eq__(self, other):
        return isinstance(other, PostfixExpressionOperatorMemberAccessAst) and self.identifier == other.identifier


@dataclass
class PostfixExpressionOperatorEarlyReturnAst(Ast):
    return_token: TokenAst

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        return f"{self.return_token.print(printer)}"


type PostfixExpressionOperatorAst = (
        PostfixExpressionOperatorFunctionCallAst |
        PostfixExpressionOperatorMemberAccessAst |
        PostfixExpressionOperatorEarlyReturnAst)

type PostfixMemberPartAst = (
        IdentifierAst |
        TokenAst)


@dataclass
class ProgramAst(Ast, PreProcessor, SymbolGenerator, SemanticAnalysis):
    module: ModulePrototypeAst
    eof_token: TokenAst

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        return f"{self.module.print(printer)}"

    def pre_process(self, context: ModulePrototypeAst) -> None:
        Seq(self.module.body.members).for_each(lambda m: m.pre_process(context))
        self.module.body.members = Seq(self.module.body.members).filter(lambda m: not isinstance(m, FunctionPrototypeAst)).value

    def generate(self, s: ScopeHandler) -> None:
        Seq(self.module.body.members).for_each(lambda m: m.generate(s))

    def do_semantic_analysis(self, scope_handler, **kwargs) -> None:
        self.module.do_semantic_analysis(scope_handler, **kwargs)


@dataclass
class WhileElseExpressionAst(Ast, SemanticAnalysis, TypeInfer):
    else_keyword: TokenAst
    body: InnerScopeAst[StatementAst]

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        return f"{self.else_keyword.print(printer)}{self.body.print(printer)}"

    def do_semantic_analysis(self, scope_handler: ScopeHandler, **kwargs) -> None:
        self.body.do_semantic_analysis(scope_handler, **kwargs)

    def infer_type(self, scope_handler: ScopeHandler, **kwargs) -> Tuple[Type[ConventionAst], TypeAst]:
        return self.body.infer_type(scope_handler, **kwargs)


@dataclass
class ReturnStatementAst(Ast, SemanticAnalysis):
    return_keyword: TokenAst
    expression: Optional[ExpressionAst]

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        s = ""
        s += f"{self.return_keyword.print(printer)}"
        s += f"{self.expression.print(printer)}" if self.expression else ""
        return s

    def do_semantic_analysis(self, scope_handler, **kwargs) -> None:
        self.expression.do_semantic_analysis(scope_handler, **kwargs) if self.expression else None
        target_return_type = kwargs.get("target-return-type")

        if self.expression:
            ensure_memory_integrity_of_expression(self.expression, scope_handler, **kwargs)

        return_type = self.expression.infer_type(scope_handler, **kwargs) if self.expression else (ConventionMovAst, CommonTypes.void())
        if return_type[0] != ConventionMovAst or not target_return_type.symbolic_eq(return_type[1], scope_handler.current_scope):
            exception = SemanticError(f"Returning variable of incorrect type:")
            exception.add_traceback(target_return_type.pos, f"Function has return type '{target_return_type}'.")
            exception.add_traceback(self.pos, f"Variable '{self.expression}' returned here is type '{return_type[0].default()}{return_type[1]}'.")
            raise exception


@dataclass
class SupMethodPrototypeAst(FunctionPrototypeAst):
    def __eq__(self, other):
        return super().__eq__(other)


@dataclass
class SupPrototypeNormalAst(Ast, PreProcessor, SymbolGenerator, SemanticAnalysis):
    sup_keyword: TokenAst
    generic_parameters: Optional[GenericParameterGroupAst]
    identifier: TypeAst
    where_block: Optional[WhereBlockAst]
    body: InnerScopeAst[SupMemberAst]

    def __post_init__(self):
        self.generic_parameters = self.generic_parameters or GenericParameterGroupAst(-1, TokenAst.dummy(TokenType.TkBrackL), [], TokenAst.dummy(TokenType.TkBrackR))
        self.where_block = self.where_block or WhereBlockAst(-1, TokenAst.dummy(TokenType.KwWhere), WhereConstraintsGroupAst(-1, TokenAst.dummy(TokenType.TkBrackL), [], TokenAst.dummy(TokenType.TkBrackR)))

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        s = ""
        s += f"{self.sup_keyword.print(printer)}{self.generic_parameters.print(printer)}{self.identifier.print(printer)}"
        s += f" {self.where_block.print(printer)}" if self.where_block else ""
        s += f"{self.body.print(printer)}"
        return s

    def pre_process(self, context: ModulePrototypeAst) -> None:
        Seq(self.generic_parameters.get_opt()).for_each(lambda p: p.default_value.substitute_generics(CommonTypes.self(), context.identifier))
        Seq(self.body.members).for_each(lambda m: m.pre_process(self))
        self.body.members = Seq(self.body.members).filter(lambda m: not isinstance(m, FunctionPrototypeAst)).value

    def generate(self, scope_handler: ScopeHandler) -> None:
        scope_handler.into_new_scope(IdentifierAst(self.identifier.parts[0].pos, self.identifier.parts[-1].value + "#SUP-functions"))
        scope_handler.current_scope.add_symbol(TypeSymbol(CommonTypes.self(), scope_handler.current_scope.get_symbol(self.identifier).type))
        Seq(self.body.members).for_each(lambda m: m.generate(scope_handler))
        Seq(self.generic_parameters.parameters).for_each(lambda p: scope_handler.current_scope.add_symbol(TypeSymbol(p.identifier, None)))
        scope_handler.exit_cur_scope()

    def do_semantic_analysis(self, scope_handler, **kwargs) -> None:
        scope_handler.move_to_next_scope()
        self.generic_parameters.do_semantic_analysis(scope_handler, **kwargs)
        # self.where_block.do_semantic_analysis(s)
        self.identifier.do_semantic_analysis(scope_handler, **kwargs)

        self.identifier.without_generics().do_semantic_analysis(scope_handler)  # ?
        cls_scope = scope_handler.current_scope.get_symbol(self.identifier.without_generics()).associated_scope
        cls_scope._sup_scopes.append((scope_handler.current_scope, self))

        self.body.do_semantic_analysis(scope_handler, inline_block=True, **kwargs)
        scope_handler.exit_cur_scope()


@dataclass
class SupPrototypeInheritanceAst(SupPrototypeNormalAst):
    super_class: TypeAst
    on_keyword: TokenAst

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        s = ""
        s += f"{self.sup_keyword.print(printer)}"
        s += f"{self.generic_parameters.print(printer)}" if self.generic_parameters else ""
        s += f"{self.super_class.print(printer)} {self.on_keyword.print(printer)}{self.identifier.print(printer)}"
        s += f" {self.where_block.print(printer)}" if self.where_block else ""
        s += f"{self.body.print(printer)}"
        return s

    def pre_process(self, context: ModulePrototypeAst) -> None:
        if self.super_class.parts[-1].value in ["FunRef", "FunMut", "FunMov"]:
            return
        super().pre_process(context)

    def generate(self, scope_handler: ScopeHandler) -> None:
        scope_handler.into_new_scope(IdentifierAst(self.identifier.parts[0].pos, self.identifier.parts[-1].value + f"#SUP-{self.super_class}"))
        scope_handler.current_scope.add_symbol(TypeSymbol(CommonTypes.self(), scope_handler.current_scope.get_symbol(self.identifier).type))
        Seq(self.body.members).for_each(lambda m: m.generate(scope_handler))
        Seq(self.generic_parameters.parameters).for_each(lambda p: scope_handler.current_scope.add_symbol(TypeSymbol(p.identifier, None)))
        scope_handler.exit_cur_scope()

    def do_semantic_analysis(self, scope_handler, **kwargs) -> None:
        scope_handler.move_to_next_scope()
        self.identifier.do_semantic_analysis(scope_handler, **kwargs)
        self.generic_parameters.do_semantic_analysis(scope_handler, **kwargs)
        self.where_block.do_semantic_analysis(scope_handler, **kwargs)
        self.super_class.do_semantic_analysis(scope_handler, **kwargs)
        self.identifier.do_semantic_analysis(scope_handler, **kwargs)

        self.identifier.without_generics().do_semantic_analysis(scope_handler)  # ?
        cls_scope = scope_handler.current_scope.get_symbol(self.identifier.without_generics()).associated_scope
        cls_scope._sup_scopes.append((scope_handler.current_scope, self))
        cls_scope._sup_scopes.append((scope_handler.current_scope.get_symbol(self.super_class).associated_scope, scope_handler.current_scope.get_symbol(self.super_class).type))

        self.body.do_semantic_analysis(scope_handler, inline_block=True, **kwargs)
        scope_handler.exit_cur_scope()

        # TODO : check there are no direct duplicate sup super-classes
        # TODO : check overriden typedefs & methods appear on super-class
        # TODO : check there are no duplicate / overlapping overloads of methods for this sup-block
        #   - At this point, all sup-blocks are discovered, so we can check for duplicate / overlapping overloads.
        #   - If in this function, it'll happen for every sup-block -- only needs to happen once though (cls block?)


type SupPrototypeAst = (
        SupPrototypeNormalAst |
        SupPrototypeInheritanceAst)


@dataclass
class TypedefStatementAst(Ast, PreProcessor):
    use_keyword: TokenAst
    generic_parameters: GenericParameterGroupAst
    old_type_namespace: Optional[TypeNamespaceAst]
    items: TypedefStatementItemAst

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        s = ""
        s += f"{self.use_keyword.print(printer)}{self.generic_parameters.print(printer)}"
        s += f"{self.old_type_namespace.print(printer)}" if self.old_type_namespace else ""
        s += f"{self.items.print(printer)}"
        return s

    def pre_process(self, context: ModulePrototypeAst) -> None:
        Seq(self.generic_parameters.get_opt()).for_each(lambda p: p.default_value.substitute_generics(CommonTypes.self(), context.identifier))
        ...


@dataclass
class TypedefStatementSpecificItemAst(Ast):
    old_type: TypeAst
    alias: Optional[TypedefStatementSpecificItemAliasAst]

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        s = ""
        s += f"{self.old_type.print(printer)}"
        s += f"{self.alias.print(printer)}" if self.alias else ""
        return s


@dataclass
class TypedefStatementSpecificItemsAst(Ast):
    paren_l_token: TokenAst
    aliases: List[TypedefStatementSpecificItemAst]
    paren_r_token: TokenAst

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        return f"{self.paren_l_token.print(printer)}{Seq(self.aliases).print(printer, ", ")}{self.paren_r_token.print(printer)}"


@dataclass
class TypedefStatementAllItemsAst(Ast):
    all_token: TokenAst

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        return f"{self.all_token.print(printer)}"


type TypedefStatementItemAst = (
        TypedefStatementSpecificItemAst |
        TypedefStatementSpecificItemsAst |
        TypedefStatementAllItemsAst)


@dataclass
class TypedefStatementSpecificItemAliasAst(Ast):
    as_keyword: TokenAst
    new_type: TypeAst

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        return f"{self.as_keyword.print(printer)}{self.new_type.print(printer)}"


@dataclass
class SupTypedefAst(TypedefStatementAst):
    annotations: List[AnnotationAst]

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        return f"{Seq(self.annotations).print(printer, "\n")}\n{super().print(printer)}"

    def pre_process(self, context: SupPrototypeAst) -> None:
        ...


@dataclass
class TypeSingleAst(Ast, SemanticAnalysis):
    parts: List[TypePartAst]

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        return f"{Seq(self.parts).print(printer, ".")}"

    def substitute_generics(self, from_ty: TypeAst, to_ty: TypeAst) -> Self:
        namespace_parts = Seq(self.parts).filter(lambda p: isinstance(p, IdentifierAst)).value
        self._substitute_generics(from_ty, to_ty)
        return self

    def _substitute_generics(self, from_ty: TypeSingleAst, to_ty: TypeSingleAst) -> Self:
        type_parts = [(i, p) for i, p in enumerate(self.parts) if isinstance(p, GenericIdentifierAst)]
        replace_n = len(self.parts) - len(type_parts)
        if self.without_generics() == from_ty.without_generics():
            self.parts = to_ty.parts

        for i, part in type_parts:
            for g in part.generic_arguments.arguments if part.generic_arguments else []:
                g.type._substitute_generics(from_ty, to_ty)

        return self

    def do_semantic_analysis(self, scope_handler: ScopeHandler, **kwargs) -> None:
        base_type_exists = scope_handler.current_scope.has_symbol(self.without_generics())
        this_type_exists = scope_handler.current_scope.has_symbol(self)
        generic_arguments = Seq(self.parts[-1].generic_arguments.arguments)

        if not base_type_exists:
            all_symbols = Seq(scope_handler.current_scope.all_symbols()).filter(lambda s: isinstance(s, TypeSymbol))
            closest_match = difflib.get_close_matches(str(self), all_symbols.map(lambda s: str(s.name)).value, n=1)
            closest_match = f" Did you mean '{closest_match[0]}'?" if closest_match else ""

            exception = SemanticError(f"Type '{self}' is not defined:")
            exception.add_traceback(self.pos, f"Type '{self}' used here.{closest_match}")
            raise exception

        elif not this_type_exists and self.parts[-1].generic_arguments.arguments:
            # Get the symbol for the base type without any generic arguments, and get its associated scope. The parent
            # scope of the type is needed as this is the scope that holds all the types.
            type_sym = scope_handler.current_scope.get_symbol(self.without_generics())
            type_scope = type_sym.associated_scope

            # For each generic parameter, set its type to the corresponding generic argument.
            all_generic_arguments = verify_generic_arguments(
                generic_parameters=Seq(type_sym.type.generic_parameters.parameters),
                inferred_generic_arguments=Seq([]),
                generic_arguments=generic_arguments,
                obj_definition=type_sym.type,
                usage=self,
                scope_handler=scope_handler,
                **kwargs) if self.without_generics() != CommonTypes.tuple([]) else Seq(self.parts[-1].generic_arguments.arguments)
            
            # If the type is a tuple, then its generic arguments are a tuple (variadic) etc etc, so jump into the
            # arguments already
            if self.without_generics() == CommonTypes.tuple([]):
                Seq(self.parts[-1].generic_arguments.arguments).for_each(lambda g: g.type.do_semantic_analysis(scope_handler, **kwargs))
            else:
                all_generic_arguments.for_each(lambda g: g.type.do_semantic_analysis(scope_handler, **kwargs))

            # Copy the scope name, and create a new scope whose name includes the generic arguments. This allows for
            # multiple types with the same name, but different generic arguments, to exist in the same scope.
            modified_type_scope_name = copy.deepcopy(type_scope._scope_name)
            modified_type_scope_name.parts[-1].generic_arguments.arguments = all_generic_arguments.value
            modified_type_scope = Scope(modified_type_scope_name, scope_handler.global_scope)
            modified_type_scope._sup_scopes = type_scope._sup_scopes
            modified_type_scope._symbol_table = copy.deepcopy(type_scope._symbol_table)

            # Copy the type, and substitute the attribute types with the generic arguments.
            modified_type = copy.deepcopy(type_sym.type)

            # Inject the type into the parent scope
            scope_handler.global_scope._children_scopes.append(modified_type_scope)
            scope_handler.global_scope.add_symbol(TypeSymbol(self, modified_type, modified_type_scope))

            # Check each generic argument is a valid type
            for generic_argument in all_generic_arguments:
                generic_argument.type.do_semantic_analysis(scope_handler, **kwargs)
                type_sym = scope_handler.current_scope.get_symbol(generic_argument.type)

                if self.without_generics() != CommonTypes.tuple([]):  # todo
                    modified_type_scope.add_symbol(TypeSymbol(generic_argument.identifier, type_sym.type))
                    modified_type_scope.get_symbol(generic_argument.identifier).associated_scope = type_sym.associated_scope

                    for attribute in Seq(modified_type_scope.all_symbols()).filter_to_type(VariableSymbol):
                        attribute.type.substitute_generics(generic_argument.identifier, generic_argument.type)

                    for attribute in modified_type.body.members:
                        attribute.type_declaration.substitute_generics(generic_argument.identifier, generic_argument.type)

    def __iter__(self):
        # Iterate the parts, and recursively the parts of generic parameters
        def iterate(type_single: TypeSingleAst):
            namespace_parts = Seq(type_single.parts).filter(lambda p: isinstance(p, IdentifierAst)).value
            non_namespace_parts = Seq(type_single.parts).filter(lambda p: not isinstance(p, IdentifierAst)).value

            for part in non_namespace_parts:
                yield TypeSingleAst(part.pos, [*namespace_parts, part])
                for g in part.generic_arguments.arguments:
                    yield from iterate(g.type)

        return iterate(self)

    def without_generics(self) -> TypeSingleAst:
        parts = []
        for part in self.parts:
            parts.append(GenericIdentifierAst(part.pos, part.value, GenericArgumentGroupAst(part.pos, TokenAst.dummy(TokenType.TkBrackL), [], TokenAst.dummy(TokenType.TkBrackR))) if isinstance(part, GenericIdentifierAst) else part)
        return TypeSingleAst(self.pos, parts)

    def __eq__(self, that):
        return isinstance(that, TypeSingleAst) and self.parts == that.parts

    def symbolic_eq(self, that, this_scope: Scope, that_scope: Optional[Scope] = None) -> bool:
        # Allows for generics and aliases to match base types etc.
        that_scope = that_scope or this_scope
        this_type = this_scope.get_symbol(self).type
        that_type = that_scope.get_symbol(that).type
        return this_type == that_type

    def __hash__(self):
        return int.from_bytes(hashlib.md5("".join([str(p) for p in self.parts]).encode()).digest())

    def __json__(self) -> str:
        printer = AstPrinter()
        return self.print(printer)


@dataclass
class TypeTupleAst(Ast):
    paren_l_token: TokenAst
    items: List[TypeAst]
    paren_r_token: TokenAst

    def as_single_type(self):
        return CommonTypes.tuple(self.items)

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        return f"{self.paren_l_token.print(printer)}{Seq(self.items).print(printer, ", ")}{self.paren_r_token.print(printer)}"


@dataclass
class TypeUnionAst(Ast):
    items: List[TypeAst]

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        return f"{Seq(self.items).print(printer, "|")}"

    def as_single_type(self) -> TypeSingleAst:
        return CommonTypes.union(self.items)


type TypeAst = (
        TypeSingleAst |
        TypeTupleAst |
        TypeUnionAst)

type TypePartAst = (
        IdentifierAst |
        GenericIdentifierAst |
        TokenAst)


@dataclass
class TypeNamespaceAst(Ast):
    items: List[TypePartAst]

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        return f"{Seq(self.items).print(printer, ".")}"


@dataclass
class UnaryExpressionAst(Ast, SemanticAnalysis, TypeInfer):
    op: UnaryOperatorAst
    rhs: ExpressionAst

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        return f"{self.op.print(printer)}{self.rhs.print(printer)}"

    def do_semantic_analysis(self, scope_handler: ScopeHandler, **kwargs) -> None:
        # Check that the rhs is a function call (only unary is async)
        if not (isinstance(self.rhs, PostfixExpressionAst) and isinstance(self.rhs.op, PostfixExpressionOperatorFunctionCallAst)):
            exception = SemanticError(f"Invalid 'async' usage:")
            exception.add_traceback(self.pos, f"'{self}' is not a function call.")
            raise exception

    def infer_type(self, scope_handler: ScopeHandler, **kwargs) -> Tuple[Type[ConventionAst], TypeAst]:
        # The type is a Fut[T] where T is the return type of the function call.
        # TODO: this will cause an error, because the RHS hasn't been analysed yet
        return ConventionMovAst, CommonTypes.fut(self.rhs.infer_type(scope_handler, **kwargs)[1], pos=self.pos)


type UnaryOperatorAst = TokenAst


@dataclass
class WhereBlockAst(Ast, SemanticAnalysis):
    where_keyword: TokenAst
    constraint_group: WhereConstraintsGroupAst

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        return f"{self.where_keyword.print(printer)}{self.constraint_group.print(printer)}"

    def __eq__(self, other):
        return isinstance(other, WhereBlockAst) and self.constraint_group == other.constraint_group

    def do_semantic_analysis(self, scope_handler: ScopeHandler, **kwargs) -> None:
        ...


@dataclass
class WhereConstraintsGroupAst(Ast):
    brack_l_token: TokenAst
    constraints: List[WhereConstraintsAst]
    brack_r_token: TokenAst

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        return f"{self.brack_l_token.print(printer)}{Seq(self.constraints).print(printer, ", ")}{self.brack_r_token.print(printer)}"

    def __eq__(self, other):
        return isinstance(other, WhereConstraintsGroupAst) and self.constraints == other.constraints


@dataclass
class WhereConstraintsAst(Ast):
    types_to_constrain: List[TypeAst]
    colon_token: TokenAst
    constraints: List[TypeAst]

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        return f"{Seq(self.types_to_constrain).print(printer, ", ")}{self.colon_token.print(printer)} {Seq(self.constraints).print(printer, ", ")}"

    def __eq__(self, other):
        return isinstance(other, WhereConstraintsAst) and self.types_to_constrain == other.types_to_constrain and self.constraints == other.constraints


@dataclass
class WhileExpressionAst(Ast, SemanticAnalysis, TypeInfer):
    while_keyword: TokenAst
    condition: ExpressionAst
    body: InnerScopeAst[StatementAst]
    else_block: Optional[WhileElseExpressionAst]

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        s = ""
        s += f"{self.while_keyword.print(printer)}{self.condition.print(printer)} {self.body.print(printer)}"
        s += f"{self.else_block.print(printer)}" if self.else_block else ""
        return s

    def do_semantic_analysis(self, scope_handler: ScopeHandler, **kwargs) -> None:
        self.condition.do_semantic_analysis(scope_handler, **kwargs)
        self.body.do_semantic_analysis(scope_handler, **kwargs)
        if self.else_block:
            self.else_block.do_semantic_analysis(scope_handler, **kwargs)

    def infer_type(self, scope_handler: ScopeHandler, **kwargs) -> Tuple[Type[ConventionAst], TypeAst]:
        return ConventionMovAst, CommonTypes.void(pos=self.pos)


@dataclass
class WithExpressionAliasAst(Ast, SemanticAnalysis):
    variable: LocalVariableAst
    assign_token: TokenAst

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        return f"{self.variable.print(printer)}{self.assign_token.print(printer)}"

    def do_semantic_analysis(self, scope_handler: ScopeHandler, with_expression: ExpressionAst = None, **kwargs) -> None:
        let_statement = LetStatementInitializedAst(
            pos=self.variable.pos,
            let_keyword=TokenAst.dummy(TokenType.KwLet),
            assign_to=self.variable,
            assign_token=self.assign_token,
            value=with_expression)
        let_statement.do_semantic_analysis(scope_handler, **kwargs)


@dataclass
class WithExpressionAst(Ast, SemanticAnalysis, TypeInfer):
    with_keyword: TokenAst
    alias: Optional[WithExpressionAliasAst]
    expression: ExpressionAst
    body: InnerScopeAst[StatementAst]

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        s = ""
        s += f"{self.with_keyword.print(printer)}"
        s += f"{self.alias.print(printer)}" if self.alias else ""
        s += f"{self.expression.print(printer)} {self.body.print(printer)}"
        return s

    def do_semantic_analysis(self, scope_handler: ScopeHandler, **kwargs) -> None:
        scope_handler.into_new_scope("<with-expression>")

        # Check that the type of object used in the "with" expression superimposes Ctx
        self.expression.do_semantic_analysis(scope_handler, **kwargs)
        object_type = self.expression.infer_type(scope_handler, **kwargs)
        object_type_sup_types = scope_handler.current_scope.get_symbol(object_type).associated_scope.sup_scopes
        if CommonTypes.ctx() not in object_type_sup_types:
            exception = SemanticError(f"Type '{object_type}' does not superimpose Ctx:")
            exception.add_traceback(self.expression.pos, f"Expression '{self.expression}' has type '{object_type}'.")
            raise exception

        # Create the symbol for the alias
        if self.alias:
            self.alias.do_semantic_analysis(scope_handler, with_expression=self.expression, **kwargs)

        self.body.do_semantic_analysis(scope_handler, **kwargs)
        scope_handler.exit_cur_scope()

    def infer_type(self, scope_handler: ScopeHandler, **kwargs) -> Tuple[Type[ConventionAst], TypeAst]:
        if self.body.members:
            return self.body.members[-1].infer_type(scope_handler, **kwargs)
        return ConventionMovAst, CommonTypes.void(pos=self.pos)


@dataclass
class YieldExpressionAst(Ast, SemanticAnalysis):
    yield_keyword: TokenAst
    with_keyword: Optional[TokenAst]
    convention: ConventionAst
    expression: ExpressionAst

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        s = ""
        s += f"{self.yield_keyword.print(printer)}"
        s += f"{self.with_keyword.print(printer)}" if self.with_keyword else ""
        s += f"{self.convention.print(printer)}{self.expression.print(printer)}"
        return s

    def do_semantic_analysis(self, scope_handler: ScopeHandler, **kwargs) -> None:
        self.expression.do_semantic_analysis(scope_handler, **kwargs)
        coroutine_return_type = kwargs.get("target-return-type")

        # Ensure that the return type is a Generator type
        if not coroutine_return_type.without_generics().symbolic_eq(CommonTypes.gen().without_generics(), scope_handler.current_scope):
            exception = SemanticError(f"Gen expressions can only occur inside a function that returns a Generator")
            exception.add_traceback(self.pos, f"Gen expression found here.")
            exception.add_traceback(coroutine_return_type.pos, f"Function returns type '{coroutine_return_type}'.")
            raise exception

        kwargs["coroutine"] = True

        # Ensure the return type's Yield typedef matches the expression's type
        # TODO: required typedefs to be implemented first


type PrimaryExpressionAst = (
        LiteralAst |
        IdentifierAst |
        LambdaPrototypeAst |
        ParenthesizedExpressionAst |
        ObjectInitializerAst |
        TypeAst |
        IfExpressionAst |
        WhileExpressionAst |
        YieldExpressionAst |
        WithExpressionAst |
        InnerScopeAst |  # [StatementAst]
        TokenAst)

type ExpressionAst = (
        BinaryExpressionAst |
        UnaryExpressionAst |
        PostfixExpressionAst |
        PrimaryExpressionAst |
        TokenAst)

type StatementAst = (
        TypedefStatementAst |
        ReturnStatementAst |
        AssignmentStatementAst |
        LetStatementAst |
        ExpressionAst)

type SupMemberAst = (
        SupMethodPrototypeAst |
        SupTypedefAst |
        ClassPrototypeAst |
        LetStatementAst |
        SupPrototypeInheritanceAst)

type ModuleMemberAst = (
        ClassPrototypeAst |
        FunctionPrototypeAst |
        TypedefStatementAst |
        SupPrototypeNormalAst |
        SupPrototypeInheritanceAst |
        LetStatementAst)


def ensure_memory_integrity_of_expression(
        expression: ExpressionAst,
        scope_handler: ScopeHandler,
        keep_consume: bool = False,
        **kwargs) -> None:

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


def verify_generic_arguments(
        generic_parameters: Seq[GenericParameterAst],
        inferred_generic_arguments: Seq[GenericArgumentAst],
        generic_arguments: Seq[GenericArgumentAst],
        obj_definition: IdentifierAst,
        usage: Ast,
        scope_handler: ScopeHandler,
        **kwargs) -> Seq[GenericArgumentAst]:

    match obj_definition:
        case FunctionPrototypeAst(): what = "Function overload"
        case ClassPrototypeAst(): what = "Class"
        case TypeSingleAst(): what = "Type"

    # TODO : change "function overload" in error messages to be based on obj_definition etc

    available_generic_parameter_names = generic_parameters.map(lambda p: p.identifier)
    required_generic_parameters = generic_parameters.filter(lambda p: isinstance(p, GenericParameterRequiredAst))

    # Check that inferred generics haven't been given explicitly:
    if inferred_generic_arguments.length + generic_arguments.length > generic_parameters.length and not isinstance(generic_parameters[-1], GenericParameterVariadicAst):
        if generic_parameters:
            exception = SemanticError(f"Generic arguments have already been inferred:")
            exception.add_traceback(generic_parameters[0].pos, f"Inferrable generic parameter '{generic_parameters[0]}' declared here.")
            exception.add_traceback_minimal(generic_arguments[0].pos, f"Generic argument '{generic_arguments[0]}' given here.")
        else:
            exception = SemanticError(f"Generic arguments have already been inferred:")
            exception.add_traceback(obj_definition.pos, f"No generic parameters declared here.")
            exception.add_traceback_minimal(generic_arguments[0].pos, f"Generic argument '{generic_arguments[0]}' given here.")
        raise exception

    # Check if there are any named generic arguments with names that don't match any generic parameter names:
    if invalid_generic_argument_names := generic_arguments.filter(lambda a: isinstance(a, GenericArgumentNamedAst)).map(lambda a: a.identifier).filter(lambda arg_name: arg_name not in available_generic_parameter_names):
        exception = SemanticError(f"Invalid generic argument names given:")
        exception.add_traceback(obj_definition.pos, f"{what} declared here with generic parameters: {available_generic_parameter_names.map(str).join(", ")}")
        exception.add_traceback_minimal(invalid_generic_argument_names[0].pos, f"Generic argument <{invalid_generic_argument_names[0]}> found here.")
        raise exception

    # Remove all named generic arguments from the available generic parameter names list:
    for generic_argument in generic_arguments.filter(lambda a: isinstance(a, GenericArgumentNamedAst)) + inferred_generic_arguments:
        available_generic_parameter_names.remove(generic_argument.identifier)

    # Check there aren't too many generic arguments provided for this overload:
    if generic_arguments.length > generic_parameters.length and not isinstance(generic_parameters[-1], GenericParameterVariadicAst):
        exception = SemanticError(f"Too many generic arguments given:")
        exception.add_traceback(obj_definition.pos, f"{what} declared here with generic parameters: {generic_parameters.map(lambda p: p.identifier).map(str).join(", ")}")
        exception.add_traceback_minimal(generic_arguments[available_generic_parameter_names.length].pos, f"{generic_arguments.length - generic_parameters.length} extra generic arguments found here.")
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
        new_generic_argument = GenericArgumentNamedAst(generic_argument.pos, generic_argument_type, available_generic_parameter_names.pop(0), TokenAst.dummy(TokenType.TkAssign))
        generic_arguments.replace(generic_argument, new_generic_argument, limit=1)
        if variadic:
            variadic_generic_argument = new_generic_argument

    # Check that all required generic parameters have been given an argument:
    if unfilled_required_generic_parameters := required_generic_parameters.map(lambda p: p.identifier).contains_any(available_generic_parameter_names):
        exception = SemanticError(f"Missing generic arguments in function call:")
        exception.add_traceback(obj_definition.pos, f"{what} declared here with required generic parameters: {available_generic_parameter_names.map(str).join(", ")}")
        exception.add_traceback_minimal(usage.pos, f"Missing generic arguments: {unfilled_required_generic_parameters.map(str).join(", ")}")
        raise exception

    return inferred_generic_arguments + generic_arguments


def infer_generic_argument_values(
        scope_handler: ScopeHandler,
        generic_parameters: Seq[GenericParameterAst],
        infer_from: Seq[TypeAst],
        replace_with: Seq[TypeAst],
        obj_definition: IdentifierAst) -> Seq[GenericArgumentNamedAst]:

    # print(f"obj: {obj_definition}")
    # print(f"generic parameters: {generic_parameters}")
    # print(f"infer from: {infer_from}")
    # print(f"replace with: {replace_with}")

    # Infer any generic type arguments that can be inferred (from parameter types etc)
    # Return a list of non-inferred generic argument types.
    inferred_generic_parameters = Seq([])
    for generic_parameter in generic_parameters:
        for parameter_t, argument_t in infer_from.zip(replace_with):
            # Check if the parameter type or any of its nested generic argument names matches the generic parameter.
            parameter_type_parts = Seq(list(iter(parameter_t)))
            if generic_parameter.identifier not in parameter_type_parts:
                continue

            # Check if the generic argument has already been inferred.
            # print("I", inferred_generic_parameters, generic_parameter, parameter_t, argument_t)
            duplicate_inferred_parameter = inferred_generic_parameters.find(lambda p: p.identifier == generic_parameter.identifier)
            if duplicate_inferred_parameter and not duplicate_inferred_parameter.type.symbolic_eq(argument_t, scope_handler.current_scope):
                exception = SemanticError(f"Generic argument has already been inferred:")
                exception.add_traceback(obj_definition.pos, f"")
                exception.add_traceback_minimal(argument_t.pos, f"Generic argument of type '{argument_t}' has already been inferred as '{duplicate_inferred_parameter.type}'.")
                raise exception

            argument_t_namespace = Seq(list(iter(argument_t))).filter(lambda p: isinstance(p, IdentifierAst)).value
            if not duplicate_inferred_parameter:
                for p_1, p_2 in zip(iter(parameter_t), iter(argument_t)):
                    if generic_parameter.identifier == p_1:
                        inferred_generic_parameters.append(GenericArgumentNamedAst(p_2.pos, p_2, generic_parameter.identifier, TokenAst.dummy(TokenType.TkAssign)))
                        break

    return inferred_generic_parameters
