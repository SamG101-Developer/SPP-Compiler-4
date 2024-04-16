import copy
from dataclasses import dataclass
from typing import List, Tuple, Type

from src.LexicalAnalysis.Tokens import TokenType

from src.SemanticAnalysis.ASTMixins.SemanticAnalyser import SemanticAnalyser
from src.SemanticAnalysis.Utils.SemanticError import SemanticError, SemanticErrorType
from src.SemanticAnalysis.Utils.CommonTypes import CommonTypes
from src.SemanticAnalysis.ASTMixins.TypeInfer import TypeInfer
from src.SemanticAnalysis.Utils.Scopes import ScopeHandler

from src.SemanticAnalysis.ASTs.Meta.Ast import Ast
from src.SemanticAnalysis.ASTs.Meta.AstPrinter import *
from src.SemanticAnalysis.ASTs.Meta.AstUtils import AstUtils

from src.SemanticAnalysis.ASTs.ConventionNonInitAst import ConventionNonInitAst
from src.SemanticAnalysis.ASTs.ConventionMovAst import ConventionMovAst
from src.SemanticAnalysis.ASTs.IdentifierAst import IdentifierAst
from src.SemanticAnalysis.ASTs.PostfixExpressionAst import PostfixExpressionAst
from src.SemanticAnalysis.ASTs.PostfixExpressionOperatorMemberAccessAst import PostfixExpressionOperatorMemberAccessAst

from src.Utils.Sequence import Seq


@dataclass
class AssignmentStatementAst(Ast, SemanticAnalyser, TypeInfer):
    """
    The AssignmentStatementAst node is used to represent a variable being assigned a value. The LHS can be any
    expression but is semantically analysed to ensure that it is a variable or attribute being assigned to. There can be
    multiple LHSs, but only if the "=" token is used; this is also enforced in the semantic analysis.

    Attributes:
        - lhs: The left-hand-side of the assignment, the variable or attribute being assigned to.
        - op: The operator being used in the assignment, ie "=" or "+=".
        - rhs: The right-hand-side of the assignment, the value being assigned to the LHS.
    """

    lhs: List["ExpressionAst"]
    op: "TokenAst"
    rhs: "ExpressionAst"

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        # Print the AssignmentStatementAst.
        s = ""
        s += f"{Seq(self.lhs).print(printer, ", ")} "
        s += f"{self.op.print(printer)} "
        s += f"{self.rhs.print(printer)}"
        return s

    def do_semantic_analysis(self, scope_handler: ScopeHandler, **kwargs) -> None:
        AstUtils.ensure_memory_integrity_of_expression(self.rhs, scope_handler, **kwargs)

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
                    temp = lhs
                    while isinstance(temp, PostfixExpressionAst):
                        temp = temp.lhs

                    sym = scope_handler.current_scope.get_symbol(temp)
                    lhs_symbols.append(sym)

                case _:
                    raise SemanticError().add_error(
                        pos=lhs.pos,
                        error_type=SemanticErrorType.VALUE_ERROR,
                        message=f"Invalid assignment target (must be an identifier).",
                        tag_message=f"Assignment target '{lhs}' is a '{lhs.__class__.__name__}'",
                        tip="Ensure the assignment target is a variable or attribute.")

        # Regular assignment with the "=" operator. Compound assignment, ie "+=" is not supported for semantic analysis
        # yet.
        if self.op.token.token_type == TokenType.TkAssign:
            for i, lhs_symbol in enumerate(lhs_symbols):

                # If the symbol isn't mutable or is the "&" borrow type, then this symbol cannot be mutated.
                # TODO: this is slightly wrong: it won't allow "&" variables to be re-assigned but it should.
                # TODO: it  should prevent "&" from being used in "&mut" i think? maybe just remove the "or ..."?
                # if (not (lhs_symbol.is_mutable or lhs_symbol.memory_info.is_borrow_mut) or lhs_symbol.memory_info.is_borrow_ref) and lhs_symbol.memory_info.ast_initialized:

                # Mutation to identifiers requires a "mut" variable declaration, and mutation to attributes requires
                # either a "mut" variable declaration or a "&mut" borrow declaration.
                if lhs_symbol.memory_info.ast_initialized and (
                        isinstance(self.lhs[i], IdentifierAst) and not lhs_symbol.is_mutable
                        or isinstance(self.lhs[i], PostfixExpressionAst) and lhs_symbol.memory_info.is_borrow_ref
                        or isinstance(self.lhs[i], PostfixExpressionAst) and not lhs_symbol.is_mutable and not lhs_symbol.memory_info.is_borrow_mut):

                    raise SemanticError().add_error(
                        pos=self.pos,
                        error_type=SemanticErrorType.VALUE_ERROR,
                        message="Cannot assign to an immutable variable.",
                        tag_message=f"Object {lhs_symbol.name} declared as: '{lhs_symbol.memory_info.ast_initialized}'",
                        tip=f"Try declaring '{lhs_symbol.name}' with the 'mut' keyword.")

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
                    raise SemanticError().add_error(
                        pos=self.rhs.pos,
                        error_type=SemanticErrorType.TYPE_ERROR,
                        message=f"Type mismatch in assignment",
                        tag_message=f"Value inferred as '{rhs_type[1]}' (!= '{lhs_type[1]}')",
                        tip=f"Ensure the RHS is a '{lhs_type[1]}' object")

            else:
                raise NotImplementedError()

        else:
            raise NotImplementedError()

    def infer_type(self, scope_handler: ScopeHandler, **kwargs) -> Tuple[Type["ConventionAst"], "TypeAst"]:
        # Assignment never returns anything, so return the Void type. This is so that the memory rules of the language
        # can be adhered to.
        return ConventionMovAst, CommonTypes.void()


__all__ = ["AssignmentStatementAst"]
