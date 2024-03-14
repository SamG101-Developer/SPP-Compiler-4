from dataclasses import dataclass
from typing import List, Tuple, Type

from src.LexicalAnalysis.Tokens import TokenType

from src.SemanticAnalysis.Analysis.SemanticAnalysis import SemanticAnalysis
from src.SemanticAnalysis.Analysis.SemanticError import SemanticError
from src.SemanticAnalysis.Types.CommonTypes import CommonTypes
from src.SemanticAnalysis.Types.TypeInfer import TypeInfer
from src.SemanticAnalysis.Symbols.Scopes import ScopeHandler

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
class AssignmentStatementAst(Ast, SemanticAnalysis, TypeInfer):
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
                    exception.add_traceback(lhs_symbols[0].memory_info.ast_initialized.pos, f"Assignment target '{self.lhs[0]}' declared here with type '{lhs_type[0]}{lhs_type[1]}'.")  # TODO : should be symbol's initialization AST
                    exception.add_traceback(self.rhs.pos, f"Assignment value '{self.rhs}' inferred here with type '{rhs_type[0]}{rhs_type[1]}'.")
                    raise exception

            else:
                raise NotImplementedError()

        else:
            raise NotImplementedError()

    def infer_type(self, scope_handler: ScopeHandler, **kwargs) -> Tuple[Type["ConventionAst"], "TypeAst"]:
        # Assignment never returns anything, so return the Void type. This is so that the memory rules of the language
        # can be adhered to.
        return ConventionMovAst, CommonTypes.void()


__all__ = ["AssignmentStatementAst"]
