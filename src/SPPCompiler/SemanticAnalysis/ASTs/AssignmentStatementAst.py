from dataclasses import dataclass
from typing import List

from SPPCompiler.SemanticAnalysis.ASTMixins.SemanticAnalyser import SemanticAnalyser
from SPPCompiler.SemanticAnalysis.ASTMixins.TypeInfer import TypeInfer, InferredType
from SPPCompiler.SemanticAnalysis.ASTs.Meta.Ast import Ast
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstPrinter import *
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstUtils import ensure_memory_integrity
from SPPCompiler.SemanticAnalysis.Utils.CommonTypes import CommonTypes
from SPPCompiler.SemanticAnalysis.Utils.Scopes import ScopeHandler
from SPPCompiler.SemanticAnalysis.Utils.SemanticError import SemanticErrors
from SPPCompiler.Utils.Sequence import Seq


@dataclass
class AssignmentStatementAst(Ast, SemanticAnalyser, TypeInfer):
    """
    The AssignmentStatementAst node is used to represent a variable being assigned a value. The LHS can be any
    expression but is semantically analysed to ensure that it is a variable or attribute being assigned to. There can be
    multiple LHSs, but only if the "=" token is used; this is also enforced in the semantic analysis.

    Attributes:
        lhs: The left-hand-side of the assignment, the variable or attribute being assigned to.
        op: The operator being used in the assignment, ie "=" or "+=".
        rhs: The right-hand-side of the assignment, the value assigned to the LHS.
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
        from SPPCompiler.SemanticAnalysis.ASTs import IdentifierAst, PostfixExpressionAst

        lhs_symbols = []

        # Create a list of symbols for each element on the lhs of the assignment. This ensures that only variables or
        # attributes are assigned to. For attributes, the symbol gotten is the outermost symbol, for example: for
        # "x.y.z = 1", the symbol got is the "x" symbol.
        for lhs in self.lhs:
            lhs.do_semantic_analysis(scope_handler, **kwargs)
            symbol = scope_handler.current_scope.get_outermost_variable_symbol(lhs)
            lhs_symbols.append(symbol)
            if not symbol:
                raise SemanticErrors.INVALID_LHS_EXPR(lhs)

        for i, lhs_symbol in Seq(lhs_symbols).enumerate():
            # Mutation requires either a mutable identifier, or an outermost mutable value. This can be an "&mut"
            # borrow, or a "mut" value. The const-ness of a borrow trumps the value's mutability for attributes:
            # for "a.b", if "a" is a "&A" type, then even "mut a.b.c" can not have its "b" mutated.
            immutable = any([
                isinstance(self.lhs[i], IdentifierAst) and not lhs_symbol.is_mutable,
                isinstance(self.lhs[i], PostfixExpressionAst) and lhs_symbol.memory_info.is_borrow_ref,
                isinstance(self.lhs[i], PostfixExpressionAst) and not (lhs_symbol.memory_info.is_borrow_mut or lhs_symbol.is_mutable)])
            if lhs_symbol.memory_info.ast_initialized and immutable:
                raise SemanticErrors.CANNOT_MUTATE_IMMUTABLE_SYM(self.lhs[i], lhs_symbol)

            # Type-check the assignment, with a symbolic equality check. This is to ensure that the types are the same,
            # and the conventions match, for example, "&String == &String", or "&mut String == &mut String".
            if len(self.lhs) == 1:
                lhs_type = self.lhs[0].infer_type(scope_handler, **kwargs)
                rhs_type = self.rhs.infer_type(scope_handler, **kwargs)
                if not lhs_type.symbolic_eq(rhs_type, scope_handler):
                    raise SemanticErrors.TYPE_MISMATCH(self.rhs, lhs_type, rhs_type, lhs_symbol)
            else:
                raise NotImplementedError("Multiple assignment not yet implemented.")

            # Resolve partial moves and full moves, by marking the identifiers as initialized, and removing partial
            # moves from the outermost symbol's list.
            match self.lhs[i]:
                case IdentifierAst() if not lhs_symbol.memory_info.ast_initialized:
                    lhs_symbol.memory_info.ast_initialized = self
                    lhs_symbol.memory_info.ast_consumed = None

                case PostfixExpressionAst():
                    lhs_symbol.memory_info.ast_partial_moves = Seq(lhs_symbol.memory_info.ast_partial_moves).filter(lambda x: x != self.lhs[i]).value

        # Check the value being assigned is initialised.
        ensure_memory_integrity(self, self.rhs, self.op, scope_handler)

    def infer_type(self, scope_handler: ScopeHandler, **kwargs) -> InferredType:
        from SPPCompiler.SemanticAnalysis.ASTs.ConventionMovAst import ConventionMovAst

        # Assignment never returns anything, so return the Void type. This is so that the memory rules of the language
        # can be adhered to.
        return InferredType(convention=ConventionMovAst, type=CommonTypes.void())


__all__ = ["AssignmentStatementAst"]
