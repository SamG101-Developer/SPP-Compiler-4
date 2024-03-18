from dataclasses import dataclass, field

from src.LexicalAnalysis.Tokens import TokenType

from src.SemanticAnalysis.ASTMixins.SemanticAnalyser import SemanticAnalyser
from src.SemanticAnalysis.Utils.Scopes import ScopeHandler
from src.SemanticAnalysis.Utils.Symbols import VariableSymbol, MemoryStatus
from src.SemanticAnalysis.ASTMixins.SymbolGeneration import SymbolGenerator
from src.SemanticAnalysis.ASTMixins.PreProcessor import PreProcessor

from src.SemanticAnalysis.ASTs.Meta.Ast import Ast
from src.SemanticAnalysis.ASTs.Meta.AstPrinter import *
from src.SemanticAnalysis.ASTs.Meta.AstUtils import AstUtils

from src.SemanticAnalysis.ASTs.TokenAst import TokenAst
from src.SemanticAnalysis.ASTs.LocalVariableAst import LocalVariableAst
from src.SemanticAnalysis.ASTs.LocalVariableSingleAst import LocalVariableSingleAst
from src.SemanticAnalysis.ASTs.LocalVariableTupleAst import LocalVariableTupleAst
from src.SemanticAnalysis.ASTs.LocalVariableDestructureAst import LocalVariableDestructureAst
from src.SemanticAnalysis.ASTs.LocalVariableSkipArgumentAst import LocalVariableSkipArgumentAst
from src.SemanticAnalysis.ASTs.ExpressionAst import ExpressionAst
from src.SemanticAnalysis.ASTs.TypeAst import TypeAst
from src.SemanticAnalysis.ASTs.PostfixExpressionAst import PostfixExpressionAst
from src.SemanticAnalysis.ASTs.PostfixExpressionOperatorMemberAccessAst import PostfixExpressionOperatorMemberAccessAst


@dataclass
class LetStatementInitializedAst(Ast, PreProcessor, SymbolGenerator, SemanticAnalyser):
    """
    The LetStatementInitializedAst node is used to represent a variable being initialized with a value. The variable
    could be a single variable, a tuple or a destructure. Recursive destructuring is supported.

    Attributes:
        - let_keyword: The `let` keyword token.
        - assign_to: The variable being assigned to.
        - assign_token: The assignment token.
        - value: The value being assigned to the variable.

        - _sup_let_type: For function preprocessing.
    """

    let_keyword: TokenAst
    assign_to: LocalVariableAst
    assign_token: TokenAst
    value: ExpressionAst
    _sup_let_type: TypeAst = field(default=None, kw_only=True)

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        # Print the LetStatementInitializedAst.
        s = ""
        s += f"{self.let_keyword.print(printer)}"
        s += f"{self.assign_to.print(printer)} "
        s += f"{self.assign_token.print(printer)} "
        s += f"{self.value.print(printer)}"
        return s

    def pre_process(self, context) -> None:
        # There is no preprocessing required for a let statement.
        # TODO: why inherit from PreProcessor?
        pass

    def generate(self, s: ScopeHandler) -> None:
        # Generate the symbol for the variable being assigned to. This is only used for function preprocessing.
        variable_symbol = VariableSymbol(self.assign_to.identifier, self._sup_let_type)
        s.current_scope.add_symbol(variable_symbol)

    # TODO: tidy up this method
    def do_semantic_analysis(self, scope_handler, **kwargs) -> None:
        match self.assign_to:
            case LocalVariableSingleAst():
                # Create the variable symbol for the single assignment target on the LHS. Some rhs types are not known
                # until Semantic Analysis has taken place, so defer the type until then. This is fine, because a symbol
                # can never refer to itself in its own definition, so nothing will try to get the type of the symbol
                # until the symbol has been defined with a type.
                sym = VariableSymbol(
                    name=self.assign_to.identifier,
                    type=None,
                    is_mutable=self.assign_to.is_mutable is not None,
                    memory_info=MemoryStatus(ast_initialized=self.assign_to.identifier))

                # Add the new variable symbol to the current scope.
                scope_handler.current_scope.add_symbol(sym)

                # Mark future operations from this caller as "assignment"
                kwargs |= {"assignment": True}

                # For a normal "let" statement, ensure the RHS is an owned object.
                if not self._sup_let_type:
                    AstUtils.ensure_memory_integrity_of_expression(self.value, scope_handler, keep_consume=True, **kwargs)
                    sym.type = self.value.infer_type(scope_handler, **kwargs)[1]

                # For function preprocessing, just analyse the RHS (in this case, the RHS is guaranteed to be owned)
                else:
                    self.value.do_semantic_analysis(scope_handler, **kwargs)
                    sym.type = self.value.infer_type(scope_handler, **kwargs)[1]

                # Mark "assignment" as complete by removing the "assignment" flag
                kwargs.pop("assignment")

            case LocalVariableTupleAst():
                # Check there are the same number of elements on the LHS as the RHS
                # TODO: move into LocalVariableTupleAst
                # rhs_tuple_type_elements = self.value.infer_type(scope_handler, **kwargs)[1].parts[-1].generic_arguments.arguments
                # if len(self.assign_to.items) != len(rhs_tuple_type_elements):
                #     exception = SemanticError(f"Invalid tuple assignment:")
                #     exception.add_traceback(self.assign_to.pos, f"Assignment target tuple contains {len(self.assign_to.items)} elements.")
                #     exception.add_traceback(self.value.pos, f"Assignment value tuple contains {len(rhs_tuple_type_elements)} elements.")
                #     raise exception

                self.assign_to.do_semantic_analysis(scope_handler, other_tuple=self.value, **kwargs)

                # Form new let statements based off the variables the RHS is being destructured into.
                new_let_statements = []
                for i, current_let_statement in enumerate(self.assign_to.items):

                    # Skip over the ".." and "_" skip arguments (if there are any).
                    if isinstance(current_let_statement, LocalVariableSkipArgumentAst):
                        pass

                    # The new RHS for each argument is a numeric postfix member access of the original RHS.
                    new_rhs = PostfixExpressionAst(
                        pos=self.pos,
                        lhs=self.value,
                        op=PostfixExpressionOperatorMemberAccessAst(
                            pos=self.pos,
                            dot_token=TokenAst.dummy(TokenType.TkDot),
                            identifier=TokenAst.dummy(TokenType.LxDecDigits, info=str(i))))

                    # Create a new "let" statement using the new RHS, assigning it to the current LHS variable.
                    new_let_statement = LetStatementInitializedAst(
                        pos=self.pos,
                        let_keyword=self.let_keyword,
                        assign_to=current_let_statement,
                        assign_token=self.assign_token,
                        value=new_rhs,
                        _sup_let_type=self._sup_let_type)

                    new_let_statements.append(new_let_statement)

                # Analyse all the new "let" statements.
                for new_let_statement in new_let_statements:
                    new_let_statement.do_semantic_analysis(scope_handler, **kwargs)

            case LocalVariableDestructureAst():
                # Check the LHS destructure is valid by ensuring that the class type has all the required fields.
                self.assign_to.do_semantic_analysis(scope_handler, **kwargs)

                # Form new let statements based off the variables the RHS is being destructured into.
                new_let_statements = []
                for current_let_statement in self.assign_to.items:

                    # Skip over the ".." skip argument (if there is one).
                    if isinstance(current_let_statement, LocalVariableSkipArgumentAst):
                        continue

                    # The new RHS for each argument is a postfix member access of the original RHS.
                    new_rhs = PostfixExpressionAst(
                        pos=self.pos,
                        lhs=self.value,
                        op=PostfixExpressionOperatorMemberAccessAst(
                            pos=self.pos,
                            dot_token=TokenAst.dummy(TokenType.TkDot),
                            identifier=current_let_statement.identifier))

                    # Create a new "let" statement using the new RHS, assigning it to the current LHS variable.
                    new_let_statement = LetStatementInitializedAst(
                        pos=self.pos,
                        let_keyword=self.let_keyword,
                        assign_to=current_let_statement,
                        assign_token=self.assign_token,
                        value=new_rhs,
                        _sup_let_type=self._sup_let_type)

                    # Save the new "let" statement for later semantic analysis.
                    new_let_statements.append(new_let_statement)

                # Analyse all the new "let" statements.
                for new_let_statement in new_let_statements:
                    new_let_statement.do_semantic_analysis(scope_handler, **kwargs)


__all__ = ["LetStatementInitializedAst"]
