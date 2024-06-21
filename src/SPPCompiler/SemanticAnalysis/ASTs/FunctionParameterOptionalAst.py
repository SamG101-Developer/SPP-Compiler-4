from dataclasses import dataclass

from SPPCompiler.SemanticAnalysis.ASTMixins.SemanticAnalyser import SemanticAnalyser
from SPPCompiler.SemanticAnalysis.ASTs.Meta.Ast import Ast
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstPrinter import *
from SPPCompiler.SemanticAnalysis.Utils.SemanticError import SemanticErrors
from SPPCompiler.SemanticAnalysis.Utils.Symbols import MemoryStatus


@dataclass
class FunctionParameterOptionalAst(Ast, SemanticAnalyser):
    """
    The FunctionParameterOptionalAst node represents an optional parameter of a function. This is a parameter that can
    be given an argument when calling the function. The parameter cannot have a convention (ie must be mov/copy). The type
    declaration is still required despite a default value being provided; this is a design decision.

    Attributes:
        variable: The variable of the parameter.
        colon_token: The colon token.
        convention: The convention of the parameter.
        type_declaration: The type declaration of the parameter.
        assignment_token: The assignment token.
        default_value: The default value of the parameter.
    """

    variable: "LocalVariableAst"
    colon_token: "TokenAst"
    convention: "ConventionAst"
    type_declaration: "TypeAst"
    assignment_token: "TokenAst"
    default_value: "ExpressionAst"

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        # Print the FunctionParameterOptionalAst.
        s = ""
        s += f"{self.variable.print(printer)}{self.colon_token.print(printer)} "
        s += f"{self.convention.print(printer)}{self.type_declaration.print(printer)} "
        s += f"{self.assignment_token.print(printer)} {self.default_value.print(printer)}"
        return s

    def do_semantic_analysis(self, scope_handler, **kwargs) -> None:
        from SPPCompiler.LexicalAnalysis.Tokens import TokenType
        from SPPCompiler.SemanticAnalysis.ASTs import (
            LetStatementUninitializedAst, TokenAst, ConventionMovAst, ConventionRefAst, ConventionMutAst, TypeAst)

        self.type_declaration.do_semantic_analysis(scope_handler, **kwargs)

        # Convert the parameter to a "let" statement.
        let_statement = LetStatementUninitializedAst(
            pos=self.pos,
            let_keyword=TokenAst.dummy(TokenType.KwLet),
            assign_to=self.variable,
            colon_token=self.colon_token,
            type_declaration=self.type_declaration)
        let_statement.do_semantic_analysis(scope_handler, **kwargs)

        # Set the symbol's memory status depending on the convention.
        symbol = scope_handler.current_scope.get_symbol(self.variable.identifier)
        symbol.memory_info = MemoryStatus(
            is_borrow_ref=isinstance(self.convention, ConventionRefAst),
            is_borrow_mut=isinstance(self.convention, ConventionMutAst),
            ast_borrow=self.convention,
            ast_initialized=self)

        # Check the convention is not a borrow convention.
        if not isinstance(self.convention, ConventionMovAst):
            raise SemanticErrors.OPTIONAL_PARAM_REQUIRES_MOV_CONVENTION(self.convention)

        # Check the default value is not a type.
        if isinstance(self.default_value, TypeAst):
            raise SemanticErrors.INVALID_USE_OF_TYPE_AS_EXPR(self.default_value)

        # Analyse the default value
        self.default_value.do_semantic_analysis(scope_handler, **kwargs)
        default_value_type = self.default_value.infer_type(scope_handler, **kwargs).type
        if not self.type_declaration.symbolic_eq(default_value_type, scope_handler.current_scope):
            raise SemanticErrors.TYPE_MISMATCH(self, self.type_declaration, default_value_type, symbol)

    def identifier_for_param(self) -> "IdentifierAst":
        from SPPCompiler.SemanticAnalysis.ASTs import LocalVariableSingleAst, IdentifierAst
        match self.variable:
            case LocalVariableSingleAst(): return self.variable.identifier
            case _: return IdentifierAst(self.pos, "UNMATCHABLE")

    def __eq__(self, other):
        # Check both ASTs are the same type and have the same identifier.
        return isinstance(other, FunctionParameterOptionalAst) and self.variable == other.variable


__all__ = ["FunctionParameterOptionalAst"]
