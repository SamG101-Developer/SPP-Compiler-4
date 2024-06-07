from __future__ import annotations

import copy
from dataclasses import dataclass, field
from typing import List, Optional

from SPPCompiler.LexicalAnalysis.Tokens import TokenType

from SPPCompiler.SemanticAnalysis.ASTMixins.SemanticAnalyser import SemanticAnalyser
from SPPCompiler.SemanticAnalysis.Utils.SemanticError import SemanticError, SemanticErrorType
from SPPCompiler.SemanticAnalysis.Utils.Scopes import ScopeHandler
from SPPCompiler.SemanticAnalysis.ASTMixins.SymbolGeneration import SymbolGenerator
from SPPCompiler.SemanticAnalysis.Utils.Symbols import TypeSymbol
from SPPCompiler.SemanticAnalysis.Utils.CommonTypes import CommonTypes
from SPPCompiler.SemanticAnalysis.ASTMixins.PreProcessor import PreProcessor

from SPPCompiler.SemanticAnalysis.ASTs.Meta.Ast import Ast
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstPrinter import *

from SPPCompiler.SemanticAnalysis.ASTs.ClassPrototypeAst import ClassPrototypeAst
from SPPCompiler.SemanticAnalysis.ASTs.ConventionRefAst import ConventionRefAst
from SPPCompiler.SemanticAnalysis.ASTs.ConventionMovAst import ConventionMovAst
from SPPCompiler.SemanticAnalysis.ASTs.ConventionMutAst import ConventionMutAst
from SPPCompiler.SemanticAnalysis.ASTs.FunctionParameterSelfAst import FunctionParameterSelfAst
from SPPCompiler.SemanticAnalysis.ASTs.GenericIdentifierAst import GenericIdentifierAst
from SPPCompiler.SemanticAnalysis.ASTs.GenericParameterGroupAst import GenericParameterGroupAst
from SPPCompiler.SemanticAnalysis.ASTs.IdentifierAst import IdentifierAst
from SPPCompiler.SemanticAnalysis.ASTs.InnerScopeAst import InnerScopeAst
from SPPCompiler.SemanticAnalysis.ASTs.LetStatementInitializedAst import LetStatementInitializedAst
from SPPCompiler.SemanticAnalysis.ASTs.LocalVariableSingleAst import LocalVariableSingleAst
from SPPCompiler.SemanticAnalysis.ASTs.ModulePrototypeAst import ModulePrototypeAst
from SPPCompiler.SemanticAnalysis.ASTs.ObjectInitializerAst import ObjectInitializerAst
from SPPCompiler.SemanticAnalysis.ASTs.ObjectInitializerArgumentGroupAst import ObjectInitializerArgumentGroupAst
from SPPCompiler.SemanticAnalysis.ASTs.ReturnStatementAst import ReturnStatementAst
from SPPCompiler.SemanticAnalysis.ASTs.SupPrototypeInheritanceAst import SupPrototypeInheritanceAst
from SPPCompiler.SemanticAnalysis.ASTs.TokenAst import TokenAst
from SPPCompiler.SemanticAnalysis.ASTs.TypeSingleAst import TypeSingleAst
from SPPCompiler.SemanticAnalysis.ASTs.WhereBlockAst import WhereBlockAst

from SPPCompiler.Utils.Sequence import Seq


@dataclass
class FunctionPrototypeAst(Ast, PreProcessor, SymbolGenerator, SemanticAnalyser):
    """
    The FunctionPrototypeAst node is an AST node that represents a function prototype. This includes the function's
    annotations, identifier, generic parameters, parameters, return type, where block, and body. The function prototype
    is used to create a function in the module or sup prototype.

    Attributes:
        - annotations: The annotations for the function.
        - fun_token: The "fun" keyword token.
        - identifier: The identifier for the function.
        - generic_parameters: The generic parameters for the function.
        - parameters: The parameters for the function.
        - arrow_token: The "->" token.
        - return_type: The return type for the function.
        - where_block: The where block for the function.
        - body: The body of the function.

        - _fn_type: The type of the function (Fn[Mov|Mut|Ref]).
        - _orig: The original identifier of the function (replaced by call_[mov|mut|ref] etc).
        - _ctx: The context of the function (module or sup prototype).
        - _specializations: The specializations of the function (generic substitutions).
    """

    annotations: List["AnnotationAst"]
    fun_token: "TokenAst"
    identifier: "IdentifierAst"
    generic_parameters: Optional["GenericParameterGroupAst"]
    parameters: "FunctionParameterGroupAst"
    arrow_token: "TokenAst"
    return_type: "TypeAst"
    where_block: Optional["WhereBlockAst"]
    body: InnerScopeAst["StatementAst"]

    _fn_type: "TypeAst" = field(default=None, kw_only=True)
    _orig: "IdentifierAst" = field(default=None, kw_only=True)
    _ctx: "ModulePrototypeAst | SupPrototypeAst" = field(default=None, kw_only=True)
    _specializations: List["FunctionPrototypeAst"] = field(default_factory=list, kw_only=True)
    _is_coro: bool = field(default=False, init=False)

    def __post_init__(self):
        self.generic_parameters = self.generic_parameters or GenericParameterGroupAst.default()
        self.where_block = self.where_block or WhereBlockAst.default()

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        # Print the FunctionPrototypeAst.
        s = ""
        s += f"{Seq(self.annotations).print(printer, "\n")}{self.fun_token.print(printer)}{self.identifier.print(printer)}"
        s += f"{self.generic_parameters.print(printer)}" if self.generic_parameters else ""
        s += f"{self.parameters.print(printer)} {self.arrow_token.print(printer)} {self.return_type.print(printer)}"
        s += f" {self.where_block.print(printer)}" if self.where_block else ""
        s += f"{self.body.print(printer)}"
        return s

    def pre_process(self, context: "ModulePrototypeAst | SupPrototypeAst") -> None:
        self._ctx = context

        # For functions that are methods (ie inside a "sup" block), substitute the "Self" type from generic parameters,
        # function parameters, and the return type.
        if not isinstance(context, ModulePrototypeAst):
            Seq(self.generic_parameters.get_opt()).for_each(lambda p: p.default_value.substitute_generics(CommonTypes.self(), context.identifier))
            Seq(self.parameters.parameters).for_each(lambda p: p.type_declaration.substitute_generics(CommonTypes.self(), context.identifier))
            self.return_type.substitute_generics(CommonTypes.self(), context.identifier)

        # Convert the "fun ..." to a "Fun___" superimposition over a type representing the function class. This allows
        # for the first-class nature of functions. The mock object for "fun function" will be "MOCK_function".
        mock_class_name = IdentifierAst(self.pos, f"MOCK_{self.identifier.value}")
        mock_class_name = TypeSingleAst(self.pos, [GenericIdentifierAst(self.pos, mock_class_name.value, None)])

        # Determine the class type and call name. This will be "FunRef/call_ref", "FunMut/call_mut" or
        # "FunMov/call_mov".
        function_class_type = self._deduce_function_class_type(context)
        function_call_name  = self._deduce_function_call_name(function_class_type)

        # If the mock class name ("MOCK_function") doesn't exist as a class, then this if the first instance of a
        # function with this name seen. Therefore, the class needs to be added into the module prototype.
        if Seq(context.body.members).filter(lambda m: isinstance(m, ClassPrototypeAst) and m.identifier == mock_class_name).empty():

            # Create the mock class prototype.
            # cls MOCK_function {}
            mock_class_ast = ClassPrototypeAst(
                pos=self.pos,
                annotations=[],
                class_token=TokenAst.dummy(TokenType.KwCls),
                identifier=mock_class_name,
                generic_parameters=None,
                where_block=None,
                body=InnerScopeAst(
                    pos=self.pos,
                    brace_l_token=TokenAst.dummy(TokenType.TkBraceL),
                    members=[],
                    brace_r_token=TokenAst.dummy(TokenType.TkBraceR)))

            # Create the let statement, which brings an instance of this class into scope.
            # let function = MOCK_function()
            mock_let_statement = LetStatementInitializedAst(
                pos=self.pos,
                let_keyword=TokenAst.dummy(TokenType.KwLet),
                assign_to=LocalVariableSingleAst(
                    pos=self.pos,
                    is_mutable=None,
                    unpack_token=None,
                    identifier=copy.deepcopy(self.identifier)),
                assign_token=TokenAst.dummy(TokenType.TkAssign),
                value=ObjectInitializerAst(
                    pos=self.pos,
                    class_type=copy.deepcopy(mock_class_name),
                    arguments=ObjectInitializerArgumentGroupAst(
                        pos=self.pos,
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
        call_method_ast = FunctionPrototypeAst(
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
            pos=self.pos,
            sup_keyword=TokenAst.dummy(TokenType.KwSup),
            generic_parameters=self.generic_parameters,
            super_class=copy.deepcopy(function_class_type),
            on_keyword=TokenAst.dummy(TokenType.KwOn),
            identifier=mock_class_name,
            where_block=self.where_block,
            body=InnerScopeAst(
                pos=self.pos,
                brace_l_token=TokenAst.dummy(TokenType.TkBraceL),
                members=[call_method_ast],
                brace_r_token=TokenAst.dummy(TokenType.TkBraceR)))

        # Append the "sup" block to the module or sup prototype ("context" will be either one).
        context.body.members.append(sup_block_ast)
        self._fn_type = function_class_type

    def _deduce_function_class_type(self, context: "ModulePrototypeAst | SupPrototypeAst") -> "TypeAst":
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

    def _deduce_function_call_name(self, function_class_type: "TypeAst") -> IdentifierAst:
        # Map the function class type to a function call name with a simple match-case statement.
        match function_class_type.parts[-1].value:
            case "FunRef": return IdentifierAst(self.identifier.pos, "call_ref")
            case "FunMut": return IdentifierAst(self.identifier.pos, "call_mut")
            case "FunMov": return IdentifierAst(self.identifier.pos, "call_mov")
            case _: raise SystemExit(f"Unknown function class type '{function_class_type}' being deduced. Report as bug.")

    def generate(self, scope_handler: ScopeHandler) -> None:
        # Create and move into a new scope for the function prototype's scope. Within this scope, generate type symbols
        # for each generic parameter. Exit the newly created function scope.
        scope_handler.into_new_scope(f"<function:{self._orig}>")
        Seq(self.generic_parameters.parameters).for_each(lambda p: scope_handler.current_scope.add_symbol(TypeSymbol(name=p.identifier, type=None)))
        scope_handler.exit_cur_scope()

    def do_semantic_analysis(self, scope_handler, **kwargs) -> None:
        scope_handler.move_to_next_scope()

        # Analyse the generic type parameters, the function parameters and the return type, in this order. This allows
        # the function parameter types and return type to use the generic type parameters.
        self.generic_parameters.do_semantic_analysis(scope_handler, **kwargs)
        self.parameters.do_semantic_analysis(scope_handler, **kwargs)
        self.return_type.do_semantic_analysis(scope_handler, **kwargs)

        # Add the "target-return-type" to the kwargs, so other ASTs can use the function's return type is required.
        # Analyse the body of the function, and then pop the return type from the kwargs.
        kwargs |= {"target-return-type": self.return_type}
        self.body.do_semantic_analysis(scope_handler, inline=True, **kwargs)
        kwargs.pop("target-return-type")

        # Check that a "ret" statement exists at the end of the function, as long as it is a subroutine with a non-Void
        # return type.
        if (not self._is_coro
                and not self.return_type.symbolic_eq(CommonTypes.void(), scope_handler.current_scope)
                and self.body.members
                and not isinstance(self.body.members[-1], ReturnStatementAst)):
            exception = SemanticError()
            exception.add_error(
                pos=self.pos, error_type=SemanticErrorType.TYPE_ERROR,
                tag_message="Return statement expected here.",
                message="Missing return statement at the end of the function.",
                tip="Ensure that the function returns a value.")
            raise exception

        scope_handler.exit_cur_scope()

    def __eq__(self, other):
        # Check both ASTs are the same type and have the same generic parameters, parameters, return type, and where block.
        return self.identifier == other.identifier and self.generic_parameters == other.generic_parameters and self.parameters == other.parameters and self.return_type == other.return_type and self.where_block == other.where_block


__all__ = ["FunctionPrototypeAst"]
