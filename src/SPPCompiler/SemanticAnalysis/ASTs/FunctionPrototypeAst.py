import copy
from dataclasses import dataclass, field
from typing import List, Optional

from SPPCompiler.LexicalAnalysis.Tokens import TokenType
from SPPCompiler.SemanticAnalysis.ASTs.Meta.Ast import Ast
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstMixins import PreProcessor, SymbolGenerator, SupScopeLoader, SemanticAnalyser
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstPrinter import *
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstUtils import check_for_conflicting_methods, FunctionConflictCheckType, VisibilityEnabled
from SPPCompiler.SemanticAnalysis.Utils.CommonTypes import CommonTypes
from SPPCompiler.SemanticAnalysis.Utils.Scopes import ScopeHandler
from SPPCompiler.SemanticAnalysis.Utils.Symbols import TypeSymbol
from SPPCompiler.Utils.Sequence import Seq


@dataclass
class FunctionPrototypeAst(Ast, PreProcessor, SymbolGenerator, SemanticAnalyser, SupScopeLoader, VisibilityEnabled):
    """
    The FunctionPrototypeAst node is an AST node that represents a function prototype. This includes the function's
    annotations, identifier, generic parameters, parameters, return type, where block, and body. The function prototype
    is used to create a function in the module or sup prototype.

    Attributes:
        annotations: The annotations for the function.
        function_token: The "fun" keyword token.
        identifier: The identifier for the function.
        generic_parameters: The generic parameters for the function.
        parameters: The parameters for the function.
        arrow_token: The "->" token.
        return_type: The return type for the function.
        where_block: The where block for the function.
        body: The body of the function.

        _orig: The original identifier of the function (replaced by call_[mov|mut|ref] etc).
        _ctx: The context of the function (module or sup prototype).
    """

    annotations: List["AnnotationAst"]
    function_token: "TokenAst"
    identifier: "IdentifierAst"
    generic_parameters: Optional["GenericParameterGroupAst"]
    parameters: "FunctionParameterGroupAst"
    arrow_token: "TokenAst"
    return_type: "TypeAst"
    where_block: Optional["WhereBlockAst"]
    body: "InnerScopeAst[StatementAst]"

    _orig: "IdentifierAst" = field(default=None, kw_only=True, repr=False)
    _ctx: "ModulePrototypeAst | SupPrototypeAst" = field(default=None, kw_only=True, repr=False)
    _abstract: bool = field(default=False, kw_only=True, repr=False)
    _virtual: bool = field(default=False, kw_only=True, repr=False)

    def __post_init__(self):
        from SPPCompiler.SemanticAnalysis.ASTs import GenericParameterGroupAst, WhereBlockAst, InnerScopeAst
        self.generic_parameters = self.generic_parameters or GenericParameterGroupAst.default()
        self.where_block = self.where_block or WhereBlockAst.default()
        self.body = self.body or InnerScopeAst.default()

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        # Print the FunctionPrototypeAst.
        s = ""
        s += f"{Seq(self.annotations).print(printer, "\n")}{self.function_token.print(printer)} {self.identifier.print(printer)}"
        s += f"{self.generic_parameters.print(printer)}" if self.generic_parameters else ""
        s += f"{self.parameters.print(printer)} {self.arrow_token.print(printer)} {self.return_type.print(printer)}"
        s += f" {self.where_block.print(printer)}" if self.where_block else ""
        s += f"{self.body.print(printer)}"
        return s

    @ast_printer_method
    def print_signature(self, printer: AstPrinter) -> str:
        # Print the FunctionPrototypeAst, without the body
        s = ""
        s += f"{self.generic_parameters.print(printer)}" if self.generic_parameters else ""
        s += f"{self.parameters.print(printer)} {self.arrow_token.print(printer)} {self.return_type.print(printer)}"
        s += f" {self.where_block.print(printer)}" if self.where_block else ""
        return s

    def pre_process(self, context: "ModulePrototypeAst | SupPrototypeAst") -> None:
        """
        Preprocessing a function prototype is a complex multistep process that involves creating multiple new AST nodes.
        The general idea, is to create a new type that represents functions of that identifier, and superimpose a
        Fun[Ref|Mut|Mov] type over this mock class for each overload. Then, a "let" statement is used to create a
        function-class object of that type. For example:

        fun function(a: Str) -> Str { ## code 1 ## }
        fun function(a: U64) -> U64 { ## code 2 ## }

        becomes:

        cls MOCK_function {}
        sup FunRef[Str, (Str)] on MOCK_function {
            fun call_ref(a: Str) -> Str { ## code 1 ## }
        }
        sup FunRef[U64, (U64)] on MOCK_function {
            fun call_ref(a: U64) -> U64 { ## code 2 ## }
        }
        let function = MOCK_function()

        Args:
            context: The context of the function (module or sup prototype).

        Returns:
            None
        """

        from SPPCompiler.LexicalAnalysis.Lexer import Lexer
        from SPPCompiler.SemanticAnalysis.ASTs import ClassPrototypeAst, SupPrototypeInheritanceAst, TypeAst, IdentifierAst, InnerScopeAst, TokenAst, ModulePrototypeAst
        from SPPCompiler.SyntacticAnalysis.Parser import Parser

        # Register the context, as it is necessary in the "load_sup_scopes" stage.
        self._ctx = context

        # For methods, substitute the "Self" type on the "self" parameter with the context's identifier.
        if not isinstance(context, ModulePrototypeAst) and self.parameters.get_self():
            self.parameters.get_self().type_declaration = context.identifier

        # Convert the "fun ..." to a "Fun___" superimposition over a type representing the function class. This allows
        # for the first-class nature of functions. The mock object for "fun function" will be "MOCK_function".
        mock_class_name = IdentifierAst(self.pos, f"MOCK_{self.identifier.value}")
        mock_class_name = TypeAst(self.pos, [], [mock_class_name.to_generic_identifier()])

        # Determine the class type and call name. This will be "FunRef/call_ref", "FunMut/call_mut" or
        # "FunMov/call_mov".
        function_class_type = self._deduce_function_class_type(context)
        function_call_name  = self._deduce_function_call_name(function_class_type)

        # If the mock class name ("MOCK_function") doesn't exist as a class, then this if the first instance of a
        # function with this name seen. Therefore, the class needs to be added into the module prototype.
        if Seq(context.body.members).filter_to_type(ClassPrototypeAst).filter(lambda m: m.identifier == mock_class_name).empty():

            # Create the mock class prototype, and the let statement that instantiates the mock class.
            mock_cls = f"cls MOCK_{self.identifier.value} {{}}"
            mock_let = f"let {self.identifier.value} = MOCK_{self.identifier.value}()"

            # Parse the mock class and let statement code to generate the respective ASTs.
            mock_cls_ast = Parser(Lexer(mock_cls).lex(), "").parse_class_prototype().parse_once()
            mock_let_ast = Parser(Lexer(mock_let).lex(), "").parse_let_statement_initialized().parse_once()
            mock_let_ast._sup_let_type = mock_cls_ast.identifier

            # Append both of these ASTs to the module or sup prototype ("context" will be either one).
            context.body.members.append(mock_cls_ast)
            context.body.members.append(mock_let_ast)

        # At this point, either the class existed, or it exists now, so superimpose the "Fun___" type onto it. Create
        # the call function, like "call_ref", and carry through the generic parameters, function parameters,
        # return type, etc.
        self._ctx = None
        fun_ast = copy.deepcopy(self)
        fun_ast.identifier = function_call_name
        fun_ast._orig = self.identifier
        fun_ast._ctx = context
        self._ctx = context

        # Create the superimposition block over the class type, which includes the "call_ref" function as a member. This
        # will allow for the type to now be callable with the parameter types and return type specified.
        sup_block_ast = SupPrototypeInheritanceAst(
            pos=self.pos,
            sup_keyword=TokenAst.dummy(TokenType.KwSup),
            identifier=mock_class_name,
            generic_parameters=self.generic_parameters,
            ext_keyword=TokenAst.dummy(TokenType.KwExt),
            super_class=copy.deepcopy(function_class_type),
            where_block=self.where_block,
            body=InnerScopeAst(
                pos=self.pos,
                brace_l_token=TokenAst.dummy(TokenType.TkBraceL),
                members=[fun_ast],
                brace_r_token=TokenAst.dummy(TokenType.TkBraceR)))

        # Mark the function as abstract or virtual if the decorators are present + visibility.
        Seq(self.annotations).for_each(lambda a: a.pre_process(fun_ast))

        # Append the "sup" block to the module or sup prototype ("context" will be either one).
        context.body.members.append(sup_block_ast)

    def load_sup_scopes(self, scope_handler: ScopeHandler) -> None:
        from SPPCompiler.SemanticAnalysis.ASTs import ModulePrototypeAst
        from SPPCompiler.SemanticAnalysis.Utils.SemanticError import SemanticErrors

        scope_handler.move_to_next_scope()

        # Load generic-version of a type in for comparisons.
        self.return_type.do_semantic_analysis(scope_handler)

        # Get the owner type or namespace scope, to check for conflicting function overloads.
        match self._ctx:
            case ModulePrototypeAst(): owner_type = scope_handler.current_scope.parent_module.name
            case _: owner_type = self._ctx.identifier
        type_scope = scope_handler.current_scope.get_symbol(owner_type).associated_scope

        # Check for conflicting function overloads in the type scope.
        if conflict := check_for_conflicting_methods(type_scope, scope_handler, self, FunctionConflictCheckType.InvalidOverload):
            raise SemanticErrors.CONFLICTING_FUNCTION_OVERLOADS(self._orig, conflict, self)

        scope_handler.exit_cur_scope()

    def load_sup_scopes_gen(self, scope_handler: ScopeHandler) -> None:
        scope_handler.move_to_next_scope()
        scope_handler.exit_cur_scope()

    def _deduce_function_class_type(self, context: "ModulePrototypeAst | SupPrototypeAst") -> "TypeAst":
        from SPPCompiler.SemanticAnalysis.ASTs import (
            FunctionParameterSelfAst, ConventionRefAst, ConventionMutAst, ConventionMovAst, ModulePrototypeAst)

        # Deducing the function call type requires knowledge of the "self" parameter. If there is a "self" parameter,
        # then use the convention to determine the function class type. If there isn't, then the function is either a
        # free function (module scope), or a static class method. In either case, it will be "FunRef".
        is_method = not isinstance(context, ModulePrototypeAst)
        has_self_parameter = self.parameters.parameters and isinstance(self.parameters.parameters[0], FunctionParameterSelfAst)

        # Get the parameter types and return type, to move into the function class type being created.
        parameter_types = Seq(self.parameters.parameters).map(lambda p: p.type_declaration).list()
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

    def _deduce_function_call_name(self, function_class_type: "TypeAst") -> "IdentifierAst":
        from SPPCompiler.SemanticAnalysis.ASTs import IdentifierAst

        # Map the function class type to a function call name with a simple match-case statement.
        match function_class_type.types[-1].value:
            case "FunRef": return IdentifierAst(self.identifier.pos, "call_ref")
            case "FunMut": return IdentifierAst(self.identifier.pos, "call_mut")
            case "FunMov": return IdentifierAst(self.identifier.pos, "call_mov")
            case _: raise SystemExit(f"Unknown function class type '{function_class_type}' being deduced. Report as bug.")

    def generate(self, scope_handler: ScopeHandler) -> None:
        from SPPCompiler.SemanticAnalysis.ASTs import (
            LocalVariableSingleIdentifierAst, FunctionParameterSelfAst, LetStatementInitializedAst, TokenAst, IdentifierAst)

        # Create and move into a new scope for the function prototype's scope. Within this scope, generate type symbols
        # for each generic parameter. Exit the newly created function scope.
        scope_handler.into_new_scope(f"<function:{self._orig}>")
        Seq(self.generic_parameters.parameters).for_each(lambda p: scope_handler.current_scope.add_symbol(TypeSymbol(name=p.identifier.types[-1], type=None, is_generic=True)))

        # Convert non-single parameters into single parameters, and inject the destructure operation into the body. This
        # doesn't affect the signature.
        for i, parameter in Seq(self.parameters.parameters).filter_not_type(FunctionParameterSelfAst).enumerate():
            if not isinstance(parameter.variable, LocalVariableSingleIdentifierAst):
                destructure_to = parameter.variable

                # Create the mock variable and replace the parameter with it.
                mock_single_variable = LocalVariableSingleIdentifierAst(
                    pos=parameter.pos,
                    is_mutable=None,
                    identifier=IdentifierAst(parameter.pos, "_param_" + str(i)))
                self.parameters.parameters[i].variable = mock_single_variable

                # Move the destructure into the function body.
                destructure_operation = LetStatementInitializedAst(
                    pos=parameter.pos,
                    let_keyword=TokenAst.dummy(TokenType.KwLet),
                    assign_to=destructure_to,
                    assign_token=TokenAst.dummy(TokenType.TkAssign),
                    value=mock_single_variable.identifier)
                self.body.members.insert(0, destructure_operation)

        scope_handler.exit_cur_scope()

    def do_semantic_analysis(self, scope_handler, **kwargs) -> None:

        # Move into the function scope.
        scope_handler.move_to_next_scope()

        # Analyse the generic type parameters, the function parameters and the return type, in this order. This allows
        # the function parameter types and return type to use the generic type parameters.
        self.generic_parameters.do_semantic_analysis(scope_handler, **kwargs)
        self.parameters.do_semantic_analysis(scope_handler, **kwargs)
        self.return_type.do_semantic_analysis(scope_handler, **kwargs)

        # The rest of the prototype (the body) is handled in the SubroutinePrototypeAst and CoroutinePrototypeAst nodes,
        # as they have different analysis requirements. This includes exiting the scope.

    def __eq__(self, other):
        # Check both ASTs are the same type and have the same generic parameters, parameters, return type, and where
        # block.

        return all([
            self.identifier == other.identifier,
            self.generic_parameters == other.generic_parameters,
            self.parameters == other.parameters,
            self.return_type == other.return_type,
            self.where_block == other.where_block])


__all__ = ["FunctionPrototypeAst"]
