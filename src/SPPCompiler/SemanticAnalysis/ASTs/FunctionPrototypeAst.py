import copy
from dataclasses import dataclass, field
from typing import List, Optional

from SPPCompiler.LexicalAnalysis.Tokens import TokenType
from SPPCompiler.SemanticAnalysis.ASTs.Meta.Ast import Ast
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstMixins import PreProcessor, SymbolGenerator, SupScopeLoader, SemanticAnalyser
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstPrinter import *
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstUtils import get_all_function_scopes
from SPPCompiler.SemanticAnalysis.Utils.CommonTypes import CommonTypes
from SPPCompiler.SemanticAnalysis.Utils.Scopes import ScopeHandler
from SPPCompiler.SemanticAnalysis.Utils.SemanticError import SemanticErrors
from SPPCompiler.SemanticAnalysis.Utils.Symbols import TypeSymbol
from SPPCompiler.Utils.Sequence import Seq


@dataclass
class FunctionPrototypeAst(Ast, PreProcessor, SymbolGenerator, SemanticAnalyser, SupScopeLoader):
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

    def __post_init__(self):
        from SPPCompiler.SemanticAnalysis.ASTs import GenericParameterGroupAst, WhereBlockAst
        self.generic_parameters = self.generic_parameters or GenericParameterGroupAst.default()
        self.where_block = self.where_block or WhereBlockAst.default()

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        # Print the FunctionPrototypeAst.
        s = ""
        s += f"{Seq(self.annotations).print(printer, "\n")}{self.function_token.print(printer)}{self.identifier.print(printer)}"
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
        from SPPCompiler.LexicalAnalysis.Lexer import Lexer
        from SPPCompiler.SemanticAnalysis.ASTs import (
            ModulePrototypeAst, ClassPrototypeAst, SupPrototypeInheritanceAst, TypeAst, GenericIdentifierAst,
            IdentifierAst, InnerScopeAst, TokenAst)
        from SPPCompiler.SyntacticAnalysis.Parser import Parser

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
        mock_class_name = TypeAst(self.pos, [], [GenericIdentifierAst(self.pos, mock_class_name.value, None)])

        # Determine the class type and call name. This will be "FunRef/call_ref", "FunMut/call_mut" or
        # "FunMov/call_mov".
        function_class_type = self._deduce_function_class_type(context)
        function_call_name  = self._deduce_function_call_name(function_class_type)

        # If the mock class name ("MOCK_function") doesn't exist as a class, then this if the first instance of a
        # function with this name seen. Therefore, the class needs to be added into the module prototype.
        if Seq(context.body.members).filter(lambda m: isinstance(m, ClassPrototypeAst) and m.identifier == mock_class_name).empty():

            # Create the mock class prototype and the let statement to instantiate the mock class. This creates the
            # function symbol.
            mock_cls = f"cls MOCK_{self.identifier.value} {{}}"
            mock_let = f"let {self.identifier.value} = MOCK_{self.identifier.value}()"

            # Todo:
            #  - Consider parsing as a global constant?
            #  - Could have issues with pinning though, unless the Copy type is superimposed too.

            # Parse the mock class and let statement code to generate the respective ASTs.
            mock_cls_ast = Parser(Lexer(mock_cls).lex(), "").parse_class_prototype().parse_once()
            mock_let_ast = Parser(Lexer(mock_let).lex(), "").parse_let_statement_initialized().parse_once()
            mock_let_ast._sup_let_type = function_class_type

            # Append both of these ASTs to the module or sup prototype ("context" will be either one).
            context.body.members.append(mock_cls_ast)
            context.body.members.append(mock_let_ast)

        # At this point, either the class existed, or it exists now, so superimpose the "Fun___" type onto it. Create
        # the call function, like "call_ref", and carry through the generic parameters, function parameters,
        # return type, etc.
        # Todo: don't deepcopy body and just link it?
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
            generic_parameters=self.generic_parameters,
            super_class=copy.deepcopy(function_class_type),
            on_keyword=TokenAst.dummy(TokenType.KwOn),
            identifier=mock_class_name,
            where_block=self.where_block,
            body=InnerScopeAst(
                pos=self.pos,
                brace_l_token=TokenAst.dummy(TokenType.TkBraceL),
                members=[fun_ast],
                brace_r_token=TokenAst.dummy(TokenType.TkBraceR)))

        # Append the "sup" block to the module or sup prototype ("context" will be either one).
        context.body.members.append(sup_block_ast)

    def load_sup_scopes(self, scope_handler: ScopeHandler) -> None:
        scope_handler.move_to_next_scope()
        # Load generic-version of a type in for comparisons.
        self.return_type.do_semantic_analysis(scope_handler)
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
        Seq(self.generic_parameters.parameters).for_each(lambda p: scope_handler.current_scope.add_symbol(TypeSymbol(name=p.identifier, type=None, is_generic=True)))

        # Convert non-single parameters into single parameters, and inject the destructure operation into the body.
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
        from SPPCompiler.SemanticAnalysis.ASTs import ModulePrototypeAst

        # Move into the function scope.
        scope_handler.move_to_next_scope()

        # Analyse the generic type parameters, the function parameters and the return type, in this order. This allows
        # the function parameter types and return type to use the generic type parameters.
        self.generic_parameters.do_semantic_analysis(scope_handler, **kwargs)
        self.parameters.do_semantic_analysis(scope_handler, **kwargs)
        self.return_type.do_semantic_analysis(scope_handler, **kwargs)

        # Check there are no conflicting function overloads. First, get all the overload function scopes.
        # Todo: this inadvertently bans overriding a super-class function implementation
        match self._ctx:
            case ModulePrototypeAst(): owner_type = scope_handler.current_scope.parent_module.name
            case _: owner_type = self._ctx.identifier.without_generics()

        type_scope = scope_handler.current_scope.get_symbol(owner_type).associated_scope
        func_scopes = get_all_function_scopes(type_scope, self._orig)

        # Iterate through each function scope, skipping the current scope (don't compare self against self).
        for func_scope in func_scopes:
            overload_definition = func_scope[1].body.members[0]
            overload_scope = func_scope[0].children[0]
            if overload_definition == self: continue
            that_parameters = overload_definition.parameters

            # Check if a "self" parameter exists on nether or both
            this_self_parameter = self.parameters.get_self()
            that_self_parameter = that_parameters.get_self()
            if (this_self_parameter is not None) ^ (that_self_parameter is not None): continue

            # Check if "self" is the same (allows override detection)
            if this_self_parameter is not None:
                this_self_type = scope_handler.current_scope.parent.name
                that_self_type = overload_scope.parent.name
                if this_self_type != that_self_type: continue

            # Compare the required parameter types and conventions of the function prototypes.
            this_required_parameter_types = Seq(self.parameters.get_req()).map(lambda p: p.type_declaration)
            that_required_parameter_types = Seq(that_parameters.get_req()).map(lambda p: p.type_declaration)
            if this_required_parameter_types.length != that_required_parameter_types.length: continue

            # Compare the required parameter conventions of the function prototypes.
            this_required_conventions = Seq(self.parameters.get_req()).map(lambda p: p.convention)
            that_required_conventions = Seq(that_parameters.get_req()).map(lambda p: p.convention)

            # Check if the required parameter types and conventions are the same.
            check_1 = all(this_required_parameter_types.zip(that_required_parameter_types).map(lambda p: p[0] == p[1]).value)
            check_2 = all(this_required_conventions.zip(that_required_conventions).map(lambda p: p[0] == p[1]).value)

            # If the required parameter types and conventions are the same, the function prototypes are conflicting.
            if check_1 and check_2:
                raise SemanticErrors.CONFLICTING_FUNCTION_OVERLOADS(self._orig, self, overload_definition)

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
