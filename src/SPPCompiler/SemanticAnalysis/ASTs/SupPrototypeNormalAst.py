from dataclasses import dataclass, field
from typing import Optional

from SPPCompiler.SemanticAnalysis.ASTs.Meta.Ast import Ast
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstMixins import PreProcessor, SymbolGenerator, SemanticAnalyser, SupScopeLoader
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstPrinter import *
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstUtils import SupNormalIdentifier, get_owner_type_of_sup_block
from SPPCompiler.SemanticAnalysis.Utils.CommonTypes import CommonTypes
from SPPCompiler.SemanticAnalysis.Utils.Scopes import ScopeHandler
from SPPCompiler.SemanticAnalysis.Utils.SemanticError import SemanticErrors
from SPPCompiler.SemanticAnalysis.Utils.Symbols import TypeSymbol
from SPPCompiler.Utils.Sequence import Seq


@dataclass
class SupPrototypeNormalAst(Ast, PreProcessor, SymbolGenerator, SemanticAnalyser, SupScopeLoader):
    """
    The SupPrototypeNormalAst node represents a superimposition prototype for methods and typedefs to be superimposed
    over a class.

    Attributes:
        sup_keyword: The "sup" keyword token.
        generic_parameters: The generic parameters of the superimposition.
        identifier: The identifier of the superimposition.
        where_block: The where block of the superimposition.
        body: The body of the superimposition.
    """

    sup_keyword: "TokenAst"
    generic_parameters: Optional["GenericParameterGroupAst"]
    identifier: "TypeAst"
    where_block: Optional["WhereBlockAst"]
    body: "InnerScopeAst[SupMemberAst]"

    def __post_init__(self):
        # Set the default values for the optional attributes.
        from SPPCompiler.SemanticAnalysis.ASTs import GenericParameterGroupAst, WhereBlockAst, InnerScopeAst
        self.generic_parameters = self.generic_parameters or GenericParameterGroupAst.default()
        self.where_block = self.where_block or WhereBlockAst.default()
        self.body = self.body or InnerScopeAst.default()

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        # Print the SupPrototypeNormalAst.
        s = ""
        s += f"{self.sup_keyword.print(printer)} "
        s += f"{self.generic_parameters.print(printer)} " if self.generic_parameters.parameters else ""
        s += f"{self.identifier.print(printer)} "
        s += f"{self.where_block.print(printer)} " if self.where_block.constraint_group.constraints else ""
        s += f"{self.body.print(printer)}"
        return s

    def pre_process(self, context: "ModulePrototypeAst") -> None:
        # Preprocess the members.
        from SPPCompiler.SemanticAnalysis.ASTs import SubroutinePrototypeAst, CoroutinePrototypeAst
        Seq(self.body.members).for_each(lambda m: m.pre_process(self))
        self.body.members = Seq(self.body.members).filter_not_type(SubroutinePrototypeAst, CoroutinePrototypeAst).list()

    def generate(self, scope_handler: ScopeHandler) -> None:
        # Create a new scope.
        scope_name = SupNormalIdentifier(this_class=self.identifier)
        scope_handler.into_new_scope(scope_name)

        # Generate the body members.
        Seq(self.generic_parameters.parameters).for_each(lambda p: scope_handler.current_scope.add_symbol(TypeSymbol(name=p.identifier.types[-1], type=None, is_generic=True)))
        Seq(self.body.members).for_each(lambda m: m.generate(scope_handler))

        # Exit the new scope.
        scope_handler.exit_cur_scope()

    def load_sup_scopes(self, scope_handler: ScopeHandler) -> None:
        scope_handler.move_to_next_scope()

        # Register the "Self" type and analyse the identifier.
        self_symbol = get_owner_type_of_sup_block(self.identifier, scope_handler)
        self_symbol and scope_handler.current_scope.add_symbol(TypeSymbol(
            name=CommonTypes.self().types[-1],
            type=self_symbol.type,
            associated_scope=self_symbol.associated_scope))

        # Can't superimpose over a generic type.
        cls_symbol = scope_handler.current_scope.get_symbol(self.identifier.without_generics())
        if cls_symbol.is_generic:
            raise SemanticErrors.SUPERIMPOSITION_ONTO_GENERIC(self.identifier.without_generics(), cls_symbol.name)

        # Add the superimposition scope to the class scope.
        cls_symbol.associated_scope._sup_scopes.append((scope_handler.current_scope, self))

        # Load the sup-scopes for methods defined over the "sup" block.
        Seq(self.body.members).for_each(lambda m: m.load_sup_scopes(scope_handler))
        scope_handler.exit_cur_scope()

    def load_sup_scopes_gen(self, scope_handler: ScopeHandler) -> None:
        scope_handler.move_to_next_scope()
        Seq(self.body.members).for_each(lambda m: m.load_sup_scopes_gen(scope_handler))
        scope_handler.exit_cur_scope()

    def do_semantic_analysis(self, scope_handler, **kwargs) -> None:
        scope_handler.move_to_next_scope()

        # Analyse the generic type parameters and where block. This will load the generics into the current scope, and
        # ensure all their constraints are valid.
        self.generic_parameters.do_semantic_analysis(scope_handler, **kwargs)
        self.where_block.do_semantic_analysis(scope_handler, **kwargs)

        # Make sure every generic parameter is present in the identifier; otherwise it has no way to be inferred.
        for generic_parameter in Seq(self.generic_parameters.parameters).map(lambda p: p.identifier):
            if not self.identifier.contains_generic(generic_parameter):
                raise SemanticErrors.UNCONSTRAINED_GENERIC_PARAMETER(self, generic_parameter)

        # Ensure there are no optional generic parameters.
        if self.generic_parameters.get_opt():
            raise SemanticErrors.OPTIONAL_GENERIC_PARAMETERS_IN_SUP(self, self.generic_parameters.get_opt()[0])

        # Make sure the identifier (the type being superimposed over), exists. If it does, analyse each member of the
        # body.
        self.identifier.do_semantic_analysis(scope_handler, **kwargs)
        self.body.do_semantic_analysis(scope_handler, inline=True, **kwargs)

        scope_handler.exit_cur_scope()


__all__ = ["SupPrototypeNormalAst"]
