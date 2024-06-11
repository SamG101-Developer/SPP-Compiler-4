from dataclasses import dataclass

from SPPCompiler.SemanticAnalysis.Utils.CommonTypes import CommonTypes
from SPPCompiler.SemanticAnalysis.Utils.SemanticError import SemanticErrors
from SPPCompiler.SemanticAnalysis.ASTs import FunctionPrototypeAst


@dataclass
class SubroutinePrototypeAst(FunctionPrototypeAst):
    def do_semantic_analysis(self, scope_handler, **kwargs) -> None:
        super().do_semantic_analysis(scope_handler, **kwargs)
        from SPPCompiler.SemanticAnalysis.ASTs import ReturnStatementAst

        # Add the "target-return-type" to the kwargs, so other ASTs can use the function's return type is required.
        # Analyse the body of the function, and then pop the return type from the kwargs.
        kwargs |= {"target-return-type": self.return_type}
        kwargs |= {"is-subroutine": self.function_token}
        self.body.do_semantic_analysis(scope_handler, inline=True, **kwargs)

        # Check a "ret" statement exists at the end of the function, as long as it is a subroutine with a non-Void
        # return type.
        if (not self.return_type.symbolic_eq(CommonTypes.void(), scope_handler.current_scope)
                and self.body.members
                and not isinstance(self.body.members[-1], ReturnStatementAst)):
            raise SemanticErrors.MISSING_RETURN_STATEMENT(self.return_type, self.body.brace_r_token)

        scope_handler.exit_cur_scope()
