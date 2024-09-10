from __future__ import annotations

import copy
from dataclasses import dataclass

from SPPCompiler.SemanticAnalysis.ASTs import FunctionPrototypeAst
from SPPCompiler.SemanticAnalysis.Utils.CommonTypes import CommonTypes
from SPPCompiler.SemanticAnalysis.Utils.SemanticError import SemanticErrors


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
        # return type, and contains statements.
        # Todo: Once STL is implemented, enable "c2".
        c1 = self.body.members and not isinstance(self.body.members[-1], ReturnStatementAst)
        c2 = False  # not self.body.members

        if not self.return_type.symbolic_eq(CommonTypes.void(), scope_handler.current_scope) and (c1 or c2):
            raise SemanticErrors.MISSING_RETURN_STATEMENT(self.return_type, self.body.brace_r_token)

        scope_handler.exit_cur_scope()

    def __deepcopy__(self, memodict) -> SubroutinePrototypeAst:
        return SubroutinePrototypeAst(
            pos=self.pos,
            annotations=self.annotations,
            function_token=copy.deepcopy(self.function_token),
            identifier=copy.deepcopy(self.identifier),
            generic_parameters=copy.deepcopy(self.generic_parameters),
            parameters=copy.deepcopy(self.parameters),
            arrow_token=copy.deepcopy(self.arrow_token),
            return_type=copy.deepcopy(self.return_type),
            where_block=copy.deepcopy(self.where_block),
            body=self.body)


__all__ = ["SubroutinePrototypeAst"]
