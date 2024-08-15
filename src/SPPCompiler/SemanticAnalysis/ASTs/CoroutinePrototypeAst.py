from __future__ import annotations

import copy
from dataclasses import dataclass

from SPPCompiler.SemanticAnalysis.ASTs import FunctionPrototypeAst
from SPPCompiler.SemanticAnalysis.Utils.CommonTypes import CommonTypes
from SPPCompiler.SemanticAnalysis.Utils.SemanticError import SemanticErrors


@dataclass
class CoroutinePrototypeAst(FunctionPrototypeAst):
    def do_semantic_analysis(self, scope_handler, **kwargs) -> None:
        super().do_semantic_analysis(scope_handler, **kwargs)

        # Ensure the return type is one of the 3 generator return types.
        # Todo: switch to symbolic equality?
        if self.return_type.without_generics() not in [
                CommonTypes.gen_mov().without_generics(),
                CommonTypes.gen_mut().without_generics(),
                CommonTypes.gen_ref().without_generics()]:
            raise SemanticErrors.INVALID_COROUTINE_RETURN_TYPE(self, self.return_type)

        # Get the coroutine return type (it is the generic argument in the "Return" type).
        coroutine_return_type = CommonTypes.opt(self.return_type.parts[-1].generic_arguments["Yield"].type)
        coroutine_return_type.do_semantic_analysis(scope_handler, **kwargs)

        # Add the "target-return-type" to the kwargs, so other ASTs can use the function's return type is required.
        # Analyse the body of the function, and then pop the return type from the kwargs.
        kwargs |= {"target-return-type": self.return_type}
        kwargs |= {"is-coroutine": self.function_token}
        self.body.do_semantic_analysis(scope_handler, inline=True, **kwargs)

        scope_handler.exit_cur_scope()

    def __deepcopy__(self, memodict) -> CoroutinePrototypeAst:
        return CoroutinePrototypeAst(
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
