from abc import ABC

from SPPCompiler.SemanticAnalysis.Utils.Scopes import ScopeHandler


class SupScopeLoader(ABC):
    def load_sup_scopes(self, scope_handler: ScopeHandler) -> None:
        scope_handler.move_to_next_scope()
        scope_handler.exit_cur_scope()


__all__ = ["SupScopeLoader"]
