from src.SemanticAnalysis.ASTs.SupPrototypeInheritanceAst import SupPrototypeInheritanceAst
from src.SemanticAnalysis.ASTs.SupPrototypeNormalAst import SupPrototypeNormalAst

type SupPrototypeAst = SupPrototypeNormalAst | SupPrototypeInheritanceAst

__all__ = ["SupPrototypeAst"]
