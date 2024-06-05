from SPPCompiler.SemanticAnalysis.ASTs.SupPrototypeInheritanceAst import SupPrototypeInheritanceAst
from SPPCompiler.SemanticAnalysis.ASTs.SupPrototypeNormalAst import SupPrototypeNormalAst

type SupPrototypeAst = SupPrototypeNormalAst | SupPrototypeInheritanceAst

__all__ = ["SupPrototypeAst"]
