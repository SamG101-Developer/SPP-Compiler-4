from SPPCompiler.SemanticAnalysis.ASTs.ClassPrototypeAst import ClassPrototypeAst
from SPPCompiler.SemanticAnalysis.ASTs.FunctionPrototypeAst import FunctionPrototypeAst
from SPPCompiler.SemanticAnalysis.ASTs.LetStatementAst import LetStatementAst
from SPPCompiler.SemanticAnalysis.ASTs.SupPrototypeNormalAst import SupPrototypeNormalAst
from SPPCompiler.SemanticAnalysis.ASTs.SupPrototypeInheritanceAst import SupPrototypeInheritanceAst
# from SPPCompiler.SemanticAnalysis.ASTs.TypedefStatementAst import TypedefStatementAst

type ModuleMemberAst = (
        ClassPrototypeAst | FunctionPrototypeAst | LetStatementAst |
        SupPrototypeNormalAst | SupPrototypeInheritanceAst)  # | TypedefStatementAst)

__all__ = ["ModuleMemberAst"]
