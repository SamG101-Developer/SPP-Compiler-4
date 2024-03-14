from src.SemanticAnalysis.ASTs.ClassPrototypeAst import ClassPrototypeAst
from src.SemanticAnalysis.ASTs.FunctionPrototypeAst import FunctionPrototypeAst
from src.SemanticAnalysis.ASTs.LetStatementAst import LetStatementAst
from src.SemanticAnalysis.ASTs.SupPrototypeNormalAst import SupPrototypeNormalAst
from src.SemanticAnalysis.ASTs.SupPrototypeInheritanceAst import SupPrototypeInheritanceAst
from src.SemanticAnalysis.ASTs.TypedefStatementAst import TypedefStatementAst

type ModuleMemberAst = (
        ClassPrototypeAst | FunctionPrototypeAst | LetStatementAst |
        SupPrototypeNormalAst | SupPrototypeInheritanceAst | TypedefStatementAst)

__all__ = ["ModuleMemberAst"]
