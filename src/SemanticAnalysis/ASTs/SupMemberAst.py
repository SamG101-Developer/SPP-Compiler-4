from src.SemanticAnalysis.ASTs.ClassPrototypeAst import ClassPrototypeAst
from src.SemanticAnalysis.ASTs.LetStatementAst import LetStatementAst
from src.SemanticAnalysis.ASTs.SupMethodPrototypeAst import SupMethodPrototypeAst
from src.SemanticAnalysis.ASTs.SupPrototypeInheritanceAst import SupPrototypeInheritanceAst
from src.SemanticAnalysis.ASTs.SupTypedefAst import SupTypedefAst

type SupMemberAst = ClassPrototypeAst | LetStatementAst | SupMethodPrototypeAst | SupPrototypeInheritanceAst | SupTypedefAst

__all__ = ["SupMemberAst"]
