from SPPCompiler.SemanticAnalysis.ASTs.ClassPrototypeAst import ClassPrototypeAst
from SPPCompiler.SemanticAnalysis.ASTs.LetStatementAst import LetStatementAst
from SPPCompiler.SemanticAnalysis.ASTs.SupMethodPrototypeAst import SupMethodPrototypeAst
from SPPCompiler.SemanticAnalysis.ASTs.SupPrototypeInheritanceAst import SupPrototypeInheritanceAst
from SPPCompiler.SemanticAnalysis.ASTs.SupTypedefAst import SupTypedefAst

type SupMemberAst = ClassPrototypeAst | LetStatementAst | SupMethodPrototypeAst | SupPrototypeInheritanceAst | SupTypedefAst

__all__ = ["SupMemberAst"]
