from SPPCompiler.SemanticAnalysis.ASTs.ClassPrototypeAst import ClassPrototypeAst
from SPPCompiler.SemanticAnalysis.ASTs.LetStatementAst import LetStatementAst
from SPPCompiler.SemanticAnalysis.ASTs.FunctionPrototypeAst import FunctionPrototypeAst
from SPPCompiler.SemanticAnalysis.ASTs.SupPrototypeInheritanceAst import SupPrototypeInheritanceAst
# from SPPCompiler.SemanticAnalysis.ASTs.SupTypedefAst import SupTypedefAst

type SupMemberAst = ClassPrototypeAst | LetStatementAst | FunctionPrototypeAst | SupPrototypeInheritanceAst  # | SupTypedefAst

__all__ = ["SupMemberAst"]
