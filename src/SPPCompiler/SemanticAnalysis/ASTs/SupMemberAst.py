from SPPCompiler.SemanticAnalysis.ASTs import (
    ClassPrototypeAst, LetStatementAst, FunctionPrototypeAst, SupPrototypeInheritanceAst)
# from SPPCompiler.SemanticAnalysis.ASTs.SupTypedefAst import SupUseStatementAst

type SupMemberAst = ClassPrototypeAst | LetStatementAst | FunctionPrototypeAst | SupPrototypeInheritanceAst  # | SupUseStatementAst

__all__ = ["SupMemberAst"]
