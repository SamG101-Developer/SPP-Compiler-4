from SPPCompiler.SemanticAnalysis.ASTs.IdentifierAst import IdentifierAst
from SPPCompiler.SemanticAnalysis.ASTs.CaseExpressionAst import CaseExpressionAst
from SPPCompiler.SemanticAnalysis.ASTs.InnerScopeAst import InnerScopeAst
from SPPCompiler.SemanticAnalysis.ASTs.LambdaPrototypeAst import LambdaPrototypeAst
from SPPCompiler.SemanticAnalysis.ASTs.LiteralAst import LiteralAst
from SPPCompiler.SemanticAnalysis.ASTs.ObjectInitializerAst import ObjectInitializerAst
from SPPCompiler.SemanticAnalysis.ASTs.ParenthesizedExpressionAst import ParenthesizedExpressionAst
from SPPCompiler.SemanticAnalysis.ASTs.TokenAst import TokenAst
from SPPCompiler.SemanticAnalysis.ASTs.TypeAst import TypeAst
from SPPCompiler.SemanticAnalysis.ASTs.LoopExpressionAst import LoopExpressionAst
from SPPCompiler.SemanticAnalysis.ASTs.WithExpressionAst import WithExpressionAst
from SPPCompiler.SemanticAnalysis.ASTs.YieldExpressionAst import YieldExpressionAst

type PrimaryExpressionAst = (
        LiteralAst | IdentifierAst | ParenthesizedExpressionAst | ObjectInitializerAst | LambdaPrototypeAst |
        CaseExpressionAst | LoopExpressionAst | WithExpressionAst | YieldExpressionAst | InnerScopeAst | TypeAst |
        TokenAst)

__all__ = ["PrimaryExpressionAst"]
