from src.SemanticAnalysis.ASTs.LiteralAst import LiteralAst
from src.SemanticAnalysis.ASTs.IdentifierAst import IdentifierAst
from src.SemanticAnalysis.ASTs.IfExpressionAst import IfExpressionAst
from src.SemanticAnalysis.ASTs.InnerScopeAst import InnerScopeAst
from src.SemanticAnalysis.ASTs.LambdaPrototypeAst import LambdaPrototypeAst
from src.SemanticAnalysis.ASTs.ObjectInitializerAst import ObjectInitializerAst
from src.SemanticAnalysis.ASTs.ParenthesizedExpressionAst import ParenthesizedExpressionAst
from src.SemanticAnalysis.ASTs.TokenAst import TokenAst
from src.SemanticAnalysis.ASTs.TypeAst import TypeAst
from src.SemanticAnalysis.ASTs.WhileExpressionAst import WhileExpressionAst
from src.SemanticAnalysis.ASTs.WithExpressionAst import WithExpressionAst
from src.SemanticAnalysis.ASTs.YieldExpressionAst import YieldExpressionAst

type PrimaryExpressionAst = (
        LiteralAst | IdentifierAst | ParenthesizedExpressionAst | ObjectInitializerAst | LambdaPrototypeAst |
        IfExpressionAst | WhileExpressionAst | WithExpressionAst | YieldExpressionAst | InnerScopeAst | TypeAst |
        TokenAst)

__all__ = ["PrimaryExpressionAst"]
