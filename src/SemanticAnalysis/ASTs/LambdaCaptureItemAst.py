from src.SemanticAnalysis.ASTs.LambdaCaptureItemNamedAst import LambdaCaptureItemNamedAst
from src.SemanticAnalysis.ASTs.LambdaCaptureItemNormalAst import LambdaCaptureItemNormalAst

type LambdaCaptureItemAst = LambdaCaptureItemNormalAst | LambdaCaptureItemNamedAst

__all__ = ["LambdaCaptureItemAst"]
