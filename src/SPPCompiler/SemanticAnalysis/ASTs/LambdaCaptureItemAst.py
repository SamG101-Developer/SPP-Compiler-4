from SPPCompiler.SemanticAnalysis.ASTs.LambdaCaptureItemNamedAst import LambdaCaptureItemNamedAst
from SPPCompiler.SemanticAnalysis.ASTs.LambdaCaptureItemNormalAst import LambdaCaptureItemNormalAst

type LambdaCaptureItemAst = LambdaCaptureItemNormalAst | LambdaCaptureItemNamedAst

__all__ = ["LambdaCaptureItemAst"]
