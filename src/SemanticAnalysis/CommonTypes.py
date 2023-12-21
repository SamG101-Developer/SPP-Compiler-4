class CommonTypes:
    @staticmethod
    def self():
        from src.SemanticAnalysis.ASTs.Ast import TypeSingleAst, GenericIdentifierAst
        return TypeSingleAst(-1, [GenericIdentifierAst(-1, "Self", None)])

    @staticmethod
    def void():
        from src.SemanticAnalysis.ASTs.Ast import TypeSingleAst, GenericIdentifierAst
        return TypeSingleAst(-1, [GenericIdentifierAst(-1, "Void", None)])

    @staticmethod
    def bool():
        from src.SemanticAnalysis.ASTs.Ast import TypeSingleAst, GenericIdentifierAst
        return TypeSingleAst(-1, [GenericIdentifierAst(-1, "Bool", None)])

    @staticmethod
    def big_num():
        from src.SemanticAnalysis.ASTs.Ast import TypeSingleAst, GenericIdentifierAst
        return TypeSingleAst(-1, [GenericIdentifierAst(-1, "BigNum", None)])

    @staticmethod
    def big_dec():
        from src.SemanticAnalysis.ASTs.Ast import TypeSingleAst, GenericIdentifierAst
        return TypeSingleAst(-1, [GenericIdentifierAst(-1, "BigDec", None)])

    @staticmethod
    def str():
        from src.SemanticAnalysis.ASTs.Ast import TypeSingleAst, GenericIdentifierAst
        return TypeSingleAst(-1, [GenericIdentifierAst(-1, "Str", None)])

    @staticmethod
    def arr(elem_type):
        from src.SemanticAnalysis.ASTs.Ast import TypeSingleAst, GenericIdentifierAst, GenericArgumentGroupAst, GenericArgumentNormalAst
        elem_type = GenericArgumentGroupAst(-1, None, [GenericArgumentNormalAst(-1, elem_type)], None)
        return TypeSingleAst(-1, [GenericIdentifierAst(-1, "Arr", elem_type)])

    @staticmethod
    def tuple(types):
        from src.SemanticAnalysis.ASTs.Ast import TypeSingleAst, GenericIdentifierAst, GenericArgumentGroupAst, GenericArgumentNormalAst
        types = GenericArgumentGroupAst(-1, None, [GenericArgumentNormalAst(-1, x) for x in types], None)
        return TypeSingleAst(-1, [GenericIdentifierAst(-1, "Tuple", types)])
