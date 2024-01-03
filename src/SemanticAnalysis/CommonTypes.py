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
    def rgx():
        from src.SemanticAnalysis.ASTs.Ast import TypeSingleAst, GenericIdentifierAst
        return TypeSingleAst(-1, [GenericIdentifierAst(-1, "Rgx", None)])

    @staticmethod
    def ctx():
        from src.SemanticAnalysis.ASTs.Ast import TypeSingleAst, GenericIdentifierAst
        return TypeSingleAst(-1, [GenericIdentifierAst(-1, "Ctx", None)])

    @staticmethod
    def arr(elem_type):
        from src.SemanticAnalysis.ASTs.Ast import TypeSingleAst, GenericIdentifierAst, GenericArgumentGroupAst, GenericArgumentNormalAst, TokenAst
        from src.LexicalAnalysis.Tokens import TokenType
        elem_type = GenericArgumentGroupAst(-1, TokenAst.dummy(TokenType.TkBrackL), [GenericArgumentNormalAst(-1, elem_type)], TokenAst.dummy(TokenType.TkBrackR))
        return TypeSingleAst(-1, [GenericIdentifierAst(-1, "Arr", elem_type)])

    @staticmethod
    def tuple(types):
        from src.SemanticAnalysis.ASTs.Ast import TypeSingleAst, GenericIdentifierAst, GenericArgumentGroupAst, GenericArgumentNormalAst, TokenAst
        from src.LexicalAnalysis.Tokens import TokenType
        types = GenericArgumentGroupAst(-1, TokenAst.dummy(TokenType.TkBrackL), [GenericArgumentNormalAst(-1, x) for x in types], TokenAst.dummy(TokenType.TkBrackR))
        return TypeSingleAst(-1, [GenericIdentifierAst(-1, "Tuple", types)])

    @staticmethod
    def fun_ref(return_type, param_types, pos: int = -1):
        from src.SemanticAnalysis.ASTs.Ast import TypeTupleAst, TypeSingleAst, GenericIdentifierAst, GenericArgumentGroupAst, GenericArgumentNormalAst, TokenAst
        from src.LexicalAnalysis.Tokens import TokenType
        return_type_generic = GenericArgumentNormalAst(-1, return_type)
        param_types_generic = GenericArgumentNormalAst(-1, TypeTupleAst(-1, TokenAst.dummy(TokenType.TkParenL), param_types, TokenAst.dummy(TokenType.TkParenR)))
        return TypeSingleAst(pos, [GenericIdentifierAst(-1, "FunRef", GenericArgumentGroupAst(-1, TokenAst.dummy(TokenType.TkBrackL), [return_type_generic, param_types_generic], TokenAst.dummy(TokenType.TkBrackR)))])

    @staticmethod
    def fun_mut(return_type, param_types, pos: int = -1):
        from src.SemanticAnalysis.ASTs.Ast import TypeTupleAst, TypeSingleAst, GenericIdentifierAst, GenericArgumentGroupAst, GenericArgumentNormalAst, TokenAst
        from src.LexicalAnalysis.Tokens import TokenType
        return_type_generic = GenericArgumentNormalAst(-1, return_type)
        param_types_generic = GenericArgumentNormalAst(-1, TypeTupleAst(-1, TokenAst.dummy(TokenType.TkParenL), param_types, TokenAst.dummy(TokenType.TkParenR)))
        return TypeSingleAst(pos, [GenericIdentifierAst(-1, "FunMut", GenericArgumentGroupAst(-1, TokenAst.dummy(TokenType.TkBrackL), [return_type_generic, param_types_generic], TokenAst.dummy(TokenType.TkBrackR)))])

    @staticmethod
    def fun_one(return_type, param_types, pos: int = -1):
        from src.SemanticAnalysis.ASTs.Ast import TypeTupleAst, TypeSingleAst, GenericIdentifierAst, GenericArgumentGroupAst, GenericArgumentNormalAst, TokenAst
        from src.LexicalAnalysis.Tokens import TokenType
        return_type_generic = GenericArgumentNormalAst(-1, return_type)
        param_types_generic = GenericArgumentNormalAst(-1, TypeTupleAst(-1, TokenAst.dummy(TokenType.TkParenL), param_types, TokenAst.dummy(TokenType.TkParenR)))
        return TypeSingleAst(pos, [GenericIdentifierAst(-1, "FunOne", GenericArgumentGroupAst(-1, TokenAst.dummy(TokenType.TkBrackL), [return_type_generic, param_types_generic], TokenAst.dummy(TokenType.TkBrackR)))])
