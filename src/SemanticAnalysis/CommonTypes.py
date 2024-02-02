class CommonTypes:
    @staticmethod
    def self(pos: int = -1):
        from src.SemanticAnalysis.ASTs.Ast import TypeSingleAst, GenericIdentifierAst
        return TypeSingleAst(pos, [GenericIdentifierAst(pos, "Self", None)])

    @staticmethod
    def void(pos: int = -1):
        from src.SemanticAnalysis.ASTs.Ast import TypeSingleAst, GenericIdentifierAst
        return TypeSingleAst(pos, [GenericIdentifierAst(pos, "Void", None)])

    @staticmethod
    def bool(pos: int = -1):
        from src.SemanticAnalysis.ASTs.Ast import TypeSingleAst, GenericIdentifierAst
        return TypeSingleAst(pos, [GenericIdentifierAst(pos, "Bool", None)])

    @staticmethod
    def big_num(pos: int = -1):
        from src.SemanticAnalysis.ASTs.Ast import TypeSingleAst, GenericIdentifierAst
        return TypeSingleAst(pos, [GenericIdentifierAst(pos, "BigNum", None)])

    @staticmethod
    def big_dec(pos: int = -1):
        from src.SemanticAnalysis.ASTs.Ast import TypeSingleAst, GenericIdentifierAst
        return TypeSingleAst(pos, [GenericIdentifierAst(pos, "BigDec", None)])

    @staticmethod
    def str(pos: int = -1):
        from src.SemanticAnalysis.ASTs.Ast import TypeSingleAst, GenericIdentifierAst
        return TypeSingleAst(pos, [GenericIdentifierAst(pos, "Str", None)])

    @staticmethod
    def rgx(pos: int = -1):
        from src.SemanticAnalysis.ASTs.Ast import TypeSingleAst, GenericIdentifierAst
        return TypeSingleAst(pos, [GenericIdentifierAst(pos, "Rgx", None)])

    @staticmethod
    def ctx(pos: int = -1):
        from src.SemanticAnalysis.ASTs.Ast import TypeSingleAst, GenericIdentifierAst
        return TypeSingleAst(pos, [GenericIdentifierAst(pos, "Ctx", None)])

    @staticmethod
    def fut(inner_type, pos: int = -1):
        from src.SemanticAnalysis.ASTs.Ast import TypeSingleAst, GenericIdentifierAst, GenericArgumentGroupAst, GenericArgumentNormalAst, TokenAst
        from src.LexicalAnalysis.Tokens import TokenType
        inner_type = GenericArgumentNormalAst(-1, inner_type)
        return TypeSingleAst(pos, [GenericIdentifierAst(pos, "Fut", GenericArgumentGroupAst(-1, TokenAst.dummy(TokenType.TkBrackL), [inner_type], TokenAst.dummy(TokenType.TkBrackR)))])

    @staticmethod
    def arr(elem_type, pos: int = -1):
        from src.SemanticAnalysis.ASTs.Ast import TypeSingleAst, GenericIdentifierAst, GenericArgumentGroupAst, GenericArgumentNormalAst, TokenAst
        from src.LexicalAnalysis.Tokens import TokenType
        elem_type = GenericArgumentGroupAst(-1, TokenAst.dummy(TokenType.TkBrackL), [GenericArgumentNormalAst(-1, elem_type)], TokenAst.dummy(TokenType.TkBrackR))
        return TypeSingleAst(pos, [GenericIdentifierAst(pos, "Arr", elem_type)])

    @staticmethod
    def tuple(types, pos: int = -1):
        from src.SemanticAnalysis.ASTs.Ast import TypeSingleAst, GenericIdentifierAst, GenericArgumentGroupAst, GenericArgumentNormalAst, TokenAst
        from src.LexicalAnalysis.Tokens import TokenType
        types = GenericArgumentGroupAst(-1, TokenAst.dummy(TokenType.TkBrackL), [GenericArgumentNormalAst(-1, x) for x in types], TokenAst.dummy(TokenType.TkBrackR))
        return TypeSingleAst(pos, [GenericIdentifierAst(pos, "Tup", types)])

    @staticmethod
    def union(types, pos: int = -1):
        from src.SemanticAnalysis.ASTs.Ast import TypeSingleAst, GenericIdentifierAst, GenericArgumentGroupAst, GenericArgumentNormalAst, TokenAst
        from src.LexicalAnalysis.Tokens import TokenType
        types = GenericArgumentGroupAst(-1, TokenAst.dummy(TokenType.TkBrackL), [GenericArgumentNormalAst(-1, x) for x in types], TokenAst.dummy(TokenType.TkBrackR))
        return TypeSingleAst(pos, [GenericIdentifierAst(pos, "Var", types)])

    @staticmethod
    def fun_ref(return_type, param_types, pos: int = -1):
        from src.SemanticAnalysis.ASTs.Ast import TypeSingleAst, GenericIdentifierAst, GenericArgumentGroupAst, GenericArgumentNormalAst, TokenAst
        from src.LexicalAnalysis.Tokens import TokenType
        return_type_generic = GenericArgumentNormalAst(-1, return_type)
        param_types_generic = GenericArgumentNormalAst(-1, CommonTypes.tuple(param_types))
        return TypeSingleAst(pos, [GenericIdentifierAst(pos, "FunRef", GenericArgumentGroupAst(-1, TokenAst.dummy(TokenType.TkBrackL), [return_type_generic, param_types_generic], TokenAst.dummy(TokenType.TkBrackR)))])

    @staticmethod
    def fun_mut(return_type, param_types, pos: int = -1):
        from src.SemanticAnalysis.ASTs.Ast import TypeSingleAst, GenericIdentifierAst, GenericArgumentGroupAst, GenericArgumentNormalAst, TokenAst
        from src.LexicalAnalysis.Tokens import TokenType
        return_type_generic = GenericArgumentNormalAst(-1, return_type)
        param_types_generic = GenericArgumentNormalAst(-1, CommonTypes.tuple(param_types))
        return TypeSingleAst(pos, [GenericIdentifierAst(pos, "FunMut", GenericArgumentGroupAst(-1, TokenAst.dummy(TokenType.TkBrackL), [return_type_generic, param_types_generic], TokenAst.dummy(TokenType.TkBrackR)))])

    @staticmethod
    def fun_mov(return_type, param_types, pos: int = -1):
        from src.SemanticAnalysis.ASTs.Ast import TypeSingleAst, GenericIdentifierAst, GenericArgumentGroupAst, GenericArgumentNormalAst, TokenAst
        from src.LexicalAnalysis.Tokens import TokenType
        return_type_generic = GenericArgumentNormalAst(-1, return_type)
        param_types_generic = GenericArgumentNormalAst(-1, CommonTypes.tuple(param_types))
        return TypeSingleAst(pos, [GenericIdentifierAst(pos, "FunMov", GenericArgumentGroupAst(-1, TokenAst.dummy(TokenType.TkBrackL), [return_type_generic, param_types_generic], TokenAst.dummy(TokenType.TkBrackR)))])
