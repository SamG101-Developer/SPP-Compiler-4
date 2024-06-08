class CommonTypes:
    @staticmethod
    def self(pos: int = -1):
        from SPPCompiler.SemanticAnalysis.ASTs import TypeAst, GenericIdentifierAst
        return TypeAst(pos, [GenericIdentifierAst(pos, "Self", None)])

    @staticmethod
    def void(pos: int = -1):
        from SPPCompiler.SemanticAnalysis.ASTs import IdentifierAst, TypeAst, GenericIdentifierAst
        return TypeAst(pos, [IdentifierAst(pos, "std"), GenericIdentifierAst(pos, "Void", None)])

    @staticmethod
    def bool(pos: int = -1):
        from SPPCompiler.SemanticAnalysis.ASTs import IdentifierAst, TypeAst, GenericIdentifierAst
        return TypeAst(pos, [IdentifierAst(pos, "std"), GenericIdentifierAst(pos, "Bool", None)])

    @staticmethod
    def big_num(pos: int = -1):
        from SPPCompiler.SemanticAnalysis.ASTs import IdentifierAst, TypeAst, GenericIdentifierAst
        return TypeAst(pos, [IdentifierAst(pos, "std"), GenericIdentifierAst(pos, "BigNum", None)])

    @staticmethod
    def big_dec(pos: int = -1):
        from SPPCompiler.SemanticAnalysis.ASTs import IdentifierAst, TypeAst, GenericIdentifierAst
        return TypeAst(pos, [IdentifierAst(pos, "std"), GenericIdentifierAst(pos, "BigDec", None)])

    @staticmethod
    def str(pos: int = -1):
        from SPPCompiler.SemanticAnalysis.ASTs import IdentifierAst, TypeAst, GenericIdentifierAst
        return TypeAst(pos, [IdentifierAst(pos, "std"), GenericIdentifierAst(pos, "Str", None)])

    @staticmethod
    def rgx(pos: int = -1):
        from SPPCompiler.SemanticAnalysis.ASTs import IdentifierAst, TypeAst, GenericIdentifierAst
        return TypeAst(pos, [IdentifierAst(pos, "std"), GenericIdentifierAst(pos, "Rgx", None)])

    @staticmethod
    def ctx_ref(pos: int = -1):
        from SPPCompiler.SemanticAnalysis.ASTs import IdentifierAst, TypeAst, GenericIdentifierAst
        return TypeAst(pos, [IdentifierAst(pos, "std"), GenericIdentifierAst(pos, "CtxRef", None)])

    @staticmethod
    def ctx_mut(pos: int = -1):
        from SPPCompiler.SemanticAnalysis.ASTs import IdentifierAst, TypeAst, GenericIdentifierAst
        return TypeAst(pos, [IdentifierAst(pos, "std"), GenericIdentifierAst(pos, "CtxMut", None)])

    @staticmethod
    def fut(inner_type, pos: int = -1):
        from SPPCompiler.SemanticAnalysis.ASTs import IdentifierAst, TypeAst, GenericIdentifierAst, GenericArgumentGroupAst, GenericArgumentNormalAst, TokenAst
        from SPPCompiler.LexicalAnalysis.Tokens import TokenType
        inner_type = GenericArgumentNormalAst(-1, inner_type)
        return TypeAst(pos, [IdentifierAst(pos, "std"), GenericIdentifierAst(pos, "Fut", GenericArgumentGroupAst(-1, TokenAst.dummy(TokenType.TkBrackL), [inner_type], TokenAst.dummy(TokenType.TkBrackR)))])

    @staticmethod
    def arr(elem_type, pos: int = -1):
        from SPPCompiler.SemanticAnalysis.ASTs import IdentifierAst, TypeAst, GenericIdentifierAst, GenericArgumentGroupAst, GenericArgumentNormalAst, TokenAst
        from SPPCompiler.LexicalAnalysis.Tokens import TokenType
        elem_type = GenericArgumentGroupAst(-1, TokenAst.dummy(TokenType.TkBrackL), [GenericArgumentNormalAst(-1, elem_type)], TokenAst.dummy(TokenType.TkBrackR))
        return TypeAst(pos, [IdentifierAst(pos, "std"), GenericIdentifierAst(pos, "Arr", elem_type)])

    @staticmethod
    def opt(inner_type, pos: int = -1):
        from SPPCompiler.SemanticAnalysis.ASTs import IdentifierAst, TypeAst, GenericIdentifierAst, GenericArgumentGroupAst, GenericArgumentNormalAst, TokenAst
        from SPPCompiler.LexicalAnalysis.Tokens import TokenType
        inner_type = GenericArgumentNormalAst(-1, inner_type)
        return TypeAst(pos, [IdentifierAst(pos, "std"), GenericIdentifierAst(pos, "Opt", GenericArgumentGroupAst(-1, TokenAst.dummy(TokenType.TkBrackL), [inner_type], TokenAst.dummy(TokenType.TkBrackR)))])

    @staticmethod
    def tuple(types, pos: int = -1):
        from SPPCompiler.SemanticAnalysis.ASTs import IdentifierAst, TypeAst, GenericIdentifierAst, GenericArgumentGroupAst, GenericArgumentNormalAst, TokenAst
        from SPPCompiler.LexicalAnalysis.Tokens import TokenType
        types = GenericArgumentGroupAst(-1, TokenAst.dummy(TokenType.TkBrackL), [GenericArgumentNormalAst(-1, x) for x in types], TokenAst.dummy(TokenType.TkBrackR))
        return TypeAst(pos, [IdentifierAst(pos, "std"), GenericIdentifierAst(pos, "Tup", types)])

    @staticmethod
    def var(types, pos: int = -1):
        from SPPCompiler.SemanticAnalysis.ASTs import IdentifierAst, TypeAst, GenericIdentifierAst, GenericArgumentGroupAst, GenericArgumentNormalAst, TokenAst
        from SPPCompiler.LexicalAnalysis.Tokens import TokenType
        types = GenericArgumentGroupAst(-1, TokenAst.dummy(TokenType.TkBrackL), [GenericArgumentNormalAst(-1, x) for x in types], TokenAst.dummy(TokenType.TkBrackR))
        return TypeAst(pos, [IdentifierAst(pos, "std"), GenericIdentifierAst(pos, "Var", types)])

    @staticmethod
    def fun_ref(return_type, param_types, pos: int = -1):
        from SPPCompiler.SemanticAnalysis.ASTs import IdentifierAst, TypeAst, GenericIdentifierAst, GenericArgumentGroupAst, GenericArgumentNormalAst, TokenAst
        from SPPCompiler.LexicalAnalysis.Tokens import TokenType
        return_type_generic = GenericArgumentNormalAst(-1, return_type)
        param_types_generic = GenericArgumentNormalAst(-1, CommonTypes.tuple(param_types))
        types = GenericArgumentGroupAst(-1, TokenAst.dummy(TokenType.TkBrackL), [return_type_generic, param_types_generic], TokenAst.dummy(TokenType.TkBrackR))
        return TypeAst(pos, [IdentifierAst(pos, "std"), GenericIdentifierAst(pos, "FunRef", types)])

    @staticmethod
    def fun_mut(return_type, param_types, pos: int = -1):
        from SPPCompiler.SemanticAnalysis.ASTs import IdentifierAst, TypeAst, GenericIdentifierAst, GenericArgumentGroupAst, GenericArgumentNormalAst, TokenAst
        from SPPCompiler.LexicalAnalysis.Tokens import TokenType
        return_type_generic = GenericArgumentNormalAst(-1, return_type)
        param_types_generic = GenericArgumentNormalAst(-1, CommonTypes.tuple(param_types))
        types = GenericArgumentGroupAst(-1, TokenAst.dummy(TokenType.TkBrackL), [return_type_generic, param_types_generic], TokenAst.dummy(TokenType.TkBrackR))
        return TypeAst(pos, [IdentifierAst(pos, "std"), GenericIdentifierAst(pos, "FunMut", types)])

    @staticmethod
    def fun_mov(return_type, param_types, pos: int = -1):
        from SPPCompiler.SemanticAnalysis.ASTs import IdentifierAst, TypeAst, GenericIdentifierAst, GenericArgumentGroupAst, GenericArgumentNormalAst, TokenAst
        from SPPCompiler.LexicalAnalysis.Tokens import TokenType
        return_type_generic = GenericArgumentNormalAst(-1, return_type)
        param_types_generic = GenericArgumentNormalAst(-1, CommonTypes.tuple(param_types))
        types = GenericArgumentGroupAst(-1, TokenAst.dummy(TokenType.TkBrackL), [return_type_generic, param_types_generic], TokenAst.dummy(TokenType.TkBrackR))
        return TypeAst(pos, [IdentifierAst(pos, "std"), GenericIdentifierAst(pos, "FunMov", types)])

    @staticmethod
    def gen_ref(gen_type=None, ret_type=None, send_type=None, pos: int = -1):
        from SPPCompiler.SemanticAnalysis.ASTs import IdentifierAst, TypeAst, GenericIdentifierAst, GenericArgumentGroupAst, GenericArgumentNormalAst, TokenAst
        from SPPCompiler.LexicalAnalysis.Tokens import TokenType
        gen_type = GenericArgumentNormalAst(-1, gen_type or CommonTypes.void())
        ret_type = GenericArgumentNormalAst(-1, ret_type or CommonTypes.void())
        send_type = GenericArgumentNormalAst(-1, send_type or CommonTypes.void())
        return TypeAst(pos, [IdentifierAst(pos, "std"), GenericIdentifierAst(pos, "GenRef", GenericArgumentGroupAst(-1, TokenAst.dummy(TokenType.TkBrackL), [gen_type, ret_type, send_type], TokenAst.dummy(TokenType.TkBrackR)))])

    @staticmethod
    def gen_mut(gen_type=None, ret_type=None, send_type=None, pos: int = -1):
        from SPPCompiler.SemanticAnalysis.ASTs import IdentifierAst, TypeAst, GenericIdentifierAst, GenericArgumentGroupAst, GenericArgumentNormalAst, TokenAst
        from SPPCompiler.LexicalAnalysis.Tokens import TokenType
        gen_type = GenericArgumentNormalAst(-1, gen_type or CommonTypes.void())
        ret_type = GenericArgumentNormalAst(-1, ret_type or CommonTypes.void())
        send_type = GenericArgumentNormalAst(-1, send_type or CommonTypes.void())
        return TypeAst(pos, [IdentifierAst(pos, "std"), GenericIdentifierAst(pos, "GenMut", GenericArgumentGroupAst(-1, TokenAst.dummy(TokenType.TkBrackL), [gen_type, ret_type, send_type], TokenAst.dummy(TokenType.TkBrackR)))])

    @staticmethod
    def gen_mov(gen_type=None, ret_type=None, send_type=None, pos: int = -1):
        from SPPCompiler.SemanticAnalysis.ASTs import IdentifierAst, TypeAst, GenericIdentifierAst, GenericArgumentGroupAst, GenericArgumentNormalAst, TokenAst
        from SPPCompiler.LexicalAnalysis.Tokens import TokenType
        gen_type = GenericArgumentNormalAst(-1, gen_type or CommonTypes.void())
        ret_type = GenericArgumentNormalAst(-1, ret_type or CommonTypes.void())
        send_type = GenericArgumentNormalAst(-1, send_type or CommonTypes.void())
        return TypeAst(pos, [IdentifierAst(pos, "std"), GenericIdentifierAst(pos, "GenMov", GenericArgumentGroupAst(-1, TokenAst.dummy(TokenType.TkBrackL), [gen_type, ret_type, send_type], TokenAst.dummy(TokenType.TkBrackR)))])

    @staticmethod
    def type_variant_to_convention(identifier: "IdentifierAst", pos: int = -1) -> "ConventionAst":
        from SPPCompiler.LexicalAnalysis.Tokens import TokenType
        from SPPCompiler.SemanticAnalysis.ASTs.ConventionMovAst import ConventionMovAst
        from SPPCompiler.SemanticAnalysis.ASTs.ConventionRefAst import ConventionRefAst
        from SPPCompiler.SemanticAnalysis.ASTs.ConventionMutAst import ConventionMutAst
        from SPPCompiler.SemanticAnalysis.ASTs.TokenAst import TokenAst

        match identifier.value[-3:]:
            case "Ref": return ConventionRefAst(pos, TokenAst.dummy(TokenType.TkBitAnd, pos=pos))
            case "Mut": return ConventionMutAst(pos, TokenAst.dummy(TokenType.TkBitAnd, pos=pos), TokenAst.dummy(TokenType.KwMut, pos=pos))
            case "Mov": return ConventionMovAst(pos)
            case _:
                raise
