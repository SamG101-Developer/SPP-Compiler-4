from abc import ABC, abstractmethod

from src.LexicalAnalysis.Tokens import TokenType
from src.SemanticAnalysis.Symbols.Scopes import ScopeHandler


class SemanticAnalysis(ABC):
    @abstractmethod
    def do_semantic_analysis(self, scope_handler: ScopeHandler, **kwargs) -> None:
        pass


BIN_OP_FUNCS = {
    TokenType.TkAdd: "add",
    TokenType.TkSub: "sub",
    TokenType.TkMul: "mul",
    TokenType.TkDiv: "div",
    TokenType.TkRem: "rem",
    TokenType.TkMod: "mod",
    TokenType.TkExp: "pow",
    TokenType.TkLogicalAnd: "and",
    TokenType.TkLogicalOr: "or",
    TokenType.TkBitAnd: "bit_and",
    TokenType.TkBitOr: "bit_or",
    TokenType.TkBitXor: "bit_xor",
    TokenType.TkBitShiftL: "bit_shl",
    TokenType.TkBitShiftR: "bit_shr",
    TokenType.TkBitRotateL: "bit_rol",
    TokenType.TkBitRotateR: "bit_ror",
    TokenType.TkEq: "eq",
    TokenType.TkNe: "ne",
    TokenType.TkLt: "lt",
    TokenType.TkGt: "gt",
    TokenType.TkLe: "le",
    TokenType.TkGe: "ge",
    TokenType.TkSs: "cmp",
}
