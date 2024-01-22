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

OP_PREC = {
    TokenType.TkCoalesce: 1,
    TokenType.TkLogicalOr: 2,
    TokenType.TkLogicalAnd: 3,
    TokenType.TkEq: 4,
    TokenType.TkNe: 4,
    TokenType.TkLt: 4,
    TokenType.TkGt: 5,
    TokenType.TkLe: 5,
    TokenType.TkGe: 5,
    TokenType.TkSs: 5,
    TokenType.KwIs: 5,
    TokenType.TkBitShiftL: 5,
    TokenType.TkBitShiftR: 5,
    TokenType.TkBitRotateL: 5,
    TokenType.TkBitRotateR: 5,
    TokenType.TkAdd: 6,
    TokenType.TkSub: 6,
    TokenType.TkBitOr: 6,
    TokenType.TkBitXor: 6,
    TokenType.TkMul: 7,
    TokenType.TkDiv: 7,
    TokenType.TkRem: 7,
    TokenType.TkMod: 7,
    TokenType.TkExp: 7,
    TokenType.TkBitAnd: 7,
}
