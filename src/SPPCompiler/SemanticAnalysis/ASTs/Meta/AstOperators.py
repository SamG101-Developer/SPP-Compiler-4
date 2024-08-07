from SPPCompiler.LexicalAnalysis.Tokens import TokenType


BIN_OP_FUNCS = {
    TokenType.TkAdd: "add",
    TokenType.TkSub: "sub",
    TokenType.TkMul: "mul",
    TokenType.TkDiv: "div",
    TokenType.TkRem: "rem",
    TokenType.TkMod: "mod",
    TokenType.TkExp: "pow",
    TokenType.KwAnd: "l_and",
    TokenType.KwOr: "l_ior",
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
    TokenType.KwOr: 2,
    TokenType.KwAnd: 3,
    TokenType.TkEq: 4,
    TokenType.TkNe: 4,
    TokenType.TkLt: 4,
    TokenType.TkGt: 5,
    TokenType.TkLe: 5,
    TokenType.TkGe: 5,
    TokenType.TkSs: 5,
    TokenType.TkAdd: 6,
    TokenType.TkSub: 6,
    TokenType.TkMul: 7,
    TokenType.TkDiv: 7,
    TokenType.TkRem: 7,
    TokenType.TkMod: 7,
    TokenType.TkExp: 7,
}


__all__ = ["BIN_OP_FUNCS", "OP_PREC"]
