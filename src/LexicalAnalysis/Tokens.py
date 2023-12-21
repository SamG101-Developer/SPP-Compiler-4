from dataclasses import dataclass
from enum import Enum


class TokenType(Enum):
    # Logical operations (AND, OR, NOT)
    TkLogicalAnd = "&&"
    TkLogicalAndAssign = "&&="
    TkLogicalOr = "||"
    TkLogicalOrAssign = "||="

    # Bitwise operations (AND, OR, XOR, NOT, SHL, SHR, ROL, ROR)
    TkBitAnd = "&"
    TkBitAndAssign = "&="
    TkBitOr = "|"
    TkBitOrAssign = "|="
    TkBitXor = "^"
    TkBitXorAssign = "^="
    TkBitShiftL = "<<"
    TkBitShiftLAssign = "<<="
    TkBitShiftR = ">>"
    TkBitShiftRAssign = ">>="
    TkBitRotateL = "<<<"
    TkBitRotateLAssign = "<<<="
    TkBitRotateR = ">>>"
    TkBitRotateRAssign = ">>>="

    # Comparison operations (EQ, NE, LE, GE, LT, GT, CMP)
    TkEq = "=="
    TkNe = "!="
    TkLe = "<="
    TkGe = ">="
    TkLt = "<"
    TkGt = ">"
    TkSs = "<=>"

    # Arithmetic operations (ADD, SUB, MUL, DIV, REM, MOD, EXP)
    TkAdd = "+"
    TkSub = "-"
    TkMul = "*"
    TkDiv = "/"
    TkRem = "%"
    TkMod = "%%"
    TkExp = "**"
    TkAddAssign = "+="
    TkSubAssign = "-="
    TkMulAssign = "*="
    TkDivAssign = "/="
    TkRemAssign = "%="
    TkModAssign = "%%="
    TkExpAssign = "**="

    # Brackets (PAREN, BRACK, BRACE)
    TkParenL = "("
    TkParenR = ")"
    TkBrackL = "["
    TkBrackR = "]"
    TkBraceL = "{"
    TkBraceR = "}"

    # Try-Types (COA)
    TkCoalesce = "??"

    # Other symbols
    TkQst = "?"
    TkVariadic = ".."
    TkColon = ":"

    TkDot = "."
    TkComma = ","
    TkAssign = "="
    TkArrowR = "->"
    TkAt = "@"
    TkUnderscore = "_"

    TkEOF = "\0"
    TkWhitespace = " "
    TkNewLine = "\n"

    # Keywords
    KwMod = "mod"
    KwCls = "cls"
    KwSup = "sup"
    KwFun = "fun"
    KwUse = "use"
    KwLet = "let"
    KwMut = "mut"
    KwIf = "if"
    KwElse = "else"
    KwWhile = "while"
    KwWith = "with"
    KwRet = "ret"
    KwYield = "yield"
    KwWhere = "where"
    KwIs = "is"
    KwAs = "as"
    KwTrue = "true"
    KwFalse = "false"
    KwSelf = "self"
    KwSelfType = "Self"
    KwOn = "on"
    KwAsync = "async"

    # Don't change order of these (regex are matched in this order)
    # 0x12 must be HexDigits not DecDigits(0) then Identifier(x12)
    LxRegex = r"r\".*\""
    LxIdentifier = r"[a-z][_a-zA-Z0-9]*"
    LxUpperIdentifier = r"[A-Z][_a-zA-Z0-9]*"
    LxBinDigits = r"0b[01]+"
    LxHexDigits = r"0x[0-9a-fA-F]+"
    LxDecDigits = r"[0-9]([0-9_]*[0-9])?"
    LxDecFloat = r"[0-9]([0-9_]*[0-9])?\.[0-9]([0-9_]*[0-9])?"
    LxDoubleQuoteStr = r"\"[^\"]*\""
    LxSingleLineComment = r"#.*"
    LxMultiLineComment = r"/\*.*\*/"

    # Unknown token to shift error to ErrFmt
    ERR = "Unknown"
    NO_TOK = ""


@dataclass
class Token:
    token_metadata: str
    token_type: TokenType

    def __str__(self):
        return self.token_metadata
