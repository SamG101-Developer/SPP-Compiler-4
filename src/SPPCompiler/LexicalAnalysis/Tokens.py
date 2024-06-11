from dataclasses import dataclass
from enum import Enum
import json_fix


class TokenType(Enum):
    # Tokens
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

    TkEOF = "<EOF>"
    TkWhitespace = " "
    TkNewLine = "\n"

    # Keywords
    # Module level declarations
    KwMod = "mod"
    KwCls = "cls"
    KwSup = "sup"
    KwFun = "fun"
    KwCor = "cor"
    KwUse = "use"

    # Variable declarations
    KwLet = "let"
    KwMut = "mut"

    # Control flow
    KwCase = "case"
    KwElse = "else"
    KwLoop = "loop"
    KwWith = "with"
    KwThen = "then"

    # Control flow exit
    KwRet = "ret"
    KwGen = "gen"

    # Type helpers
    KwWhere = "where"
    KwAs = "as"
    KwIs = "is"

    # Types
    KwTrue = "true"
    KwFalse = "false"
    KwSelf = "self"
    KwSelfType = "Self"

    # Misc
    KwOn = "on"
    KwAsync = "async"

    # Lexemes
    # Don't change order of these (regex are matched in this order)
    # 0x12 must be HexDigits not DecDigits(0) then Identifier(x12)
    LxRegex = r"r\".*\""

    LxIdentifier = r"[a-z][_a-zA-Z0-9]*"
    LxUpperIdentifier = r"[A-Z][_a-zA-Z0-9]*"

    LxBinDigits = r"0b[01]+"
    LxHexDigits = r"0x[0-9a-fA-F]+"
    LxDecInteger = r"[0-9]([0-9_]*[0-9])?"
    LxDecDecimal = r"[0-9]([0-9_]*[0-9])?\.[0-9]([0-9_]*[0-9])?"

    LxDoubleQuoteStr = r"\"[^\"]*\""
    LxMultiLineComment = r"##[^#]*##"
    LxSingleLineComment = r"#.*"

    # Unknown token to shift error to ErrFmt
    ERR = "Unknown"
    NO_TOK = ""

    def __json__(self) -> str:
        return self.value


@dataclass
class Token:
    token_metadata: str
    token_type: TokenType

    def __str__(self):
        return self.token_metadata
