module MGC.Parser where
  import MGC.Syntax
  import Text.ParserCombinators.Parsec

  reserverdWords = [
    "append"
    "bool",
    "break",
    "case",
    "chan",
    "const",
    "continue",
    "default",
    "defer",
    "else",
    "fallthrough",
    "float64",
    "for",
    "func",
    "go",
    "goto",
    "if",
    "import",
    "int",
    "interface",
    "map",
    "package",
    "print",
    "println",
    "range",
    "return",
    "rune",
    "select",
    "string",
    "struct",
    "switch",
    "type",
    "var",
  ]

  binaryOps = ["||", "&&"] ++ relOps ++ addOps ++ mulOps
  relOps = ["==", "!=", "<", "<=", ">", ">="]
  addOps = ["+", "-", "|", "^"]
  mulOps = ["*", "/", "%", "<<", ">>", "&", "&^"]
  unaryOps = ["+", "-", "!", "^"]


  topLevelDef :: Parser TopLevelDefinition
  topLevelDef = declaration <|> funcDec

  declaration :: Parser Declaration
  declaration = typeDec <|> varDec

  funcDec :: Parser FuncDecl

  signature :: Parser Signature

  typeDecl :: Parser Declaration

  varDec :: Parser Declaration

  statement :: Parser Statement
  statement = returnStmt <|> ifStmt <|> switchStmt <|> forStmt <|> blockStmt

  simpleStatement :: Parser SimpleStatement
  simpleStatement = exprStmt <|> incDec <|> assign <|> shortDec
  
  type :: Parser Type
  type = typeName <|> typeLit