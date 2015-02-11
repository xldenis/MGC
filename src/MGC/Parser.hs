module MGC.Parser where
  import MGC.Syntax

  import Text.Parsec (try, many)
  import Text.Parsec.String
  --import Text.Parsec.Char (letter, char, digit, string, oneOf, satisfy, space, noneOf, anyChar)
  import Text.Parsec.Char
  import Text.Parsec.Combinator
  import Control.Applicative ((<$>), (<*>), (<*), (*>), (<|>))

  reservedWords = [
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
    "var"]

  binaryOps = ["||", "&&"] ++ relOps ++ addOps ++ mulOps
  relOps = ["==", "!=", "<", "<=", ">", ">="]
  addOps = ["+", "-", "|", "^"]
  mulOps = ["*", "/", "%", "<<", ">>", "&", "&^"]
  unaryOps = ["+", "-", "!", "^"]

  parens = between (char "(") (char ")")
  brackets = between (char "[") (char "]")
  braces = between (char "{") (char "}")
  semi = lexeme' ";"
  lexeme :: Parser String -> Parser String
  lexeme s = s <* (manyTill spaces (noneOf "\t\f\v"))
  lexeme' :: Parser String -> Parser String
  lexeme' s = s <* (manyTill spaces (oneOf "\n\r"))

  reserved s = try $ do
    name <- lexeme s
    if (elem name reservedWords)
    then return $ name
    else fail $  name ++"is not a reserved word" 

  identifier :: Parser Identifier
  identifier = try $ do
    name <- (++) <$> satisfy (oneOf "a-zA-Z") <*> manyTill anyChar (noneOf "_a-zA-Z0-9")
    if (elem name reservedWords)
    then fail $ "cannot use reserved word " ++ name ++" as identifier" 
    else return $ name

  topLevelDef :: Parser TopLevelDefinition
  topLevelDef = declaration <|> funcDec

  declaration :: Parser Declaration
  declaration = typeDec <|> varDec

  funcDec :: Parser FuncDecl
  funcDec = do
    reserved "func"
    name <- lexeme identifier
    sig <- signature
    case optionMaybe block of 
      Just b -> return $ FunctionDecl name sig b
      Nothing -> return $ FunctionSig name sig

  signature :: Parser Signature

  typeDec :: Parser Declaration
  typeDec = do
    string "type"
    typeSpec <|> parens $ many (typeSpec <* semi)

  typeSpec :: Parser (Identifier Type)
  typeSpec = do{return $ (identifier typeParser)}

  varDec :: Parser Declaration

  --statement :: Parser Statement
  --statement = returnStmt <|> ifStmt <|> switchStmt <|> forStmt <|> blockStmt

  --returnStmt :: Parser Statement

  --ifStmt :: Parser Statement

  --switchStmt :: Parser Statement

  --forStmt :: Parser Statement

  --blockStmt :: Parser Statement

  simpleStatement :: Parser SimpleStatement
  simpleStatement = exprStmt <|> incDec <|> assign <|> shortDec
  
  exprStmt :: Parser SimpleStatement
  exprStmt = do{return $ ExpressionStmt expression}

  incDec :: Parser SimpleStatement
  incDec = do
    e <- expression
    return (string "++" *> Dec e) <|> (string "--" *> Dec e)

  assign :: Parser SimpleStatement

  shortDec :: Parser SimpleStatement

  expression :: Parser Expression

  typeParser :: Parser Type
  typeParser = typeName <|> typeLit <|> (parens typeParser)

  typeName :: Parser Type
  typeName = do{ return $ Name identifier}

  typeLit :: Parser Type
  typeLit = arrayType <|> structType <|> pointerType <|> functionType <|> interfaceType <|> sliceType

  arrayType :: Parser TypeLit
  arrayType = do{return $ Array (brackets expression) typeParser}

  sliceType :: Parser TypeLit
  sliceType = do{char "[";char "]"; return $ Slice typeParser }

  --structType :: Parser TypeLit
  --structType = do
  --  string "struct"
  --  braces many (fieldDecl <* semi)

  --fieldDecl = do
  --  many identifier <* (char ",") <|> 

  pointerType :: Parser TypeLit
  pointerType = do 
    char "*"
    return $ Pointer typeParser

  functionType :: Parser TypeLit
  functionType = do
    string "func"
    return $ Function signature

  interfaceType :: Parser TypeLit
  interfaceType = do
    string "interface"
    return $ Interface braces many (methodSpec <* semi)

  methodSpec :: Parser MethodSpec
  methodSpec = do
    return (MethodSpec signature <|> InterfaceName identifier)
