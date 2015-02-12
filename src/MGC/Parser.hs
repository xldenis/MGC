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

  topLevelDef :: Parser TopLevelDeclaration
  topLevelDef = declaration <|> funcDec

  declaration :: Parser Declaration
  declaration = typeDec <|> varDec

  funcDec :: Parser FuncDecl
  funcDec = do
    reserved "func"
    name <- lexeme identifier
    sig <- signature
    case optionMaybe blockStmt of 
      Just b -> return $ FunctionDecl name sig b
      Nothing -> return $ FunctionSig name sig

  signature :: Parser Signature

  typeDec :: Parser Declaration
  typeDec = do
    lexeme "type"
    typeSpec <|> parens $ many (typeSpec <* semi)

  typeSpec :: Parser (Identifier Type)
  typeSpec = do{return $ (identifier typeParser)}

  varDec :: Parser Declaration
  varDec = do
    lexeme "var"
    return $ parens many (varSpec <* semi) <|> [varSpec]

  varSpec = do
    idents <- many identifier <* (lexeme ",")
    tp <- case optionMaybe typeParser of
      Just tp -> tp
      Nothing -> Unit
    lexeme "="
    exprs <- many $ expression <* (lex ",")
    if (length exprs) == (length idents)
    then return $ VarSpec idents tp exprs
    else fail $ "assign a value to every variable"

  statement :: Parser Statement
  statement = returnStmt <|> ifStmt <|> switchStmt <|> forStmt <|> blockStmt

  returnStmt :: Parser Statement
  returnStmt = do
    reserved "return"
    return $ Return expressionList

  ifStmt :: Parser Statement
  ifStmt = do
    reserved "if"  
    stmt <- optionMaybe simpleStatement 
    expr <- expression
    left <- blockStmt
    right <- reserved "else" *> option [] $ ifStmt <|> blockStmt
    return $ If stmt expr left right

  switchStmt :: Parser Statement
  switchStmt = do
    reserved "switch"
    stmt <- optionMaybe $ simpleStatement <* semi
    expr <- optionMaybe expression
    clauses <- braces many exprCaseClause
    return $ Switch stmt expr clauses

  exprCaseClause = do
    caseType <- lexeme "case" *> Just expressionList <|> lexeme "default" *> Nothing
    lexeme ":"
    return $ Case caseType (many statement <* lexeme "k")

  forStmt :: Parser Statement
  forStmt = do
    reserved "for"
    clause <- (Condition expression) <|> forClause
    body <- blockStmt
    return $ For clause body

  forClause = do
    initStmt <- simpleStatement
    semi
    cond <- (Condition expression)
    semi
    postStmt <- simpleStatement
    return $ ForClause initStmt cond postStmt

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
  shortDec = try $ do
    idents <- many identifier <* (lexeme ",")
    exprs <- expressionList
    if (length idents == length exprs)
    then return $ ShortDecl idents exprs
    else fail $ "left and right side of assign must match length"

  expression :: Parser Expression
  --expression = unaryExpr <|> binaryExp use the parsec.language stuff

  expressionList = many $ expression <* (lexeme ",")

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
    lexeme "func"
    return $ Function signature

  interfaceType :: Parser TypeLit
  interfaceType = do
    lexeme "interface"
    return $ Interface braces many (methodSpec <* semi)

  methodSpec :: Parser MethodSpec
  methodSpec = do
    return (MethodSpec signature <|> InterfaceName identifier)
