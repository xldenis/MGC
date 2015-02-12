{-# LANGUAGE OverloadedStrings #-}
module MGC.Parser where
  import MGC.Syntax

  import Text.Parsec (try, many, sepEndBy, Parsec)
  import Text.Parsec.String
  --import Text.Parsec.Char (letter, char, digit, string, oneOf, satisfy, space, noneOf, anyChar)
  import Text.Parsec.Char
  import Text.Parsec.Combinator
  import Control.Applicative ((<$>), (<*>), (<*), (*>), (<|>))

  reservedWords = [
    "append",
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

  parens :: Parsec String u a -> Parsec String u a
  parens = between (char '(') (char ')')
  
  brackets :: Parsec String u a -> Parsec String u a
  brackets = between (char '[') (char ']')

  braces :: Parsec String u a -> Parsec String u a
  braces = between (char '{') (char '}')

  semi = lexeme' ";"

  lexeme :: String -> Parsec String u String
  lexeme s = string s <* (manyTill spaces (noneOf "\t\f\v"))
  lexeme' :: String -> Parsec String u String -- currently the same as lexeme. Needs to change so that it consumes \n\r
  lexeme' s = string s <* (manyTill spaces (oneOf "\n\r"))

  addOpParser :: Parsec String u AddOp
  addOpParser = (char '+' *> return Plus) <|> (char '-' *> return Minus) <|> (char '|' *> return BinBitOr) <|> (char '^' *> return BinBitXor)

  mulOpParser :: Parsec String u MulOp
  mulOpParser =(char '*' *> return Mult) <|> (char '/' *> return Div) <|> (char '%' *> return Mod) <|> 
    (string "<<" *> return LShift) <|> (string ">>" *> return RShift) <|> 
    (char '&' *> return BinBitAnd) <|> (string "&^" *> return BinBitClear )

  reserved :: String -> Parsec String u String
  reserved s = try $ do
    name <- lexeme s
    if (elem name reservedWords)
    then return $ name
    else fail $  name ++"is not a reserved word" 

  identifier :: Parsec String u Identifier
  identifier = try $ do
    firstChar <- oneOf "a-zA-Z"
    lastChars <- manyTill anyChar (noneOf "_a-zA-Z0-9")
    let name = [firstChar] ++ lastChars
    if (elem name reservedWords)
    then fail $ "cannot use reserved word " ++ name ++" as identifier" 
    else return $ name

  identifierList = identifier `sepEndBy` (lexeme ",")

  topLevelDef :: Parsec String u TopLevelDeclaration
  topLevelDef = declaration <|> funcDec

  declaration :: Parsec String u Declaration
  declaration = typeDec <|> varDec

  funcDec :: Parsec String u FuncDecl
  funcDec = do
    reserved "func"
    name <- identifier
    sig <- signature
    case optionMaybe blockStmt of 
      Just b -> return $ FunctionDecl name sig b
      Nothing -> return $ FunctionSig name sig

  signature :: Parsec String u Signature
  signature = do
    params <- parameters
    result <- optionMaybe parameters
    return $ Signature params result 

  parameters = parens $ ((option [] identifierList) typeParser) `sepEndBy` lexeme ","

  typeDec :: Parsec String u Declaration
  typeDec = do
    lexeme "type"
    typeSpec <|> parens $ many (typeSpec <* semi)

  typeSpec :: Parsec String u (Identifier, Type)
  typeSpec = do{return $ (identifier typeParser)}

  varDec :: Parsec String u Declaration
  varDec = do
    lexeme "var"
    parens $ many (varSpec <* semi) <|> [varSpec]

  varSpec = do
    idents <- identifierList
    tp <- case optionMaybe typeParser of
      Just tp -> tp
      Nothing -> Unit
    lexeme "="
    exprs <- expressionList
    if (length exprs) == (length idents)
    then return $ VarSpec idents exprs tp
    else fail $ "assign a value to every variable"

  statement :: Parsec String u Statement
  statement = returnStmt <|> ifStmt <|> switchStmt <|> forStmt <|> blockStmt

  returnStmt :: Parsec String u Statement
  returnStmt = do
    reserved "return"
    Return <$> expressionList

  ifStmt :: Parsec String u Statement
  ifStmt = do
    reserved "if"  
    stmt <- optionMaybe simpleStatement 
    expr <- expression
    left <- blockStmt
    right <- reserved "else" *> (option [] $ ifStmt <|> blockStmt)
    return $ If stmt expr left right

  switchStmt :: Parsec String u Statement
  switchStmt = do
    reserved "switch"
    stmt <- optionMaybe $ simpleStatement <* semi
    expr <- optionMaybe expression
    clauses <- braces (many exprCaseClause) 
    return $ Switch stmt expr clauses

  exprCaseClause = do
    caseType <- lexeme "case" *> (Just <$> expressionList <|> lexeme "default" *> Nothing)
    lexeme ":"
    stmts <- statement `sepEndBy` semi
    return $ Case caseType stmts

  forStmt :: Parsec String u Statement
  forStmt = do
    reserved "for"
    let clause = (Condition <$> expression) <|> forClause
    body <- blockStmt
    return $ For clause body

  forClause = do
    initStmt <- simpleStatement
    semi
    cond <- expression
    semi
    postStmt <- simpleStatement
    return $ ForClause initStmt cond postStmt

  blockStmt :: Parsec String u Statement
  blockStmt = Block <$> (braces $ statement `sepEndBy` semi)

  simpleStatement :: Parsec String u SimpleStatement
  simpleStatement = exprStmt <|> incDec <|> assign <|> shortDec
  
  exprStmt :: Parsec String u SimpleStatement
  exprStmt = ExpressionStmt <$> expression

  incDec :: Parsec String u SimpleStatement
  incDec = do
    e <- expression
    (string "++" *> (return $ Inc e)) <|> (string "--" *> (return $ Dec e))

  assign :: Parsec String u SimpleStatement
  assign = do
    lhs <- expressionList
    op <- (addOpParser <|> mulOpParser) <* lexeme "="
    rhs <- expressionList
    return $ Assignment op lhs rhs

  shortDec :: Parsec String u SimpleStatement
  shortDec = try $ do
    idents <- identifierList
    exprs <- expressionList
    if (length idents == length exprs)
    then return $ ShortDecl idents exprs
    else fail $ "left and right side of assign must match length"

  expression :: Parsec String u Expression
  --expression = unaryExpr <|> binaryExp use the parsec.language stuff
  expression = do{return $ Operand}

  expressionList = expression `sepEndBy` (lexeme ",")

  typeParser :: Parsec String u Type
  typeParser = typeName <|> typeLit <|> (parens typeParser)

  typeName :: Parsec String u Type
  typeName = do{ return $ Name identifier}

  typeLit :: Parsec String u Type
  typeLit = arrayType <|> structType <|> pointerType <|> functionType <|> interfaceType <|> sliceType

  arrayType :: Parsec String u TypeLit
  arrayType = do{ Array <$> (brackets expression) <*> typeParser}

  sliceType :: Parsec String u TypeLit
  sliceType = do{char '[';char ']'; tp <- typeParser; return $ Slice tp }

  structType :: Parsec String u TypeLit
  structType = do {return Struct}
  --structType = do
  --  string "struct"
  --  braces many (fieldDecl <* semi)

  --fieldDecl = do
  --  many identifier <* (char ",") <|> 

  pointerType :: Parsec String u TypeLit
  pointerType = do 
    char '*'
    Pointer <$> typeParser

  functionType :: Parsec String u TypeLit
  functionType = do
    lexeme "func"
    Function <$> signature

  interfaceType :: Parsec String u TypeLit
  interfaceType = do
    lexeme "interface"
    Interface <$> (braces $ many (methodSpec <* semi))

  methodSpec :: Parsec String u MethodSpec
  methodSpec = do
    (MethodSpec <$> identifier <*> signature <|> InterfaceName <$> identifier)
