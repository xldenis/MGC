module MGC.Parser.Prim where
  import Text.Parsec
  import Text.Parsec.String
  import Data.Char 
  
  import Numeric
  import Control.Applicative ((<*), (*>), (<$>), (<*>))
  import Control.Monad (liftM, liftM2, (>>))
  import MGC.Syntax

  reservedWords = [ 
    "append", "bool", "break", "case", "chan", "const", "continue", "default",
    "defer", "else", "fallthrough", "float64", "for", "func", "go", "goto",
    "if", "import", "int", "interface", "map", "package", -- "print", "println",
    "range", "return", "rune", "select", "string", "struct", "switch", "type", "var" ]

  reservedTypes = [ "int", "interface", "float64", "bool", "int", "rune", "map", "chan", "func"]

  parens :: Parser a -> Parser a
  parens = between (lexeme "(") (lexeme ")")
  
  parens' :: Parser a -> Parser a
  parens' = between (lexeme' "(") (lexeme' ")")

  brackets :: Parser a -> Parser a
  brackets = between (char '[') (char ']')

  braces :: Parser a -> Parser a
  braces = between (lexeme' "{") (lexeme' "}")

  quotes :: Parser a -> Parser a
  quotes = between (char '"') (char '"')

  quotes' :: Parser a -> Parser a
  quotes' = between (char '\'') (char '\'')

  ticks :: Parser a -> Parser a
  ticks = between (char '`') (char '`')

  semi = lexeme ";" >> return ()

  semi' = (lexeme' ";" >> return ()) <|> fullSpace

  lexeme :: String -> Parser String
  lexeme s = try $ string s <* lineSpace

  lexeme' :: String -> Parser String -- currently the same as lexeme. Needs to change so that it consumes \n\r
  lexeme' s = try $ string s <* (spaces)

  fullSpace :: Parser ()
  fullSpace = try $ do
    many $ lineComment <|> (space >> spaces >> return ()) <|> singleComment <|> fullComment
    return ()
    
  lineSpace :: Parser ()
  lineSpace = try $ do
    many $ lineComment <|> (satisfy (\x -> isSpace x && not (x == '\n' || x == '\r')) >> return ())
    return ()

  singleComment :: Parser ()
  singleComment = try $ do
    string "//"
    manyTill anyChar (try $ char '\n')
    return ()

  fullComment :: Parser ()
  fullComment = try $ do
    string "/*"
    manyTill anyChar (try $ string "*/")
    return ()

  lineComment :: Parser ()
  lineComment = try $ do
    string "/*"
    manyTill (noneOf "\n") (try $ string "*/")
    return () 

  reserved :: String -> Parser String
  reserved s = try $ do
    name <- lexeme s
    if (elem name reservedWords)
    then return $ name
    else fail $  name ++"is not a reserved word" 

  identifier :: Parser Identifier
  identifier = try $ do
    firstChar <- letter <|> char '_'
    lastChars <- (many $ (alphaNum <|> char '_')) <* lineSpace
    let name = [firstChar] ++ lastChars
    if (elem name reservedWords)
    then fail $ "cannot use reserved word " ++ name ++" as identifier" 
    else return $ name

  reservedType :: Parser Identifier
  reservedType = try $ do
    word <- (many1 $ (alphaNum)) <* lineSpace
    if (elem word reservedWords) && not (elem word reservedTypes)
    then fail $ "cannot use reserved word " ++ word ++" as identifier" 
    else return $ word  

  identifierList = identifier `sepEndBy1` (lexeme ",")

  literal = basicLit

  basicLit = intLit <|> stringLit <|> runeLit

  runeLit :: Parser Expression
  runeLit = Rune <$> quotes' (byteLit <|> anyChar)

  byteLit :: Parser Char
  byteLit = try $ do
    char '\\'
    chars <-  (((:) <$> char 'x' <*> count 2 hexDigit)) <|>(count 3 octDigit)
    return $ (fst . head . readLitChar) ('\\':chars)

  stringLit :: Parser Expression
  stringLit = try $ (quotes (interpretedString) <|> (try $ RawString <$> (ticks (many (noneOf "`"))))) 

  interpretedString :: Parser Expression
  interpretedString = try $ IntString . concat <$> (many $ unicodeEscape <|> escapeSeq <|> (flip (:) [] <$> (noneOf "\n\"")))

  unicodeEscape :: Parser String
  unicodeEscape = try $ do
    len <- char '\\' *> (char 'u' <|> char 'U')
    case len of 
      'U' ->  ((++) "\\U") <$> count 8 hexDigit
      'u' ->  ((++) "\\u") <$> count 4 hexDigit

  escapeSeq :: Parser String
  escapeSeq = try $ (char '\\') *> liftM ((:) '\\' . (flip (:) [])) (oneOf "abfnrtv\\'\"")

  intLit ::  Parser Expression
  intLit = (hexLit <|> octLit <|> decimalLit) <* lineSpace

  octLit ::  Parser Expression
  octLit = try $ do
    char '0'
    liftM (Integer . fst . head . readOct) (many1 octDigit)

  decimalLit :: Parser Expression
  decimalLit = try $ liftM (Integer . fst . head . readDec) (many1 digit)

  hexLit :: Parser Expression
  hexLit = try $ do
    char '0'
    oneOf "xX"
    liftM (Integer . fst . head . readHex) (many1 hexDigit)

  floatLit :: Parser Expression
  floatLit = floatLitA <|> floatLitB <|> floatLitC

  floatLitA :: Parser Expression
  floatLitA = try $ do
    int <- many digit
    char '.'
    dec <- (many1 digit) <|> (return "0")
    exp <- optionMaybe $ (:) <$> (oneOf "eE") <*> ((:) <$> (oneOf "+-") <*> (many digit))
    case exp of 
      Just e -> return $ (Float . fst.head.readFloat) (int ++ "." ++ dec ++ e) 
      _ -> return $ (Float . fst.head.readFloat) (int ++ "." ++ dec)

  floatLitB :: Parser Expression
  floatLitB = try $  do
    int <- many digit
    exp <- (:) <$> (oneOf "eE") <*> ((:) <$> (oneOf "+-") <*> (many digit))
    return $ (Float .fst.head.readFloat) (int ++ exp)

  floatLitC :: Parser Expression
  floatLitC = try $ do
    char '.'
    dec <- many1 digit
    exp <- (try $ (:) <$> (oneOf "eE") <*> ((:) <$> (oneOf "+-") <*> (many digit))) <|> (return "")
    return $ (Float .fst.head.readFloat) ("0." ++ dec ++ exp)
