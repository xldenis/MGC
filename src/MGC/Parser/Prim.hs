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
    "if", "import", "int", "interface", "map", "package", "print", "println",
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
  semi' = (lexeme' ";" >> return ()) <|> spaces

  lexeme :: String -> Parser String
  lexeme s = try $ string s <* lineSpace

  lexeme' :: String -> Parser String -- currently the same as lexeme. Needs to change so that it consumes \n\r
  lexeme' s = try $ string s <* (spaces)

  lineSpace :: Parser ()
  lineSpace = try $ many (satisfy (\x -> isSpace x && not (x == '\n' || x == '\r'))) >> return ()

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
  stringLit = try $ String <$> (quotes (interpretedString) <|> ticks (many anyChar))

  interpretedString :: Parser String
  interpretedString = many $ unicodeEscape <|> escapeSeq <|> (noneOf "\n\"")

  unicodeEscape :: Parser Char
  unicodeEscape = try $ do
    len <- char '\\' *> (char 'u' <|> char 'U')
    case len of 
      'U' ->  fst . head . readLitChar . ((++) "\\") <$> count 8 hexDigit
      'u' ->  fst . head . readLitChar . ((++) "\\") <$> count 4 hexDigit

  escapeSeq :: Parser Char
  escapeSeq = try $ (char '\\') *> liftM ( fst . head . readLitChar . (:) '\\' . (flip (:) [])) (oneOf "abfnrtv\\'\"")

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
