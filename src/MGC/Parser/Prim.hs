module MGC.Parser.Prim where
  import Text.Parsec
  import Text.Parsec.String

  import Numeric
  import Control.Applicative ((<*), (*>))
  import Control.Monad (liftM, (>>))
  import MGC.Syntax

  reservedWords = [ 
    "append", "bool", "break", "case", "chan", "const", "continue", "default",
    "defer", "else", "fallthrough", "float64", "for", "func", "go", "goto",
    "if", "import", "int", "interface", "map", "package", "print", "println",
    "range", "return", "rune", "select", "string", "struct", "switch", "type", "var" ]

  parens :: Parsec String u a -> Parsec String u a
  parens = between (char '(') (char ')')
  
  brackets :: Parsec String u a -> Parsec String u a
  brackets = between (char '[') (char ']')

  braces :: Parsec String u a -> Parsec String u a
  braces = between (char '{') (char '}')

  semi = lexeme' ";"

  lexeme :: String -> Parsec String u String
  lexeme s = string s <* (many (oneOf "\t\f\v"))
  lexeme' :: String -> Parsec String u String -- currently the same as lexeme. Needs to change so that it consumes \n\r
  lexeme' s = string s <* (many spaces)

  reserved :: String -> Parsec String u String
  reserved s = try $ do
    name <- lexeme s
    if (elem name reservedWords)
    then return $ name
    else fail $  name ++"is not a reserved word" 

  identifier :: Parsec String u Identifier
  identifier = try $ do
    firstChar <- letter <|> char '_'
    lastChars <- many $ oneOf (['a'..'z']++['A'..'Z']++['0'..'9'])
    let name = [firstChar] ++ lastChars
    if (elem name reservedWords)
    then fail $ "cannot use reserved word " ++ name ++" as identifier" 
    else return $ name

  identifierList = identifier `sepEndBy` (lexeme ",")

  literal = basicLit

  basicLit = intLit

  intLit ::  Parser Expression
  intLit = hexLit <|> octLit <|> decimalLit

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
