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
  lexeme s = string s <* (manyTill spaces (noneOf "\t\f\v"))
  lexeme' :: String -> Parsec String u String -- currently the same as lexeme. Needs to change so that it consumes \n\r
  lexeme' s = string s <* (manyTill spaces (oneOf "\n\r"))

  reserved :: String -> Parsec String u String
  reserved s = try $ do
    name <- lexeme s
    if (elem name reservedWords)
    then return $ name
    else fail $  name ++"is not a reserved word" 

  identifier :: Parsec String u Identifier
  identifier = try $ do
    firstChar <- letter
    lastChars <- manyTill anyChar (noneOf $ ['a'..'z']++['A'..'Z']++['0'..'9'])
    let name = [firstChar] ++ lastChars
    if (elem name reservedWords)
    then fail $ "cannot use reserved word " ++ name ++" as identifier" 
    else return $ name

  identifierList = identifier `sepEndBy` (lexeme ",")

  literal = basicLit

  basicLit = intLit -- <|> floatLit <|> runeLit <|> stringLit

  intLit ::  Parser Expression
  intLit = octLit

  octLit ::  Parser Expression
  octLit = do
    char '0'
    liftM (Integer . fst . head . readOct) (many octDigit)

  decimalLit :: Integral a => Parser a
  decimalLit = liftM (fst . head . readDec) (many digit)