module MGC.Parser.Type  where
  import MGC.Syntax (Type(..), MethodSpec(..), Signature(..))
  import MGC.Parser.Prim
  import MGC.Parser.Expression


  import Text.Parsec
  import Text.Parsec.String
  import Control.Applicative ((<$>), (<*>), (<*), (*>))
  
  typeParser :: Parser Type
  typeParser = typeLit <|> typeName <|> (parens typeParser)

  typeName :: Parser Type
  typeName = TypeName <$> reservedType

  typeLit :: Parser Type
  typeLit = builtins <|> arrayType <|> functionType <|> interfaceType <|> sliceType

  arrayType :: Parser Type
  arrayType = Array <$> (brackets expression) <*> typeParser

  sliceType :: Parser Type
  sliceType = try $ do{char '[';char ']'; tp <- typeParser; return $ Slice tp }

  builtins :: Parser Type
  builtins = (string "int" *> return TInteger) <|> (string "float64" *> return TFloat) <|> (string "rune" *> return TRune) <|> (string "string" *> return TString)

  structType :: Parser Type
  structType = do {return Struct}
  --structType = do
  --  string "struct"
  --  braces many (fieldDecl <* semi)

  --fieldDecl = do
  --  many identifier <* (char ",") <|> 

  pointerType :: Parser Type
  pointerType = try $ do 
    char '*'
    Pointer <$> typeParser

  functionType :: Parser Type
  functionType = try $ do
    lexeme "func"
    Function <$> signature

  interfaceType :: Parser Type
  interfaceType = try $ do
    lexeme "interface"
    Interface <$> (braces $ many (methodSpec <* semi))

  methodSpec :: Parser MethodSpec
  methodSpec = do
    (MethodSpec <$> identifier <*> signature <|> InterfaceName <$> identifier)

  signature :: Parser Signature
  signature = do
    params <- parameters
    result <- optionMaybe parameters
    return $ Signature params result

  parameters = parens $ do{ids<-(option [] identifierList); tp<-typeParser; return (ids, tp)}`sepEndBy` lexeme ","
 