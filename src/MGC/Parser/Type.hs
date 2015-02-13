module MGC.Parser.Type (typeParser, signature) where
  import MGC.Syntax (Type(..), MethodSpec(..), Signature(..))
  import MGC.Parser.Prim
  import MGC.Parser.Expression


  import Text.Parsec
  import Text.Parsec.String
  import Control.Applicative ((<$>), (<*>), (<*), (*>))
  
  typeParser :: Parser Type
  typeParser = typeName <|> typeLit <|> (parens typeParser)

  typeName :: Parser Type
  typeName = Name <$> identifier

  typeLit :: Parser Type
  typeLit = arrayType <|> structType <|> pointerType <|> functionType <|> interfaceType <|> sliceType

  arrayType :: Parser Type
  arrayType = Array <$> (brackets expression) <*> typeParser

  sliceType :: Parser Type
  sliceType = do{char '[';char ']'; tp <- typeParser; return $ Slice tp }

  structType :: Parser Type
  structType = do {return Struct}
  --structType = do
  --  string "struct"
  --  braces many (fieldDecl <* semi)

  --fieldDecl = do
  --  many identifier <* (char ",") <|> 

  pointerType :: Parser Type
  pointerType = do 
    char '*'
    Pointer <$> typeParser

  functionType :: Parser Type
  functionType = do
    lexeme "func"
    Function <$> signature

  interfaceType :: Parser Type
  interfaceType = do
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
 