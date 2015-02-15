module MGC.Parser.Type  where
  import MGC.Syntax (Type(..), MethodSpec(..), Signature(..), FieldDecl(..), Identifier)
  import MGC.Parser.Prim
  import MGC.Parser.Expression


  import Text.Parsec
  import Text.Parsec.String
  import Control.Applicative ((<$>), (<*>), (<*), (*>))
  
  typeParser :: Parser Type
  typeParser = typeLit <|> typeName <|> (parens typeParser)

  typeName :: Parser Type
  typeName = TypeName <$> (try $ reservedType)

  typeLit :: Parser Type
  typeLit = builtins <|> arrayType <|> functionType <|> pointerType <|> interfaceType <|> sliceType <|> structType

  arrayType :: Parser Type
  arrayType = try $ Array <$> (brackets expression) <*> typeParser

  sliceType :: Parser Type
  sliceType = try $ string "[]" *> (Slice <$> typeParser)

  builtins :: Parser Type
  builtins = try $ (string "int" *> return TInteger) <|> (string "float64" *> return TFloat) <|> (string "rune" *> return TRune) <|> (string "string" *> return TString) <|> (string "bool" *> return TBool)

  structType :: Parser Type
  structType = do
    reserved "struct"
    fields <- braces $ many (fieldDecl <* semi')
    return $ Struct fields

  fieldDecl = namedField <|> anonField

  tag = try $ optionMaybe stringLit
  namedField =  do
    p <- param 
    t <- tag
    return $ NamedField (fst p) (snd p) t

  anonField = try $ (AnonField <$> ((optional $ (char '*')) *> typeName) <*> tag)

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
    result <- optionMaybe $ parameters <|> ((flip (:) []) <$> ((,) [] <$> (typeParser <* spaces)))
    case result of
      Nothing -> return $Signature params []
      Just res -> return $ Signature params res

  parameters = try $ parens $ param `sepEndBy` lexeme ","

  param :: Parser ([Identifier], Type)
  param = do{ids<-(option [] identifierList); tp<-typeParser; return (ids, tp)}