module MGC.Parser.Expression  where 
  import Text.Parsec.Expr
  import Text.Parsec.String
  import Text.Parsec

  import {-# SOURCE #-} MGC.Parser.Type

  import Control.Applicative ((<$>), (<*), (*>), (<*>))
  import Control.Monad ((>>), liftM2, liftM)

  import MGC.Syntax
  import MGC.Parser.Prim

  binaryOps = [("||", Or), ("&&", And)] ++ relOps ++ addOps ++ mulOps
  relOps = [("==", Eq), ("!=", NEq),("<", LessThan), ("<=", LessThanEq), (">", GreaterThan), (">=", GreaterThanEq)]
  addOps = [("+", Plus), ("-", Minus), ("|", BitOr), ("^", BitXor)]
  mulOps = [("*", Mult),("/", Div), ("%",Mod),("<<",LShift), (">>", RShift),("&", BitAnd),("&^", BitClear)]
  unaryOps = [("+", Pos), ("-",Neg), ("!", Not), ("^", BComp) ]


  table = [
     [ Postfix (selector), Postfix (index), Postfix (slice)]
   , (map (\(s,tp) -> prefix s tp) unaryOps)
   , (map (\(s,tp) -> binary s tp AssocLeft) mulOps)
   , (map (\(s,tp) -> binary s tp AssocLeft) addOps)
   , (map (\(s,tp) -> binary s tp AssocLeft) relOps)
   , [binary "&&" (And) AssocLeft]
   , [binary "||" (Or)  AssocLeft]
   ]

  binary n fun assoc = Infix (do{ op n; return $ BinaryOp fun; }) assoc
  prefix n fun = Prefix (do{op n; return $ UnaryOp fun;})
  postfix n fun = Postfix (do{op n; return $ UnaryOp fun;})

  opLetter :: [Char]
  opLetter = "/%*=!<>|&^"

  op :: String -> Parser ()
  op s = try $ do 
    o <- string s
    case o of
      "+" -> notFollowedBy (oneOf $opLetter ++ "+")
      "-" -> notFollowedBy (oneOf $opLetter ++ "-")
      _  -> notFollowedBy (oneOf opLetter)
    lineSpace

  expression :: Parser Expression
  expression = try (buildExpressionParser table primaryExpr <?> "Expression")

  primaryExpr :: Parser Expression
  primaryExpr = (operand) <* lineSpace

  operand :: Parser Expression
  operand = literal <|>  args <|> name <|> conversion <|> (parens expression)

  name :: Parser Expression
  name = (Name <$> identifier) <|> (QualName <$> identifier <*> identifier)

  conversion :: Parser Expression
  conversion = try $ Conversion <$>  typeParser <*> (parens $ expression <* (optional $ lexeme "," ))

  selector :: Parser (Expression -> Expression)
  selector = try $ do{lexeme' "."; i <- identifier;  return $ (flip Selector) i}

  index :: Parser (Expression -> Expression)
  index = try $ liftM (flip Index) (brackets expression <* lineSpace)

  slice :: Parser (Expression -> Expression)
  slice = try $ brackets $ do 
    e1 <- expression 
    lexeme ":"
    e2 <- expression
    a <- optionMaybe (lexeme ":" *> expression)
    case a of
      Just e3 -> return $ (\x -> FullSlice x e1 e2 e3)
      Nothing -> return $ (\x -> SimpleSlice x e1 e2)

  args :: Parser (Expression)
  args = try $ Arguments <$> (Name <$> reservedFunc) <*> parens expressionList

  expressionList = expression `sepEndBy` (lexeme ",")