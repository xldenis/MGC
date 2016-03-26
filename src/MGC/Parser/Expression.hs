module MGC.Parser.Expression  where
  import Text.Parsec.Expr
  import Text.Parsec.String
  import Text.Parsec

  import {-# SOURCE #-} MGC.Parser.Type

  import Control.Applicative ((<$>), (<*), (*>), (<*>))
  import Control.Monad (liftM)

  import MGC.Syntax
  import MGC.Parser.Prim

  binaryOps = [("||", Or), ("&&", And)] ++ relOps ++ addOps ++ mulOps
  relOps = [("==", Eq), ("!=", NEq),("<", LessThan), ("<=", LessThanEq), (">", GreaterThan), (">=", GreaterThanEq)]
  addOps = [("+", Plus), ("-", Minus), ("|", BitOr), ("^", BitXor)]
  mulOps = [("*", Mult),("/", Div), ("%",Mod),("<<",LShift), (">>", RShift),("&", BitAnd),("&^", BitClear)]
  unaryOps = [("+", Pos), ("-",Neg), ("!", Not), ("^", BComp) ]

  table = [
     map (\(s,tp) -> prefix s tp) unaryOps
   , map (\(s,tp) -> binary s tp AssocLeft) mulOps
   , map (\(s,tp) -> binary s tp AssocLeft) addOps
   , map (\(s,tp) -> binary s tp AssocLeft) relOps
   , [binary "&&" And AssocLeft]
   , [binary "||" Or  AssocLeft]
   ]

  binary n fun assoc = Infix (do{ op n; return $ BinaryOp () fun; }) assoc
  prefix n fun = Prefix (do{op n; return $ UnaryOp () fun;})
  postfix n fun = Postfix (do{op n; return $ UnaryOp () fun;})

  opLetter :: String
  opLetter = "/%*=<>|&^"

  op :: String -> Parser ()
  op s = try $ do
    o <- string s
    case o of
      "+" -> notFollowedBy (oneOf $ opLetter ++ "+")
      "-" -> notFollowedBy (oneOf $ opLetter ++ "-")
      _  -> notFollowedBy (oneOf opLetter)
    lineSpace

  expression :: Parser (Expression ())
  expression = try (buildExpressionParser table primaryExpr <?> "Expression")

  primaryExpr :: Parser (Expression ())
  primaryExpr = try $ do
    op <- operand
    consumePrimaryExpr op

  consumePrimaryExpr :: Expression () -> Parser (Expression ())
  consumePrimaryExpr e = try $ do
    res <- optionMaybe (selector e <|> index e)
    case res of
      Nothing -> return e
      Just x -> consumePrimaryExpr x

  operand :: Parser (Expression ())
  operand = (literal <|> conversion <|>  args <|> name <|> parens expression) <* lineSpace

  name :: Parser (Expression ())
  name = (Name () <$> identifier) <|> (QualName <$> identifier <*> identifier)

  conversion :: Parser (Expression ())
  conversion = try $ Conversion () <$>  (builtins <|> parens builtins) <*> parens (expression <* (optional $ lexeme "," ))

  selector :: Expression () -> Parser (Expression ())
  selector e = try $ do{lexeme' "."; i <- identifier;  return $ Selector () e i}

  index :: Expression () -> Parser (Expression ())
  index e = try $ liftM (Index () e) (brackets expression <* lineSpace)

  slice :: Expression () -> Parser (Expression ())
  slice e = try $ brackets $ do
    e1 <- expression
    lexeme ":"
    e2 <- expression
    a <- optionMaybe (lexeme ":" *> expression)
    case a of
      Just e3 -> return (FullSlice () e e1 e2 e3)
      Nothing -> return (SimpleSlice () e e1 e2)

  args :: Parser (Expression ())
  args = try $ Arguments () <$> (Name () <$> (reservedFunc <|> parens reservedFunc)) <*> parens expressionList

  expressionList = expression `sepEndBy` lexeme ","
