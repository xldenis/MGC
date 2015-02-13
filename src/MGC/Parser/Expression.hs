module MGC.Parser.Expression  where 
  import Text.Parsec.Expr
  import Text.Parsec.String
  import Text.Parsec

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
      [Postfix (selector) ]
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
  opLetter = "+-/%*=!<>|&^"


  op :: String -> Parser ()
  op s = try $ string s >> notFollowedBy (oneOf opLetter) >> lineSpace

  expression :: Parser Expression
  expression = buildExpressionParser table primaryExpr <?> "Expression"

  primaryExpr :: Parser Expression
  primaryExpr = (operand) <* lineSpace

  operand :: Parser Expression
  operand = literal <|> (parens expression)

  --conversion :: Parser Expression
  --conversion = liftM2 typeParser (parens $ expression <* optional lexeme "," )

  --selector :: Parser Expression 
  selector = try $ do{char '.'; i <- identifier;  return $ (flip Selector) i}

  index :: Parser Expression
  index = try $ liftM Index (brackets expression)

  slice :: Parser Expression
  slice = try $ brackets $ do 
    e1 <- expression 
    lexeme ":"
    e2 <- expression
    a <- optionMaybe (lexeme ":" *> expression)
    case a of
      Just e3 -> return $ FullSlice e1 e2 e3
      Nothing -> return $ SimpleSlice e1 e2
  --typeAssertion :: Parser Expression
  --typeAssertion  = char '.' >> parens typeParser

  --args :: Parser Expression

