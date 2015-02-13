module MGC.Parser.Expression  where 
  import Text.Parsec.Expr
  import Text.Parsec.String
  import Text.Parsec

  import Control.Applicative ((<$>), (<*), (<*>))
  import Control.Monad ((>>), liftM2, liftM)

  import MGC.Syntax
  import MGC.Parser.Prim

  binaryOps = [("||", Or), ("&&", And)] ++ relOps ++ addOps ++ mulOps
  relOps = [("==", Eq), ("!=", NEq),("<", LessThan), ("<=", LessThanEq), (">", GreaterThan), (">=", GreaterThanEq)]
  addOps = [("+", Plus), ("-", Minus), ("|", BitOr), ("^", BitXor)]
  mulOps = [("*", Mult),("/", Div), ("%",Mod),("<<",LShift), (">>", RShift),("&", BitAnd),("&^", BitClear)]
  unaryOps = ["+", "-", "!", "^"]


  table = [ (map (\(s,tp) -> binary s tp AssocRight) relOps) ]

  binary n fun assoc = Infix (do{ op n; return $ BinaryOp fun; }) assoc
  prefix n fun = Prefix (do{op n; return $ UnaryOp fun;})
  postfix n fun = Postfix (do{op n; return $ UnaryOp fun;})

  op :: String -> Parsec String u ()
  op s = string s   >> return ()

  expression :: Parser Expression
  expression = buildExpressionParser table primaryExpr <?> "Expression"

  primaryExpr :: Parser Expression
  primaryExpr = operand <|> index <|> selector

  operand :: Parser Expression
  operand = literal <|> (parens expression)

  --conversion :: Parser Expression
  --conversion = liftM2 typeParser (parens $ expression <* optional lexeme "," )

  selector :: Parser Expression 
  selector = try $ liftM2 Selector (primaryExpr <* char '.') identifier

  index :: Parser Expression
  index = try $ liftM Index (brackets expression)

  slice :: Parser Expression
  slice = try $ brackets expression 

  --typeAssertion :: Parser Expression
  --typeAssertion  = char '.' >> parens typeParser

  --args :: Parser Expression

