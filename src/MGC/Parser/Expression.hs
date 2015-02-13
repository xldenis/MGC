module MGC.Parser.Expression (expression)  where 
  import Text.Parsec.Expr
  import Text.Parsec.String
  import Text.Parsec

  import Control.Applicative ((<$>), (<*), (<*>))
  import Control.Monad ((>>), liftM2)

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
  primaryExpr = selector <|> index <|> slice 

  operand :: Parser Expression
  operand = literal <|> (parens expression)

  --conversion :: Parser Expression
  --conversion = liftM2 typeParser (parens $ expression <* optional lexeme "," )

  selector :: Parser Expression 
  selector = liftM2 Selector primaryExpr identifier

  index :: Parser Expression
  index = liftM2 Selector (brackets expression) identifier

  slice :: Parser Expression
  slice = brackets expression 

  --typeAssertion :: Parser Expression
  --typeAssertion  = char '.' >> parens typeParser

  --args :: Parser Expression

