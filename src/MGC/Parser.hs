{-# LANGUAGE OverloadedStrings #-}
module MGC.Parser where
  import MGC.Syntax
  import MGC.Parser.Expression
  import MGC.Parser.Type
  import MGC.Parser.Prim

  import Text.Parsec (try, many, sepEndBy, Parsec)
  import Text.Parsec.String
  --import Text.Parsec.Char (letter, char, digit, string, oneOf, satisfy, space, noneOf, anyChar)
  import Text.Parsec.Char
  import Text.Parsec.Combinator
  import Control.Applicative ((<$>), (<*>), (<*), (*>), (<|>))
  import Control.Monad ((>>))

  mulOpParser = lexeme "*" *> return Mult

  addOpParser = string "+" *> return Plus

  topLevelDef :: Parser TopLevelDeclaration
  topLevelDef = declaration <|> funcDec

  declaration :: Parser TopLevelDeclaration
  declaration = typeDec <|> varDec

  funcDec :: Parser TopLevelDeclaration
  funcDec = do
    reserved "func"
    name <- identifier
    sig <- signature
    body <- optionMaybe blockStmt

    return $ FunctionDecl name sig body
    
  typeDec :: Parser TopLevelDeclaration
  typeDec = do
    lexeme "type"
    TypeDecl <$> ( (flip (:) []) <$> typeSpec <|> (parens $ typeSpec `sepEndBy` semi))

  typeSpec :: Parser (Identifier, Type)
  typeSpec =  (,) <$> identifier <*> typeParser

  varDec :: Parser TopLevelDeclaration
  varDec = do
    lexeme "var"
    VarDecl <$> (parens $ (varSpec `sepEndBy` semi))

  varSpec = do
    idents <- identifierList

    tp <- (\x -> case x of
      Just t -> t
      Nothing -> Unit) <$> optionMaybe typeParser

    lexeme "="
    exprs <- expressionList
    if (length exprs) == (length idents)
    then return $ VarSpec idents exprs tp
    else fail $ "assign a value to every variable"

  statement :: Parser Statement
  statement = returnStmt <|> ifStmt <|> switchStmt <|> forStmt <|> blockStmt

  returnStmt :: Parser Statement
  returnStmt = do
    reserved "return"
    Return <$> expressionList

  ifStmt :: Parser Statement
  ifStmt = do
    reserved "if"  
    stmt <- optionMaybe simpleStatement 
    expr <- expression
    left <- blockStmt
    right <- reserved "else" >> (ifStmt <|> blockStmt)
    return $ If stmt expr left right

  switchStmt :: Parser Statement
  switchStmt = do
    reserved "switch"
    stmt <- optionMaybe $ simpleStatement <* semi
    expr <- optionMaybe expression
    clauses <- braces (many exprCaseClause) 
    return $ Switch stmt expr clauses

  exprCaseClause = do
    caseType <- lexeme "case" >> (Just <$> expressionList <|> (lexeme "default" >> return Nothing))
    lexeme ":"
    stmts <- statement `sepEndBy` semi
    return $ Case caseType stmts

  forStmt :: Parser Statement
  forStmt = do
    reserved "for"
    cond <- (Condition <$> expression) <|> forClause
    body <- blockStmt
    return $ For cond body

  forClause = do
    initStmt <- simpleStatement
    semi
    cond <- expression
    semi
    postStmt <- simpleStatement
    return $ ForClause initStmt cond postStmt

  blockStmt :: Parser Statement
  blockStmt = Block <$> (braces $ statement `sepEndBy` semi)

  simpleStatement :: Parser Statement
  simpleStatement = exprStmt <|> incDec <|> assign <|> shortDec
  
  exprStmt :: Parser Statement
  exprStmt = ExpressionStmt <$> expression

  incDec :: Parser Statement
  incDec = do
    e <- expression
    (string "++" *> (return $ Inc e)) <|> (string "--" *> (return $ Dec e))

  assign :: Parser Statement
  assign = do
    lhs <- expressionList
    op <- (addOpParser <|> mulOpParser) <* lexeme "="
    rhs <- expressionList
    return $ Assignment op lhs rhs

  shortDec :: Parser Statement
  shortDec = try $ do
    idents <- identifierList
    exprs <- expressionList
    if (length idents == length exprs)
    then return $ ShortDecl idents exprs
    else fail $ "left and right side of assign must match length"

  expressionList = expression `sepEndBy` (lexeme ",")

 
