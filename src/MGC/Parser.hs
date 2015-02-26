{-# LANGUAGE OverloadedStrings #-}
module MGC.Parser where
  import MGC.Syntax
  import MGC.Parser.Expression
  import MGC.Parser.Type
  import MGC.Parser.Prim

  import Text.Parsec (try, many, sepEndBy, Parsec)
  import Text.Parsec.String
  import Text.Parsec.Char
  import Text.Parsec.Combinator
  import Control.Applicative ((<$>), (<*>), (<*), (*>), (<|>))
  import Control.Monad ((>>))

  mulOpParser = lexeme "*" *> return Mult

  addOpParser = string "+" *> return Plus

  package :: Parser Package

  package = do
    fullSpace
    reserved "package"
    name <- identifier 
    semi'
    content <- many topLevelDef
    return $ Package name content

  topLevelDef :: Parser TopLevelDeclaration
  topLevelDef = (declaration <|> funcDec) <* fullSpace

  declaration :: Parser TopLevelDeclaration
  declaration = Decl <$> (typeDec <|> varDec)

  funcDec :: Parser TopLevelDeclaration
  funcDec = try $ do
    reserved "func"
    name <- identifier
    sig <- signature
    body <- optionMaybe blockStmt
    return $ FunctionDecl name sig body
    
  typeDec :: Parser Statement
  typeDec = try $ do
    lexeme "type"
    TypeDecl <$> ( (flip (:) []) <$> typeSpec <|> (parens' $ typeSpec `sepEndBy` semi'))

  typeSpec :: Parser TypeSpec
  typeSpec =  TypeSpec <$> identifier <*> typeParser

  varDec :: Parser Statement
  varDec = try $ do
    lexeme "var"
    VarDecl <$> ((flip (:) []) <$> varSpec <* semi' <|> (parens' $ (varSpec `sepEndBy` semi')))

  varSpec = try $ do
    idents <- identifierList

    tp <- optionMaybe typeParser
    exprs <- (try $ lexeme "=" *> expressionList) <|> (return [])
    return $ VarSpec idents exprs tp

  statement :: Parser Statement
  statement = varDec <|> typeDec <|> simpleStatement <|> returnStmt <|> ifStmt <|> 
    switchStmt <|> forStmt <|> blockStmt <|> breakStmt <|> 
    contStmt <|> ((return Empty) <* (char ';' >> fullSpace))

  returnStmt :: Parser Statement
  returnStmt = do
    reserved "return"
    Return <$> expressionList

  breakStmt :: Parser Statement
  breakStmt = (reserved "break" <* semi') *> return Break

  contStmt :: Parser Statement
  contStmt = (reserved "continue" <* semi') *> return Break

  ifStmt :: Parser Statement
  ifStmt = do
    reserved "if"  
    stmt <- optionMaybe (try $ simpleStatement <* semi)
    expr <- expression
    left <- blockStmt
    right <- (reserved "else" >> (ifStmt <|> blockStmt)) <|> return Empty
    return $ If stmt expr left right

  switchStmt :: Parser Statement
  switchStmt = do
    reserved "switch"
    stmt <- optionMaybe $ try $ simpleStatement <* (semi)
    expr <- optionMaybe expression
    clauses <- braces (many exprCaseClause) 
    return $ Switch stmt expr clauses

  exprCaseClause :: Parser SwitchClause
  exprCaseClause = try $ do
    caseType <- (lexeme "case" >> Just <$> expressionList) <|> (lexeme "default" >> return Nothing)
    lexeme' ":"
    stmts <- statement `sepEndBy` semi'
    return $ (caseType, stmts)

  forStmt :: Parser Statement
  forStmt = do
    reserved "for"
    cond <- ((try $ semi >> semi) >> (return $ Nothing)) <|>
      (optionMaybe $ forClause 
        <|> (try $ Condition <$> expression) 
        <|> (try $ Condition <$> between (semi) (semi) expression))


    body <- blockStmt
    return $ For cond body

  forClause :: Parser ForCond
  forClause = try $ do
    initStmt <- simpleStatement <|> (lookAhead (try $ char ';') >> return Empty)
    semi
    cond <- expression
    semi
    postStmt <- simpleStatement <|> (lookAhead (try $ char ';') >> return Empty)
    return $ ForClause initStmt cond postStmt

  blockStmt :: Parser Statement
  blockStmt = Block <$> (braces $ statement `sepEndBy` (semi'))

  simpleStatement :: Parser Statement
  simpleStatement = try $ incDec <|> assign <|> opAssign <|> shortDec <|> exprStmt
  
  exprStmt :: Parser Statement
  exprStmt = try $ ExpressionStmt <$> expression

  incDec :: Parser Statement
  incDec = try $ do
    e <- expression
    (lexeme "++" *> (return $ Inc e)) <|> (lexeme "--" *> (return $ Dec e))

  assign :: Parser Statement
  assign = try $ do
    lhs <- expressionList
    lexeme "=" -- wrong
    rhs <- expressionList
    return $ Assignment Eq lhs rhs

  opAssign :: Parser Statement
  opAssign = try $ do
    lhs <- expression
    op <- (addOpParser <|> mulOpParser) <* lexeme "=" -- wrong
    rhs <- expression
    return $ Assignment op [lhs] [rhs]

  shortDec :: Parser Statement
  shortDec = try $ do
    idents <- identifierList
    lexeme ":="
    exprs  <- expressionList
    return $ ShortDecl idents exprs

