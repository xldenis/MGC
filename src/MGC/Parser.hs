{-# LANGUAGE OverloadedStrings #-}
module MGC.Parser where
  import MGC.Syntax
  import MGC.Parser.Expression
  import MGC.Parser.Type
  import MGC.Parser.Prim

  import Text.Parsec (try, many)
  import Text.Parsec.String
  import Text.Parsec.Char
  import Text.Parsec.Combinator
  import Control.Applicative ((<$>), (<*>), (<*), (*>), (<|>))
  import Control.Monad ((>>))

  mulOpParser = do
    op <- many1 (oneOf $  concat (map (fst) mulOps))
    case lookup op mulOps of
      Just op' -> return op'
      _ -> fail "Invalid AddOp"

  addOpParser = do
    op <- many1 (oneOf $ concat (map (fst) addOps))
    case lookup op addOps of
      Just op' -> return op'
      _ -> fail "Invalid AddOp"

  package :: Parser (Package ())

  package = do
    fullSpace
    reserved "package"
    name <- identifier 
    semi'
    content <- many topLevelDef
    return $ Package name content

  topLevelDef :: Parser (TopLevelDeclaration ())
  topLevelDef = (declaration <|> funcDec) <* fullSpace

  declaration :: Parser (TopLevelDeclaration ())
  declaration = Decl <$> ((typeDec <* semi') <|> (varDec <* semi'))

  funcDec :: Parser (TopLevelDeclaration ())
  funcDec = try $ do
    reserved "func"
    name <- identifier
    sig <- signature
    body <- optionMaybe blockStmt
    return $ FunctionDecl name sig body
    
  typeDec :: Parser (Statement ())
  typeDec = try $ do
    lexeme "type"
    TypeDecl <$> ( (flip (:) []) <$> typeSpec <|> (parens' $ typeSpec `sepEndBy` semi'))

  typeSpec :: Parser TypeSpec
  typeSpec =  TypeSpec <$> identifier <*> typeParser

  varDec :: Parser (Statement ())
  varDec = try $ do
    lexeme "var"
    VarDecl <$> ((flip (:) []) <$> (varSpec) <|> (parens' $ (varSpec `sepEndBy` semi')))

  varSpec = try $ do
    idents <- identifierList

    tp <- optionMaybe typeParser
    exprs <- (try $ lexeme "=" *> expressionList) <|> (return [])
    return $ VarSpec idents exprs tp

  statement :: Parser (Statement ())
  statement = ((varDec <|> typeDec <|> simpleStatement <|> returnStmt <|> ifStmt <|> 
    switchStmt <|> forStmt <|> blockStmt <|> breakStmt <|> fallthroughStmt <|>
    contStmt) <* semi') <|> ((return Empty) <* (char ';' >> fullSpace))

  returnStmt :: Parser (Statement ())
  returnStmt = do
    reserved "return"
    Return <$> expressionList

  breakStmt :: Parser (Statement ())
  breakStmt = (reserved "break") *> return Break

  contStmt :: Parser (Statement ())
  contStmt = (reserved "continue") *> return Continue
  
  fallthroughStmt :: Parser (Statement ())
  fallthroughStmt = (reserved "fallthrough") *> return Fallthrough
  
  ifStmt :: Parser (Statement ())
  ifStmt = do
    reserved "if"  
    stmt <- optionMaybe (try $ (simpleStatement <|> (return Empty)) <* semi)
    expr <- expression
    left <- blockStmt
    right <- (reserved' "else" >> (ifStmt <|> blockStmt)) <|> return Empty
    return $ If stmt expr left right

  switchStmt :: Parser (Statement ())
  switchStmt = do
    reserved "switch"
    stmt <- optionMaybe $ try $ simpleStatement <* semi
    expr <- optionMaybe expression
    clauses <- braces' (many exprCaseClause) 
    return $ Switch stmt expr clauses

  exprCaseClause :: Parser (SwitchClause ())
  exprCaseClause = try $ do
    caseType <- (lexeme "case" >> Just <$> expressionList) <|> (lexeme "default" >> return Nothing)
    lexeme' ":"
    stmts <- statement `sepEndBy` semi'
    return $ (caseType, stmts)

  forStmt :: Parser (Statement ())
  forStmt = do
    reserved "for"
    cond <- (optionMaybe $ forClause 
        <|> (try $ Condition <$> expression) 
        <|> (try $ Condition <$> between (semi) (semi) expression))
        <|> ((try $ semi >> semi) >> (return $ Nothing)) 

    body <- blockStmt
    return $ For cond body

  forClause :: Parser (ForCond ())
  forClause = try $ do
    initStmt <- simpleStatement <|> (lineSpace >> lookAhead (try $ char ';') >> return Empty)
    semi
    cond <- optionMaybe expression
    semi
    postStmt <- incDec <|> assign <|> opAssign <|> exprStmt <|> (lineSpace >> return Empty)
    return $ ForClause initStmt cond postStmt

  blockStmt :: Parser (Statement ())
  blockStmt = Block <$> (braces' $ many statement)

  simpleStatement :: Parser (Statement ())
  simpleStatement = try $ incDec <|> assign <|> shortDec <|> opAssign <|>  exprStmt
  
  exprStmt :: Parser (Statement ())
  exprStmt = try $ ExpressionStmt <$> expression

  incDec :: Parser (Statement ())
  incDec = try $ do
    e <- expression
    (lexeme "++" *> (return $ Inc e)) <|> (lexeme "--" *> (return $ Dec e))

  assign :: Parser (Statement ())
  assign = try $ do
    lhs <- expressionList
    lexeme "=" -- wrong
    rhs <- expressionList
    return $ Assignment Eq lhs rhs

  opAssign :: Parser (Statement ())
  opAssign = try $ do
    lhs <- expression
    op <- (addOpParser <|> mulOpParser) <* lexeme "=" -- wrong
    rhs <- expression
    return $ Assignment op [lhs] [rhs]

  shortDec :: Parser (Statement ())
  shortDec = try $ do
    idents <- identifierList
    lexeme ":="
    exprs  <- expressionList
    return $ ShortDecl idents exprs

