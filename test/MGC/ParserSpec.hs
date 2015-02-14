module MGC.ParserSpec (spec) where
  import MGC.Expectation
  import MGC.Parser
  import MGC.Syntax
  import Test.Hspec
  import Control.Applicative ((<|>))

  spec :: Spec
  spec = do
    describe "topLevelDeclaration" $ do
      it "parses Declarations" $ do 
          pending
      it "parses functions" $ do
        pending

    describe "typeDec" $ do
      it "parses single types" $ do
        typeDec `parses` "type IntArray [16]int" ~> TypeDecl [("IntArray", Array (Integer 16) (TInteger))]
      it "parses multi-type delcarations" $ do
        pending
      it "doesnt require a semi colon at the end" $ do
        pending

    describe "block statement" $ do
      it "allows semi colons" $ do
        blockStmt `parses` "{\n0++;1++\n2++}\n" ~> (Block [(Inc $ Integer 0), (Inc $ Integer 1), (Inc $ Integer 2)])
      it "allows statements" $ do
        blockStmt `parses` "{0++}" ~> (Block [(Inc $ Integer 0)])

    describe "if statement" $ do
      it "works with one branch" $ do
        ifStmt `parses` "if 0 {1++}" ~> (If (Nothing) (Integer 0) (Block [Inc (Integer 1)]) Empty)
      it "works with simpleStatements" $ do
        ifStmt `parses` "if 0; 0 {1}" ~> (If (Just (ExpressionStmt (Integer 0))) (Integer 0) (Block [ExpressionStmt (Integer 1)]) Empty)
      it "works with else branch" $ do
        ifStmt `parses` "if 0 {1}else{0}" ~> (If Nothing (Integer 0) (Block [(ExpressionStmt (Integer 1))]) (Block [(ExpressionStmt (Integer 0))]))
      it "works with else if branch" $ do
        pending

    describe "return" $ do
      it "returns" $ do
        returnStmt `parses` "return" ~> (Return [])
      it "returns w expressions" $ do
        returnStmt `parses` "return 0" ~> (Return [Integer 0])
      it "returns w semicolons" $ do
        returnStmt `parses` "return;" ~> (Return [])
      it "eats new lines" $ do
        returnStmt `parses` "return\n" ~> (Return [])

    describe "for" $ do
      it "works with infinite loop" $ do
        forStmt `parses` "for {}" ~> (For Nothing (Block []))
      it "works with standard for clauses" $ do
        forStmt `parses` "for x = 0; x<10; x++ {}" ~> (For (Just $ ForClause (Assignment Eq [(Name "x")] [(Integer 0)]) (BinaryOp LessThan (Name "x") (Integer 10)) (Inc $ Name "x")) (Block []))
      it "works with expression loops" $ do
        forStmt `parses` "for x<y {}" ~> (For (Just $ Condition (BinaryOp LessThan (Name "x") (Name "y"))) (Block []))
        

    describe "shortDec" $ do
      it "parses single lhs & rhs" $ do
        pending
      it "parses empty idents" $ do
        pending
      it "parses complex lhs & rhs" $ do
        pending

    describe "switch" $ do
      it "parses expression switches" $ do
        switchStmt `parses` "switch 0 {\ndefault:\n1}" ~> (Switch Nothing (Just (Integer 0)) [(Nothing, [ExpressionStmt (Integer 1)])])
      it "parses empty switches" $ do
        switchStmt `parses` "switch {}" ~> (Switch Nothing Nothing [])
      it "parses statement switches" $ do
        switchStmt `parses` "switch 0; {}" ~> (Switch (Just (ExpressionStmt (Integer 0))) Nothing [])
      it "parses expression & statement switches" $ do
        switchStmt `parses` "switch 0; 0 {\n}" ~> (Switch (Just (ExpressionStmt (Integer 0))) (Just (Integer 0)) [])

    describe "function declarations" $ do
      it "parses function signatures" $ do
        pending
      it "parses full functions" $ do
        pending

    describe "simpleStatement" $ do
      it "parses expressions" $ do
        simpleStatement `parses` "0" ~> ExpressionStmt (Integer 0)
      it "parses increments" $ do
        simpleStatement `parses` "(0[2])++" ~> (Inc $ Index (Integer 0) (Integer 2))
      it "parses decrements" $ do
        simpleStatement `parses` "(0[2])--" ~> (Dec $ Index (Integer 0) (Integer 2))
      it "parses assignment" $ do
        simpleStatement `parses` "test = 5" ~> (Assignment Eq [(Name "test")] [(Integer 5)])
      it "parses short declarations" $ do
        shortDec `parses` "test := 5" ~> (ShortDecl [("test")] [(Integer 5)])

    describe "incDec" $ do
      it "parses 0++" $ do
        incDec `parses` "0++" ~> (Inc $ Integer 0)
      it "parses (0[1])--" $ do
        incDec `parses` "(0[0])--" ~> (Dec $ Index (Integer 0) (Integer 0))

    describe "assign" $ do
      it "parses single var assignment" $ do
        assign `parses` "test = 5" ~> (Assignment Eq [(Name "test")] [(Integer 5)]) 
      it "parses multi var assignment" $ do
        assign `parses` "test, x, y = 5" ~> (Assignment Eq [(Name "test"), (Name "x"), (Name "y")] [(Integer 5)])
      it "op assignments only allow one element on each side" $ do
        (assign <|> opAssign) `wontParse` "test, x *= 5"  
      it "assignments allow blank idents but op ones dont" $ do
        assign `parses` "_ = 5" ~> (Assignment Eq [(Name "_")] [(Integer 5)])
        opAssign `wontParse` "_ = 5"
