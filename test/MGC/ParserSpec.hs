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
        pending

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
