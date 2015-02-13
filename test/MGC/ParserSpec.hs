module MGC.ParserSpec (spec) where
  import MGC.Expectation
  import MGC.Parser
  import MGC.Syntax
  import Test.Hspec
  import Text.Parsec

  spec :: Spec
  spec = do
    describe "topLevelDeclaration" $ do
      it "parses Declarations" $ do 
          pending
      it "parses functions" $ do
        pending
    describe "typeDec" $ do
      it "parses single types" $ do
        pending
      it "parses multi-type delcarations" $ do
        pending
      it "doesnt require a semi colon at the end" $ do
        pending

    describe "simpleStatement" $ do
      it "parses expressions" $ do
        pending
      it "parses increments" $ do
        pending
      it "parses decrements" $ do
        pending
      it "parses assignment" $ do
        pending
      it "parses short declarations" $ do
        pending

    describe "incDec" $ do
      it "parses ++0" $ do
        pending
        --parse incDec "" "++0" `shouldParseTo` (Inc $ Integer 0)