module MGC.Parser.TypeSpec (spec) where
  import MGC.Expectation
  import MGC.Parser.Type
  import MGC.Syntax
  import Test.Hspec
  import Control.Applicative ((<|>))

  spec :: Spec
  spec = do 
    describe "typeName" $ do
      it "parses strings" $ do
        typeName `parses` "test" ~> TypeName "test"

    describe "typeLit" $ do
      it "parses arrays" $ do
        typeLit `parses` "[1]int" ~> (Array (Integer 1) (TInteger))
    describe "typeParser" $ do
      it "parses literals" $ do
        pending