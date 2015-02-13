module MGC.Parser.PrimSpec (spec) where
  import MGC.Expectation
  import Test.Hspec
  import MGC.Parser.Prim
  import Text.Parsec
  import MGC.Syntax
  spec :: Spec
  spec = do
    describe "literal" $ do
      describe "int" $ do 
        it "parses octal 0" $ do
          intLit `parses` "00" `to` (Integer 0)
        it "parses octal 17" $ do
          intLit `parses` "021" `to` (Integer 17)
        it "parses hex 0x0" $ do
          intLit `parses` "0x0" `to` (Integer 0)
        it "parses hex 0X0" $ do
          intLit `parses` "0X0" `to` (Integer 0)
        it "parses hex 0X15" $ do
          intLit `parses` "0X15" `to` (Integer 21)
        it "parses decimal 0" $ do
          intLit `parses` "0" `to` (Integer 0)
    describe "identifier" $ do
      it "recognizes strings" $ do
        identifier `parses` "akadjfkadjfkl " `to` ("akadjfkadjfkl")
      it "accepts _ as a leading char" $ do
        identifier `parses` "_test " `to` ("_test")
      it "doesnt allow reserved words" $ do
        identifier `wontParse` "var"