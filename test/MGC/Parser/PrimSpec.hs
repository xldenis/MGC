module MGC.Parser.PrimSpec (spec) where
  import MGC.Expectation
  import Test.Hspec
  import MGC.Parser.Prim
  import Text.Parsec
  import MGC.Syntax
  import Control.Applicative ((<*))
  spec :: Spec
  spec = do
    describe "literal" $ do
      describe "int" $ do 
        it "parses octal 0" $ do
          intLit `parses` "00" ~> (Integer 0)
        it "parses octal 17" $ do
          intLit `parses` "021" ~> (Integer 17)
        it "parses hex 0x0" $ do
          intLit `parses` "0x0" ~> (Integer 0)
        it "parses hex 0X0" $ do
          intLit `parses` "0X0" ~> (Integer 0)
        it "parses hex 0X15" $ do
          intLit `parses` "0X15" ~> (Integer 21)
        it "parses decimal 0" $ do
          intLit `parses` "0" ~> (Integer 0)
        it "eats whitespace" $ do
          (intLit <* eof) `parses` "0 " ~> (Integer 0)
      describe "string" $ do
        it "parses interpreted strings" $ do
          pending
        it "parses literal strings" $ do
          stringLit `parses` "`test`" ~> (RawString "test")
        it "parses unicode literals" $ do
          stringLit `parses` "\"\\u0000\"" ~> (IntString "\\u0000")
        it "parses escape sequences" $ do
          stringLit `parses` "\" \\n \\r \\t \\a \\' \\\" \"" ~> (IntString " \\n \\r \\t \\a \\' \\\" ")
      describe "runes" $ do
        it "recognizes normal chars" $ do
          runeLit `parses` "'a'" ~> (Rune "a")
        it "recognizes octal bytes" $ do
          runeLit `parses` "'\\000'" ~> (Rune "\\000")
        it "recognizes hex bytes" $ do
          runeLit `parses` "'\\x00'" ~> (Rune "\\x00")

    describe "identifier" $ do
      it "recognizes strings" $ do
        identifier `parses` "akadjfkadjfkl" ~> ("akadjfkadjfkl")
      it "accepts _ as a leading char" $ do
        identifier `parses` "_test" ~> ("_test")
      it "doesnt allow reserved words" $ do
        identifier `wontParse` "var"