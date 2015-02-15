{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}
module MGC.Parser.TypeSpec (spec) where
  import NeatInterpolation
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
    describe "typeParser" $ do
      describe "struct" $ do
        it "parses empty structs" $ do
          typeParser `parses` "struct{}" ~> (Struct [])

        it "parses complex structs" $ do
          let test = [string| 
            struct {
              x, y int
              u float32
              _ float32
              A []int
              F func()
            }
          |]
          let expected = Struct [NamedField ["x","y"] TInteger Nothing,NamedField ["u"] (TypeName "float32") Nothing,NamedField ["_"] (TypeName "float32") Nothing,NamedField ["A"] (Slice TInteger) Nothing,NamedField ["F"] (Function (Signature [] [])) Nothing]
          typeParser `parses` test ~> expected